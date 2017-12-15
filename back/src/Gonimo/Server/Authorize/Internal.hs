{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Server.Authorize.Internal
Description : Types and internal functions for "Gonimo.Server.Authorize"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Authorize.Internal where

import           Control.Lens
import           Data.Maybe
import           Reflex
import           Reflex.Host.App


import           Gonimo.Prelude
import           Gonimo.Server.Cache            (HasModel (..))
import qualified Gonimo.Server.Cache            as Cache
import           Gonimo.Server.Clients.Internal (Clients (..))
import           Gonimo.Server.Clients.ClientStatus
import           Gonimo.Server.Error
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Model
import qualified Gonimo.Server.Cache.FamilyAccounts as FamilyAccounts
import qualified Gonimo.Server.Cache.Invitations    as Invitations




data Config t
  = Config { __clients :: Clients t
           , _onAuthorize :: Event t (DeviceId, FromClient)
           }

data Authorize t
  = Authorize { _onForbidden :: Event t (DeviceId, ToClient)
              , _onAuthorized :: Event t (DeviceId, FromClient)
              }

data AuthRequest
  = AuthRequest { cache'   :: !Cache.Model
                , clients' :: !ClientStatuses
                , senderId :: !DeviceId
                , msg :: !FromClient
                }

make :: forall t m. MonadAppHost t m => Config t -> m (Authorize t)
make _ = undefined

-- | Tell what requests should be forbidden.
--
--   Returns a 'Just' 'ServerError' for requests that should not be allowed.
--
--   For simplicity we currently quite often check whether a device is online in a
--   family instead of whether the device is a mere member of the family. This
--   is not a hard requirements in all cases and can be lifted when needed.
deny :: AuthRequest -> Maybe ServerError
deny auth@AuthRequest {..} =
  case msg of
    Ping                     -> pure InternalServerError -- Should already be handled!
    MakeDevice _             -> pure InternalServerError -- Should already be handled!
    Authenticate _           -> pure InternalServerError -- Should already be handled!
    MakeFamily               -> mzero -- All devices are allowed to create families.
    MakeInvitation fid       -> denyUnless (isOnlineInFamily clients' senderId fid)
    -- If the device knows the secret it may access the invitation. Whether it is already
    -- claimed, is checked at access:
    ClaimInvitation _        -> mzero
    AnswerInvitation invId _ -> denyUnless (deviceOwnsInvitation cache' senderId invId)
    SendMessage toId _       -> denyUnless (isOnlineInSameFamily clients' senderId toId)
    UpdateServer update'     -> denyUpdate auth update'
    Get view'                -> denyView auth view'


-- | Deny not allowed updates.
denyUpdate :: AuthRequest -> Update -> Maybe ServerError
denyUpdate auth@AuthRequest {..} update' =
  case update' of
    OnChangedFamilyName         fid _       -> denyUnless (isOnlineInFamily clients' senderId fid)
    OnChangedFamilyLastAccessed _ _         -> pure Forbidden
    OnNewFamilyMember           _ _         -> pure Forbidden
    OnRemovedFamilyMember fid aid           -> denyUnless ( isOnlineInFamily clients' senderId fid
                                                            && isAccountInOurFamily auth aid
                                                          )
    OnNewFamilyInvitation       _ _         -> pure Forbidden
    OnRemovedFamilyInviation fid invId      -> denyUnless ( isOnlineInFamily clients' senderId fid
                                                            && onlineFamilyHasInvitation auth invId
                                                          )
    OnNewAccountDevice          _ _         -> pure Forbidden
    OnRemovedAccountDevice      _ _         -> pure Forbidden
    OnNewAccountInvitation      _ _         -> pure Forbidden
    OnNewAccountFamily          _ _         -> pure Forbidden
    OnChangedDeviceName         devId _     -> denyUnless ( senderId == devId
                                                            || isOnlineInSameFamily clients' senderId devId
                                                          )
    OnChangedDeviceLastAccessed _ _         -> pure Forbidden
    OnChangedDeviceStatus       devId fid _ -> denyUnless ( senderId == devId
                                                            && isFamilyMember cache' senderId fid
                                                          )
    OnClaimedInvitation         _           -> pure Forbidden
    OnChangedInvitationDelivery invId _     -> denyUnless (onlineFamilyHasInvitation auth invId)

-- | Deny not allowed views.
denyView :: AuthRequest -> ViewSelector -> Maybe ServerError
denyView auth@AuthRequest {..} view' =
  case view' of
   SelectAccountDevice devId     -> denyUnless (isSameAccount auth devId)
   SelectAccountFamily fid       -> denyUnless (isFamilyMember cache' senderId fid)
   SelectAccountInvitation invId -> denyUnless (hasClaimedInvitation cache' senderId invId)

   SelectFamilyAccount aid       -> denyUnless (isAccountInOurFamily auth aid)
   SelectFamilyDevice devId      -> denyUnless (isDeviceInOurFamily auth devId)
   SelectFamilyInvitation invId  -> denyUnless (familyHasInvitation auth invId)

-- | Return 'Forbidden' unless the first argument is true.
denyUnless :: MonadPlus m => Bool -> m ServerError
denyUnless c = guard (not c) >> pure Forbidden


-- | Check whether the sender device is member of an account which is a family
--   member of the given family.
isFamilyMember :: Cache.Model -> DeviceId -> FamilyId -> Bool
isFamilyMember cache' senderId' fid = isJust $ do
  aid <- cache'^?devices.at senderId'._Just.to deviceAccountId
  let fids = FamilyAccounts.getFamilies aid (cache'^.familyAccounts)
  guard (fid `elem` fids)

-- | Does the given device id belong to the same account as we?
isSameAccount :: AuthRequest -> DeviceId -> Bool
isSameAccount AuthRequest {..} other = isJust $ do
  otherDevice <- cache'^.devices.at other
  ourDevice <- cache'^.devices.at senderId
  guard (deviceAccountId ourDevice == deviceAccountId otherDevice)

-- | Does the sender device own the given invitation?
hasClaimedInvitation :: Cache.Model -> DeviceId -> InvitationId -> Bool
hasClaimedInvitation cache' senderId' invId = isJust $ do
  aid <- cache'^?devices.at senderId'._Just.to deviceAccountId
  let byReceiverId =  Invitations.byReceiverId (cache'^.invitations)
  invitations' <- byReceiverId^.at aid
  guard (invId `elem` invitations')

-- | Is the invitation claimed already?
isInvitationClaimed :: Cache.Model -> InvitationId -> Bool
isInvitationClaimed cache' invId = isJust
  $ cache'^?invitations.at invId._Just.to invitationReceiverId._Just

-- | Does the invitatation belong to the family we are currently online in?
familyHasInvitation :: AuthRequest -> InvitationId -> Bool
familyHasInvitation AuthRequest {..} invId = isJust $ do
  ourFamily <- clients'^? at senderId . _Just . clientFamily . _Just
  let byFamilyId' = Invitations.byFamilyId (cache'^.invitations)
  invitations' <- byFamilyId' ^. at ourFamily
  guard (invId `elem` invitations')

-- | Auth check whether a device is currently online in a given family.
isOnlineInFamily :: ClientStatuses -> DeviceId -> FamilyId -> Bool
isOnlineInFamily clients' devId' fid
  = isJust
    $ clients'^?at devId' . _Just . clientFamily . only (Just fid)

-- | Both devices in the same family online?
isOnlineInSameFamily :: ClientStatuses -> DeviceId -> DeviceId -> Bool
isOnlineInSameFamily clients' we they = isJust $ do
    ourFamily <- clients'^? at we . _Just . clientFamily . _Just
    guard (isOnlineInFamily clients' they ourFamily)

-- | Invitation is from the family the device is currently online in?
onlineFamilyHasInvitation :: AuthRequest -> InvitationId -> Bool
onlineFamilyHasInvitation AuthRequest{..} invId = isJust $ do
    ourFamily <- clients'^? at senderId . _Just . clientFamily . _Just
    let byFamilyId' = Invitations.byFamilyId (cache'^.invitations)
    invIds <- byFamilyId' ^. at ourFamily
    guard (invId `elem` invIds)

-- | The device already claimed the invitation?
deviceOwnsInvitation :: Cache.Model -> DeviceId -> InvitationId -> Bool
deviceOwnsInvitation cache' devId' invId' = isJust $ do
  ourAccount <- cache'^?devices.at devId'._Just.to deviceAccountId
  invitationAccount <- cache'^?invitations.at invId'._Just . to invitationReceiverId._Just
  guard (ourAccount == invitationAccount)

-- | Is the given account part of the family we are currently online in?
isAccountInOurFamily :: AuthRequest -> AccountId -> Bool
isAccountInOurFamily AuthRequest {..} aid = isJust $ do
  fid <- clients'^? at senderId . _Just . clientFamily . _Just
  let accounts' = FamilyAccounts.getAccounts fid (cache'^.familyAccounts)
  guard (aid `elem` accounts')

-- | Is the given device part of the family we are currently online in?
isDeviceInOurFamily :: AuthRequest -> DeviceId -> Bool
isDeviceInOurFamily auth@AuthRequest {..} devId = isJust $ do
  aid <- cache'^?devices.at devId._Just.to deviceAccountId
  guard (isAccountInOurFamily auth aid)
