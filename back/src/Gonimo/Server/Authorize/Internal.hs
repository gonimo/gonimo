{-|
Module      : Gonimo.Server.Authorize.Internal
Description : Types and internal functions for "Gonimo.Server.Authorize"
Copyright   : (c) Robert Klotzner, 2017
-}
module Gonimo.Server.Authorize.Internal where

import Reflex
import Reflex.Host.App
import Control.Lens


import Gonimo.Server.Error
import Gonimo.Prelude
import Gonimo.Server.Cache
import Gonimo.Clients.Internal


data Config t
  = Config { __clients :: Clients t
           , _onAuthorize :: Event t (DeviceId, FromClient)
           }

data Authorize t
  = Authorize { _onForbidden :: Event t (DeviceId, ToClient)
              , _onAuthorized :: Event t (DeviceId, FromClient)
              }

data AuthRequest
  = AuthRequest { cache'   :: !Cache.Sampled
                , clients' :: !Clients.Sampled
                , senderId :: !DeviceId
                , msg :: !FromClient
                }

make :: forall t m. MonadAppHost t m => Config t -> m (Authorize t)
make config = undefined

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
    -- If the device knows the secret it may access the invitation. If it is already
    -- claimed, is checked at access:
    ClaimInvitation secret   -> mzero
    AnswerInvitation invId _ -> denyUnless (deviceOwnsInvitation cache' senderId invId)
    SendMessage toId _       -> denyUnless (isOnlineInSameFamily clients' senderId toId)
    UpdateServer update'     -> denyUpdate auth update'
    Get view'                -> denyView auth view'


-- | Deny not allowed updates.
denyUpdate :: AuthRequest -> Update -> Maybe ServerError
denyUpdate auth@AuthRequest {..} update' =
  case udpate' of
    OnChangedFamilyName         fid name    -> denyUnless (isOnlineInFamily clients' senderId fid)
    OnChangedFamilyLastAccessed fid time'   -> pure Forbidden
    OnNewFamilyMember           fid aid     -> pure Forbidden
    OnRemovedFamilyMember       fid aid     -> denyUnless (isOnlineInFamily clients' senderId fid)
    OnNewFamilyInvitation       fid invId   -> pure Forbidden
    OnRemovedFamilyInviation fid invId      -> denyUnless ( isOnlineInFamily clients' senderId fid
                                                       && onlineFamilyHasInvitation auth invId
                                                     )
    OnNewAccountDevice          aid devId   -> pure Forbidden
    OnRemovedAccountDevice      aid devId   -> pure Forbidden
    OnNewAccountInvitation      aid invId   -> pure Forbidden
    OnNewAccountFamily          aid fid     -> pure Forbidden
    OnChangedDeviceName         devId _     -> denyUnless (senderId == devId)
    OnChangedDeviceLastAccessed devId time' -> pure Forbidden
    OnChangedDeviceStatus       devId fid _ -> denyUnless ( senderId == devId
                                                            && isFamilyMember cache' senderId fid
                                                          )
    OnClaimedInvitation         invId       -> pure Forbidden
    OnChangedInvitationDelivery invId _     -> denyUnless (onlineFamilyHasInvitation auth invId)

-- | Deny not allowed views.
denyView :: AuthRequest -> ViewSelector -> Maybe ServerError
denyView auth@AuthRequest {..} view' =
  case view' of
   SelectAccountOwn              -> mzero -- One is always allowed to retrieve it's own account data.
   SelectAccountDevice devId     -> denyUnless (isSameAccount auth devId)
   SelectAccountFamily fid       -> denyUnless (isFamilyMember cache' senderId fid)
   SelectAccountInvitation invId -> denyUnless (hasClaimedInvitation cache' senderId invId)

   SelectFamily fid              -> denyUnless (isOnlineInFamily clients' senderId fid)
   SelectFamilyAccount aid       -> denyUnless (isAccountInOurFamily auth aid)
   SelectFamilyDevice devId      -> denyUnless (isDeviceInOurFamily aut devId)
   SelectFamilyInvitation invId  -> denyUnless (familyHasInvitation auth invId)

-- | Return 'Forbidden' unless the first argument is true.
denyUnless :: MonadPlus m => Bool -> m ServerError
denyUnless c = mfilter (not c) $ pure Forbidden


-- | Check whether the sender device is member of an account which is a family
--   member of the given family.
isFamilyMember :: Cache.Sampled -> DeviceId -> FamilyId -> Bool
isFamilyMember cache' senderId' fid = maybe False (const True) $ do
  aid <- cache'^?sampledDevices.at senderId'._Just.to deviceAccountId
  fids <- cache'^.sampledAccountFamilies.at aid
  mfilter (fid `elem` fids) $ pure ()

-- | Does the given device id belong to the same account as we?
isSameAccount :: AuthRequest -> DeviceId -> Bool
isSameAccount AuthRequest {..} other = maybe False (const True) $ do
  otherDevice <- cache'^.sampledDevices.at devId
  ourDevice <- cache'^.sampledDevices.at senderId
  mfilter (devieAccountId ourDevice == deviceAccountId otherDevice) $ pure ()

-- | Does the sender device own the given invitation?
hasClaimedInvitation :: Cache.Sampled -> DeviceId -> InvitationId -> Bool
hasClaimedInvitation cache' senderId' invId = maybe False (const True) $ do
  aid <- cache'^?sampledDevices.at senderId'._Just.to deviceAccountId
  invitations <- cache'^.sampledAcountInvitations.at aid
  mfilter (invId `elem` invitations) $ pure ()

-- | Does the invitatation belong to the family we are currently online in?
familyHasInvitation :: AuthRequest -> InvitationId -> Bool
familyHasInvitation AuthRequest {..} invId = maybe False (const True) $ do
  ourFamily <- clients'^.sampledSelectedFamily . at senderId
  invitations <- cache'^.sampledFamilyInvitations.at ourFamily
  mfilter (invId `elem` invitations) $ pure ()

-- | Auth check whether a device is currently online in a given family.
isOnlineInFamily :: Clients.Sampled -> DeviceId -> FamilyId -> Bool
isOnlineInFamily clients' devId' fid
  = maybe False (const True)
    $ clients'^?sampledSelectedFamily .at devId'.only (Just fid)

-- | Both devices in the same family online?
isOnlineInSameFamily :: Clients.Sampled -> DeviceId -> DeviceId -> Bool
isOnlineInSameFamily clients' we they =
  let
    mOurFamily = clients'^.sampledSelectedFamily . at we
  in
    case mOurFamily of
      Nothing          -> False -- We are not online in any family - we are done!
      Just ourFamily -> isOnlineInFamily clients' they ourFamily

-- | Invitation is from the family the device is currently online in?
onlineFamilyHasInvitation :: AuthRequest -> InvitationId -> Bool
onlineFamilyHasInvitation AuthRequest{..} invId = maybe False (const True) $ do
    ourFamily <- clients'^.sampledSelectedFamily . at senderId
    invIds <- cache'^.sampledFamilyInvitations.at ourFamily
    mfilter (invId `elem` invIds) $ pure ()

-- | The device already claimed the invitation?
deviceOwnsInvitation :: Cache.Sampled -> DeviceId -> InvitationId -> Bool
deviceOwnsInvitation cache' devId' invId'
  = maybe False (const True)
    $ cache'^.sampledInvitations.at invId'._Just . to invitationReceiverId. only (Just devId')

-- | Is the given account part of the family we are currently online in?
isAccountInOurFamily :: AuthRequest -> AccountId -> Bool
isAccountInOurFamily AuthRequest {..} aid = maybe False (const True) $ do
  fid <- clients'^.sampledSelectedFamily . at senderId
  accounts <- cache'^.sampledFamilyAccounts.at fid
  mfilter (aid `elem` accounts) $ pure ()

-- | Is the given device part of the family we are currently online in?
isDeviceInOurFamily :: AuthRequest -> DeviceId -> Bool
isDeviceInOurFamily auth@AuthRequest {..} devId = maybe False (const True) $ do
  aid <- cache'^?sampledDevices.at devId._Just.to deviceAccountId
  mfilter (isAccountInOurFamily auth aid) $ pure ()
