{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Family.Impl
Description : Family specific data is served by this module.
Copyright   : (c) Robert Klotzner, 2018
At the moment this is only about managing of claimed invitations.
-}
module Gonimo.Client.Family.Impl ( -- * Interface
                               module API
                               -- * Types
                             , ModelConfig(..)
                             , HasModel
                             , HasModelConfig
                               -- * Creation
                             , make
                             ) where


import           Control.Monad.Fix        (mfix)
import           Data.Foldable            (maximumBy)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           Data.Time                (NominalDiffTime)

import qualified Gonimo.Client.Device     as Device
import           Gonimo.Client.Family
import qualified Gonimo.Client.Host       as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex     (MDynamic, buildBufferedMap)
import           Gonimo.Client.Server     (onResponse)
import qualified Gonimo.Client.Server     as Server
import qualified Gonimo.Client.Subscriber as Subscriber
import           Gonimo.SocketAPI         as API
import           Gonimo.SocketAPI.Types   (codeValidTimeout)
import qualified Gonimo.SocketAPI.Types   as API


-- | Our dependencies
type HasModel model = (Server.HasServer model, Host.HasHost model, Device.HasDevice model)

-- | Example datatype fulfilling 'HasModelConfig'.
data ModelConfig t
  = ModelConfig { -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                  _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig     :: Server.Config t
                }

-- | Configurations we provide for the model as inputs.
type HasModelConfig c t = (IsConfig c t, Subscriber.HasConfig c, Server.HasConfig c)





-- | Create a Family based on a FamilyId.
--
--   Clients always operate on one particular family they have selected. They
--   are considered online in this family and can connect to baby stations in
--   this family or become baby stations themselves.
--
make :: forall t m model mConf c
              . ( Reflex t, MonadHold t m, MonadFix m, PerformEvent t m
                , TriggerEvent t m, MonadIO (Performable m)
                , HasModel model, HasConfig c, HasModelConfig mConf t
                )
           => model t -> c t -> API.FamilyId -> m (mConf t, Family t)
make model conf _identifier = mfix $ \ ~(_, family') -> do
    subscriberConf <- Subscriber.fromServerRequests onRequestInvitations

    invitationIds <- holdDyn [] onGotInvitationIds
    _openInvitations <- buildBufferedMap invitationIds onGotInvitation

    _activeInvitationCode <- getInvitationCode model family'

    let
      _activeInvitation = getActiveInvitation (model ^. Device.identifier) _openInvitations


    pure ( subscriberConf & Server.onRequest .~ sendServerRequests conf family'
         , Family {..}
         )
  where
    onGotInvitationIds :: Event t [API.InvitationId]
    onGotInvitationIds = filterSelfFixed _identifier
                        . fmapMaybe (^? API._ResGotFamilyInvitations)
                        $ model ^. onResponse

    onGotInvitation :: Event t (API.InvitationId, API.Invitation)
    onGotInvitation = fmapMaybe (^? API._ResGotInvitation) $ model ^. onResponse

    onRequestInvitations = map ReqGetInvitation <$> onGotInvitationIds


-- | Get the currently active invitation.
--
--   For details see `getActiveInvitationPure`. We use `joinDynThroughMap` from
--   "Reflex.Dynamic" in order to get rid of the inner `Dynamic`s of the `Map`.
getActiveInvitation :: forall t .  Reflex t
                    => MDynamic t API.DeviceId -> MDynamic t (Invitations t)
                    -> MDynamic t API.InvitationId
getActiveInvitation mDevId mInvitations =
    liftA2 getActiveInvitationPure mDevId joinedMaybeMap
  where
    -- Ignore the `Maybe` for a sec, so we can apply `joinDynThroughMap`
    joinedJustMap = joinDynThroughMap $ fromMaybe Map.empty <$> mInvitations

    -- Restore the `Maybe`
    joinedMaybeMap :: MDynamic t (Map API.InvitationId API.Invitation)
    joinedMaybeMap = do
      mInvitations' <- mInvitations
      if isNothing mInvitations'
        then pure Nothing
        else Just <$> joinedJustMap


-- | Get the currently active invitation.
--
--   The currently active invitation is the youngest invitation in existence for
--   this family that was issued by this device.
getActiveInvitationPure
  :: Maybe API.DeviceId -> Maybe (Map API.InvitationId API.Invitation) -> Maybe API.InvitationId
getActiveInvitationPure mDevId mInvitations = do
  devId <- mDevId
  invitations <- mInvitations

  let
    ourInvitations = Map.toList $ Map.filter ((== devId) . API.invitationSenderId) invitations
    lastInvitation = maximumBy compareByCreated ourInvitations
    compareByCreated a b = compare (API.invitationCreated . snd $ a) (API.invitationCreated . snd $ b)
  -- maximumBy is partial:
  guard $ (not . null) ourInvitations
  pure $ fst lastInvitation

-- | Handle requests in conf that result in server requests.
sendServerRequests :: forall t c
                     . ( Reflex t , HasConfig c )
                  => c t -> Family t -> Event t [API.ServerRequest]
sendServerRequests conf family' =
  let
    fid = family' ^. identifier

    currentInv :: Behavior t (Maybe API.InvitationId)
    currentInv = current $ family' ^. activeInvitation

    onReqSetName = API.ReqSetFamilyName fid <$> conf ^. onSetName

    onReqCreateInvitation = API.ReqCreateInvitation fid <$ conf ^. onCreateInvitation

    onReqCreateCode = fmapMaybe id
                      . tag (fmap API.ReqCreateInvitationCode <$> currentInv)
                      $ conf ^. onCreateCode

    onReqSetDeviceName = uncurry API.ReqSetDeviceName <$> conf ^. onSetDeviceName

  in
    mergeAsList [ onReqSetName
                , onReqCreateInvitation
                , onReqCreateCode
                , onReqSetDeviceName
                ]

-- | Get the current invitation code from the server.
--
--   Also takes care of invalidating it after approximately `codeValidTimeout`.
getInvitationCode :: forall t m model
                     . ( Reflex t, MonadHold t m, MonadFix m, PerformEvent t m
                       , TriggerEvent t m, MonadIO (Performable m)
                       , HasModel model
                       )
                  => model t -> Family t -> m (MDynamic t API.InvitationCode)
getInvitationCode model family' = do
  let
    -- We Assume server/client roundtrip does not take longer than five seconds.
    flightTime :: Int
    flightTime = 5

    timeout :: NominalDiffTime
    timeout = fromIntegral $ codeValidTimeout - flightTime

    -- Make sure the code belongs to our current invitation.
    filterOurCode :: (API.InvitationId, API.InvitationCode) -> PushM t (Maybe API.InvitationCode)
    filterOurCode (invId, invCode) = runMaybeT $ do
      cInv <- MaybeT . sample . current $ family' ^. activeInvitation
      guard $  invId == cInv
      pure invCode

    onOurCode = push filterOurCode
                . fmapMaybe (^? API._ResCreatedInvitationCode)
                $ model ^. onResponse
    -- Time up:
  onInvalidCode <- delay timeout onOurCode

  -- Put together:
  holdDyn Nothing $ leftmost [ Just    <$> onOurCode
                             , Nothing <$  onInvalidCode
                             ]


-- -- | Same as `filterOurSelf` but takes into account that we might not even exist.
-- filterMaybeSelf :: (Reflex t, Eq someId) => Behavior t (Maybe someId) -> Event t (someId, someVal) -> Event t someVal
-- filterMaybeSelf mayId = filterOurSelf mayId . fmap (first Just)

-- -- | Filter an event with someid.
-- --
-- --   Only pass the event through if it matches our id, which is passed via a
-- --   `Behavior`.
-- filterOurSelf :: (Reflex t, Eq someId) => Behavior t someId -> Event t (someId, someVal) -> Event t someVal
-- filterOurSelf ourId = push (\(evId, evVal) -> do
--                               self <- sample ourId
--                               if self == evId
--                                 then pure $ Just evVal
--                                 else pure Nothing
--                            )

-- | same as filterOurSelf, but with a fixed id.
filterSelfFixed :: (Reflex t, Eq someId) => someId -> Event t (someId, someVal) -> Event t someVal
filterSelfFixed someId = fmap snd . ffilter ((== someId) . fst)

