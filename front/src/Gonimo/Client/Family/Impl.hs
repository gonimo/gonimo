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


import           Control.Arrow            (second)
import           Control.Monad.Fix        (mfix)
import qualified Data.Aeson               as Aeson
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time                (NominalDiffTime)
import qualified GHCJS.DOM                as DOM
import qualified GHCJS.DOM.History        as History
import qualified GHCJS.DOM.Location       as Location
import           GHCJS.DOM.Types          (MonadJSM, liftJSM, toJSVal)
import qualified GHCJS.DOM.Window         as Window
import           Network.HTTP.Types       (urlDecode)
import           Reflex.Network
import           Reflex.NotReady.Class

import           Gonimo.Client.Family
import           Gonimo.Client.Host       (Intent)
import qualified Gonimo.Client.Host       as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex     (MDynamic, buildBufferedMap)
import           Gonimo.Client.Server     (onRequest, onResponse)
import qualified Gonimo.Client.Server     as Server
import qualified Gonimo.Client.Subscriber as Subscriber
import           Gonimo.SocketAPI         as API
import           Gonimo.SocketAPI.Types   (InvitationInfo, InvitationSecret)
import qualified Gonimo.SocketAPI.Types   as API


-- | Our dependencies
type HasModel model = (Server.HasServer model, Host.HasHost model)

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



-- | Create a Dynamic Family, representing the currently selected family.
--
--   Clients always operate on one particular family they have selected. They
--   are considered online in this family and can connect to baby stations in
--   this family or become baby stations themselves.
--
--   If the returned `Dynamic` contains `Nothing` this either means that no
--   `FamilyId` was selected yet or that the `FamilyId`s of this account have
--   not yet been loaded. You can distinguish the two cases by checking the
--   `Account.famililies` `Dynamic`. If it is `Nothing` then families are not
--   loaded yet. The difference is not only for display purposes relevant, but
--   also if the families are not yet loaded, no action is required (just wait
--   for it to be loaded), if they are loaded but this is still `Nothing` you
--   might want to perform some action so some `Family` is selected.
--
make
  :: forall t m model mConf c
     . ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m, NotReady t m, Adjustable t m, PostBuild t m
       , HasModel model, HasConfig c, HasModelConfig mConf t
       )
     => model t -> c t -> m (mConf t, MDynamic t (Family t))
make model conf = do
    let
      onSelectedFamily :: Event t API.FamilyId
      onSelectedFamily = fmapMaybe (^? API._ResSwitchedFamily . _2) $ model ^. onResponse

      onRequestInvitationIds = ReqGetFamilyInvitations <$> onSelectedFamily

      makeJustFamily :: API.FamilyId -> m (mConf t, Maybe (Family t))
      makeJustFamily = fmap (second Just) . makeFamily model conf

      onFamily =  makeJustFamily <$> onSelectedFamily

    -- TODO: Missing: We have to clear the selection when the family list gets
    -- empty and we have to update it if our current id gets deleted:
    initEv <- networkView =<< holdDyn (pure (mempty, Nothing)) onFamily

    mConf <- flatten $ fst <$> initEv
    dynFamily <- holdDyn Nothing $ snd <$> initEv

    pure (mConf, dynFamily)


-- | Create a Family based on a FamilyId.
makeFamily :: forall t m model mConf c
              . ( Reflex t, MonadHold t m, MonadFix m
                , HasModel model, HasConfig c, HasModelConfig mConf t
                )
           => model t -> c t -> API.FamilyId -> m (mConf t, Family t)
makeFamily model conf selectedFamilyId = mfix $ \(mConf, family') -> do
    ourSubscriptions <- holdDyn Set.empty . fmap Set.fromList $ onRequestInvitations

    invitationIds <- holdDyn [] onGotInvitationIds
    _openInvitations <- buildBufferedMap invitationIds onGotInvitation

    _activeInvitationCode <- getInvitationCode model family'

    let
      _activeInvitation = getActiveInvitation <$> _openInvitations

      mConf = mempty & Server.onRequest .~ sendServerRequests conf family'
                     & Subscriber.subscriptions .~ ourSubscriptions

    pure (mConf, Family {..})
  where
    onGotInvitationIds :: Event t [API.InvitationId]
    onGotInvitationIds = filterSelfFixed selectedFamilyId
                        . fmapMaybe (^? API._ResGotFamilyInvitations)
                        $ model ^. onResponse

    onGotInvitation :: Event t (API.InvitationId, API.Invitation)
    onGotInvitation = fmapMaybe (^? API._ResGotInvitation) $ model ^. onResponse

    onRequestInvitations = map ReqGetInvitation <$> onGotInvitationIds

-- | Handle requests in conf that result in server requests.
sendServerRequests :: forall t m mConf c
                     . ( Reflex t , HasConfig c, HasModelConfig mConf t )
                  => c t -> Family t -> mConf t
sendServerRequests conf family' =
  let
    fid = family' ^. identifier

    currentInv :: Behavior t (Maybe API.InvitationId)
    currentInv = current $ family' ^. activeInvitation

    onReqSetName = API.ReqSetFamilyName fid <$> conf ^. onSetName

    onReqCreateInvitation = fmap API.ReqCreateInvitation fid
                            . fmapMaybe id
                            . tag currentInv
                            $ conf ^. onCreateInvitation

    onReqCreateCode = fmapMaybe id
                      . tag (fmap API.ReqCreateInvitationCode <$> currentInv)
                      $ conf ^. onCreateCode

    onSetDeviceName = uncurry API.ReqSetDeviceName <$> conf ^. onSetDeviceName

  in
    mempty & onRequest .~ mergeAsList [ onReqSetName
                                      , onReqCreateInvitation
                                      , onReqCreateCode
                                      , onSetDeviceName
                                      ]

-- | Get the current invitation code from the server.
--
--   Also takes care of invalidating it after approximately `codeValidTimeout`.
getInvitationCode :: forall t m model
                     . ( Reflex t, MonadHold t m, MonadFix m
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
    filterOurCode = push (\(invId, invCode) -> runMaybeT $ do
                                cInv <- sample $ current (family ^. activeInvitation)
                                guard $  invId == cInv
                                pure invCode
                            )
    onOurCode = filterOurCode
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

