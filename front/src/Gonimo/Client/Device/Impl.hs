{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Device.Impl
Description : Entry point to data from the server.
Copyright   : (c) Robert Klotzner, 2018
A device can create families, retrieve family data, can access it's account, ....
-}
module Gonimo.Client.Device.Impl ( -- * Interface
                               module API
                               -- * Types
                             , ModelConfig(..)
                             , HasModel
                             , HasModelConfig
                               -- * Creation
                             , make
                             ) where











import           Control.Arrow             (second)
import           GHCJS.DOM.Types           (MonadJSM)

import           Gonimo.Client.Server      (onResponse)
import           Reflex.Network
import           Reflex.NotReady.Class



import           Gonimo.Client.Device
import           Gonimo.Client.Family      (Family)
import qualified Gonimo.Client.Family.Impl as Family
import qualified Gonimo.Client.Host        as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude

import qualified Gonimo.Client.Auth        as Auth
import qualified Gonimo.Client.Server      as Server
import qualified Gonimo.Client.Subscriber  as Subscriber
import           Gonimo.SocketAPI          as API
import           Gonimo.SocketAPI.Types    (InvitationInfo, InvitationSecret)
import qualified Gonimo.SocketAPI.Types    as API



-- | Our dependencies
type HasModel model = (Server.HasServer model, Host.HasHost model, Auth.HasAuth model
                      , Family.HasModel model
                      )

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


-- | Create a Device.
--
make
  :: forall t m model mConf c
     . ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m, NotReady t m, Adjustable t m, PostBuild t m
       , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
       , HasModel model, HasConfig c, HasModelConfig mConf t
       )
     => model t -> c t -> m (mConf t, Device t)
make model conf = do
  let
    _identifier = fmap API.deviceId <$> model ^. Auth.authData
  (mConf, _selectedFamily) <- makeFamily model conf
  pure (mConf, Device {..})

makeFamily
  :: forall t m model mConf c
     . ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m, NotReady t m, Adjustable t m, PostBuild t m
       , PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)
       , HasModel model, HasConfig c, HasModelConfig mConf t
       )
     => model t -> c t -> m (mConf t, MDynamic t (Family t))
makeFamily model conf = do
    let
      onSelectedFamily :: Event t API.FamilyId
      onSelectedFamily = fmapMaybe (^? API._ResSwitchedFamily . _2) $ model ^. onResponse

      onRequestInvitationIds = ReqGetFamilyInvitations <$> onSelectedFamily

      makeJustFamily :: API.FamilyId -> m (mConf t, Maybe (Family t))
      makeJustFamily = fmap (second Just) . Family.make model (conf ^. familyConfig)

      onFamily =  makeJustFamily <$> onSelectedFamily

    -- TODO: Missing: We have to clear the selection when the family list gets
    -- empty and we have to update it if our current id gets deleted and we have to initialize it from storage:
    initEv <- networkView =<< holdDyn (pure (mempty, Nothing)) onFamily

    famMConf <- flatten $ fst <$> initEv
    subscriberConf <- Subscriber.fromServerRequests $ (:[]) <$> onRequestInvitationIds
    dynFamily <- holdDyn Nothing $ snd <$> initEv

    pure ( famMConf <> subscriberConf
         , dynFamily
         )
