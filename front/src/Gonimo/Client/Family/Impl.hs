{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Account.Impl
Description : Account specific data is served by this module.
Copyright   : (c) Robert Klotzner, 2018
At the moment this is only about managing of claimed invitations.
-}
module Gonimo.Client.Account.Impl ( -- * Interface
                               module API
                               -- * Types
                             , ModelConfig(..)
                             , HasModel
                             , HasModelConfig
                               -- * Creation
                             , make
                             ) where


import qualified Data.Aeson               as Aeson
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified GHCJS.DOM                as DOM
import qualified GHCJS.DOM.History        as History
import qualified GHCJS.DOM.Location       as Location
import           GHCJS.DOM.Types          (MonadJSM, liftJSM, toJSVal)
import qualified GHCJS.DOM.Window         as Window
import           Network.HTTP.Types       (urlDecode)

import           Gonimo.Client.Account    as API
import           Gonimo.Client.Host       (Intent)
import qualified Gonimo.Client.Host       as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import qualified Gonimo.Client.Server     as Server
import qualified Gonimo.Client.Subscriber as Subscriber
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types   (InvitationInfo)
import           Gonimo.Types             (InvitationSecret)


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

-- | Create an Family.
--
make :: ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
        , HasModel model, HasConfig c, HasModelConfig mConf t
        )
     => FamilyId -> model t -> c t -> m (mConf t, Family t)
make familyId' model conf = do
  _activeInvitation <- _
  pure $ mempty & Server.onRequest .~ [ API.ReqSetFamilyName familyId' <$> conf ^. onSetName
                                      , uncurry API.ReqSetDeviceName   <$> conf ^. onSetDeviceName
                                      , API.ReqCreateInvitation familyId' <$ conf ^. onCreateInvitation
                                      , API.ReqCreateInvitationCode <$> tag (current activeInvitation) (conf ^. onCreateCode)
                                      ]
