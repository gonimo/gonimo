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











import           GHCJS.DOM.Types          (MonadJSM, liftJSM, toJSVal)



import           Gonimo.Client.Device

import qualified Gonimo.Client.Host       as Host
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import qualified Gonimo.Client.Server     as Server
import qualified Gonimo.Client.Subscriber as Subscriber
import           Gonimo.SocketAPI         as API
import qualified Gonimo.SocketAPI.Types   as API
import           Gonimo.SocketAPI.Types   (InvitationInfo, InvitationSecret)
import           Gonimo.Client.Family (Family)
import           Gonimo.Client.Reflex (buildMap)



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

-- | Create an Device.
--
--   You can claim invitations and answer them.
--   At the moment we don't yet have a means for getting claimed invitations
--   from the server (ReqGetClaimedInvitations), so we only provide the ones of
--   the current session. If you restart gonimo, your claimed invitations are no
--   longer visible.
make :: ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
        , HasModel model, HasConfig c, HasModelConfig mConf t
        )
     => model t -> c t -> m (mConf t, Device t)
make model conf' = undefined

