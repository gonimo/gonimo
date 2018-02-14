{-|
Module      : Gonimo.Client.Account.Internal
Description : Types and internal functions for "Gonimo.Client.Account"
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Account.Internal where


import qualified Data.Map                  as Map

import           Gonimo.Client.Account.API
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server      (Server)
import qualified Gonimo.Client.Server      as Server
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types    (InvitationInfo)
import           Gonimo.Types              (InvitationSecret)



data FullConfig t
  = FullConfig { __config :: Config t
               , __server :: Server t
               , __subscriber :: Subscriber t
               }

data FullAccount t
  = FullAccount { __account     :: Account t
                -- | Commands going to the server.
                , _serverConfig :: Server.Config t
                }

makeClaimedInvitations
  :: forall c t m. (Reflex t, MonadHold t m, MonadFix m, Server.HasServer c)
  => c t -> m (Dynamic t ClaimedInvitations)
makeClaimedInvitations conf =
  let
    onClaimedInvitation :: Event t (InvitationSecret, InvitationInfo)
    onClaimedInvitation = fmapMaybe (^?_ResClaimedInvitation) $ conf ^. Server.response

    onAnsweredInvitation :: Event t InvitationSecret
    onAnsweredInvitation = fmapMaybe (^?_ResAnsweredInvitation._1) $ conf ^. Server.response
  in
    foldDyn id Map.empty $ leftmost [ uncurry Map.insert <$> onClaimedInvitation
                                    , Map.delete <$> onAnsweredInvitation
                                    ]

makeServerConfig :: (Reflex t, HasConfig c) => c t -> Server.Config t
makeServerConfig conf =
  let
    onClaim  = map ReqClaimInvitation <$> conf^.onClaimInvitation
    onAnswer = map (uncurry ReqAnswerInvitation) <$> conf^.onAnswerInvitation

  in
    def & Server.request .~ mconcat [ onClaim
                                    , onAnswer
                                    ]

instance Server.HasConfig FullAccount where
  config = serverConfig

instance Server.HasServer FullConfig where
  server = _server

instance HasConfig FullConfig where
  config = _config

instance HasAccount FullAccount where
  account = _account

-- Auto generated lenses:


-- Lenses for FullConfig t:

_config :: Lens' (FullConfig t) (Config t)
_config f fullConfig' = (\_config' -> fullConfig' { __config = _config' }) <$> f (__config fullConfig')

_server :: Lens' (FullConfig t) (Server t)
_server f fullConfig' = (\_server' -> fullConfig' { __server = _server' }) <$> f (__server fullConfig')


-- Lenses for FullAccount t:

_account :: Lens' (FullAccount t) (Account t)
_account f fullAccount' = (\_account' -> fullAccount' { __account = _account' }) <$> f (__account fullAccount')

serverConfig :: Lens' (FullAccount t) (Server.Config t)
serverConfig f fullAccount' = (\serverConfig' -> fullAccount' { _serverConfig = serverConfig' }) <$> f (_serverConfig fullAccount')


