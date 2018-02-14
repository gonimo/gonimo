{-|
Module      : Gonimo.Client.Account.Internal
Description : Types and internal functions for "Gonimo.Client.Account"
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Account.Internal where


import qualified Data.Map                  as Map
import qualified Data.Set                  as Set


import           Gonimo.Client.Account.API
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server      (Server)
import qualified Gonimo.Client.Server      as Server
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types    (InvitationInfo)
import           Gonimo.Types              (InvitationSecret)
import qualified Gonimo.Client.Subscriber.API as Subscriber





data FullConfig t
  = FullConfig { __config :: Config t
               , __server :: Server t
               , __subscriber :: Subscriber t
               }

data FullAccount t
  = FullAccount { __account     :: Account t

                  -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                , _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig :: Subscriber.Config t
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

answerInvitations :: (Reflex t, HasConfig c) => c t -> Server.Config t
answerInvitations conf =
  let
    onAnswer = map (uncurry ReqAnswerInvitation) <$> conf^.onAnswerInvitation
  in
    mempty & Server.request .~ onAnswer

subscribeInvitationClaims :: (Reflex t, HasConfig c) => c t -> Subscriber.Config t
subscribeInvitationClaims conf = do
    invitationsToClaim <- foldDyn id Set.empty
                          $ mergeWith (.) [ Set.insert <$> conf ^. onClaimInvitation
                                          , Set.delete . fst <$> conf ^. onAnswerInvitation
                                          ]

    pure $ mempty & configSubscriptions .~ makeSubscriptions invitationsToClaim

  where
    makeSubscriptions :: Set InvitationSecret -> Set ServerRequest
    makeSubscriptions = Set.fromList . map ReqClaimInvitation . Set.toList 



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


