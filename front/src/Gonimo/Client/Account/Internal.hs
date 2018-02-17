{-|
Module      : Gonimo.Client.Account.Internal
Description : Types and internal functions for "Gonimo.Client.Account"
Copyright   : (c) Robert Klotzner, 2018
-}
module Gonimo.Client.Account.Internal where


import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Gonimo.Client.Account.API
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server         (Server)
import qualified Gonimo.Client.Server         as Server
import qualified Gonimo.Client.Subscriber.API as Subscriber
import           Gonimo.SocketAPI
import           Gonimo.SocketAPI.Types       (InvitationInfo)
import           Gonimo.Types                 (InvitationSecret)
import           Gonimo.Client.Model


data Model t
  = Model { __server :: Server t
          }

type HasModel d = Server.HasServer d

data ModelConfig t
  = ModelConfig { -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                  _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig     :: Server.Config t
                }

type HasModelConfig c t = (IsConfig c t, Subscriber.HasConfig c, Server.HasConfig c)

makeClaimedInvitations
  :: forall d t m. (Reflex t, MonadHold t m, MonadFix m, HasModel d)
  => d t -> m (Dynamic t ClaimedInvitations)
makeClaimedInvitations model =
  let
    onClaimedInvitation :: Event t (InvitationSecret, InvitationInfo)
    onClaimedInvitation = fmapMaybe (^?_ResClaimedInvitation) $ model ^. Server.onResponse

    onAnsweredInvitation :: Event t InvitationSecret
    onAnsweredInvitation = fmapMaybe (^?_ResAnsweredInvitation._1) $ model ^. Server.onResponse

    onError :: Event t InvitationSecret
    onError = fmapMaybe (^?_ResError . _1 . _ReqAnswerInvitation . _1) $ model ^. Server.onResponse
  in
    foldDyn id Map.empty $ leftmost [ uncurry Map.insert <$> onClaimedInvitation
                                    , Map.delete <$> onAnsweredInvitation
                                    , Map.delete <$> onError
                                    ]

answerInvitations :: (Reflex t, HasConfig c, IsConfig mconf t, Server.HasConfig mconf)
  => c t -> mconf t
answerInvitations conf =
  let
    onAnswer = map (uncurry ReqAnswerInvitation) <$> conf^.onAnswerInvitation
  in
    mempty & Server.onRequest .~ onAnswer

subscribeInvitationClaims :: (Reflex t, HasConfig c, MonadHold t m, MonadFix m
                             , IsConfig mconf t, Subscriber.HasConfig mconf)
                          => c t -> m (mconf t)
subscribeInvitationClaims conf = do
    invitationsToClaim <- foldDyn id Set.empty
                          $ mergeWith (.) [ doAll Set.insert <$> conf ^. onClaimInvitation
                                          , doAll (Set.delete . fst) <$> conf ^. onAnswerInvitation
                                          ]

    pure $ mempty & Subscriber.subscriptions .~ fmap makeSubscriptions invitationsToClaim

  where
    makeSubscriptions :: Set InvitationSecret -> Set ServerRequest
    makeSubscriptions = Set.fromList . map ReqClaimInvitation . Set.toList

    doAll :: (a -> b -> b) -> [a] -> b -> b
    doAll f = foldr (.) id . map f



instance Server.HasConfig ModelConfig where
  config = serverConfig

instance Subscriber.HasConfig ModelConfig where
  config = subscriberConfig

instance Server.HasServer Model where
  server = _server


-- Auto generated lenses:


-- Lenses for Model t:

_server :: Lens' (Model t) (Server t)
_server f fullConfig' = (\_server' -> fullConfig' { __server = _server' }) <$> f (__server fullConfig')


-- Lenses for FullAccount t:


subscriberConfig :: Lens' (ModelConfig t) (Subscriber.Config t)
subscriberConfig f modelConfig' = (\subscriberConfig' -> modelConfig' { _subscriberConfig = subscriberConfig' }) <$> f (_subscriberConfig modelConfig')

serverConfig :: Lens' (ModelConfig t) (Server.Config t)
serverConfig f modelConfig' = (\serverConfig' -> modelConfig' { _serverConfig = serverConfig' }) <$> f (_serverConfig modelConfig')


