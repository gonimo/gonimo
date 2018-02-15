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


data Model t
  = Model { __server :: Server t
         }

type HasModel d = Server.HasServer d

data FullAccount t
  = FullAccount { __account         :: Account t

                  -- | For subscribing important commands.
                  --
                  --   We subscribe important commands to ensure they will be
                  --   delivered even on connection problems.
                , _subscriberConfig :: Subscriber.Config t

                  -- | Commands going to the server.
                , _serverConfig     :: Server.Config t
                }

makeClaimedInvitations
  :: forall d t m. (Reflex t, MonadHold t m, MonadFix m, HasModel d)
  => d t -> m (Dynamic t ClaimedInvitations)
makeClaimedInvitations model =
  let
    onClaimedInvitation :: Event t (InvitationSecret, InvitationInfo)
    onClaimedInvitation = fmapMaybe (^?_ResClaimedInvitation) $ model ^. Server.onResponse

    onAnsweredInvitation :: Event t InvitationSecret
    onAnsweredInvitation = fmapMaybe (^?_ResAnsweredInvitation._1) $ model ^. Server.onResponse
  in
    foldDyn id Map.empty $ leftmost [ uncurry Map.insert <$> onClaimedInvitation
                                    , Map.delete <$> onAnsweredInvitation
                                    ]

answerInvitations :: (Reflex t, HasConfig c) => c t -> Server.Config t
answerInvitations conf =
  let
    onAnswer = map (uncurry ReqAnswerInvitation) <$> conf^.onAnswerInvitation
  in
    mempty & Server.onRequest .~ onAnswer

subscribeInvitationClaims :: (Reflex t, HasConfig c, MonadHold t m, MonadFix m)
                          => c t -> m (Subscriber.Config t)
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



instance Server.HasConfig FullAccount where
  config = serverConfig

instance Subscriber.HasConfig FullAccount where
  config = subscriberConfig

instance Server.HasServer Model where
  server = _server

instance HasAccount FullAccount where
  account = _account

-- Auto generated lenses:


-- Lenses for Model t:

_server :: Lens' (Model t) (Server t)
_server f fullConfig' = (\_server' -> fullConfig' { __server = _server' }) <$> f (__server fullConfig')


-- Lenses for FullAccount t:

_account :: Lens' (FullAccount t) (Account t)
_account f fullAccount' = (\_account' -> fullAccount' { __account = _account' }) <$> f (__account fullAccount')

subscriberConfig :: Lens' (FullAccount t) (Subscriber.Config t)
subscriberConfig f fullAccount' = (\subscriberConfig' -> fullAccount' { _subscriberConfig = subscriberConfig' }) <$> f (_subscriberConfig fullAccount')

serverConfig :: Lens' (FullAccount t) (Server.Config t)
serverConfig f fullAccount' = (\serverConfig' -> fullAccount' { _serverConfig = serverConfig' }) <$> f (_serverConfig fullAccount')


