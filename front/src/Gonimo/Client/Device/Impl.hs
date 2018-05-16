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

-- | Create an Account.
--
--   You can claim invitations and answer them.
--   At the moment we don't yet have a means for getting claimed invitations
--   from the server (ReqGetClaimedInvitations), so we only provide the ones of
--   the current session. If you restart gonimo, your claimed invitations are no
--   longer visible.
make :: ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
        , HasModel model, HasConfig c, HasModelConfig mConf t
        )
     => model t -> c t -> m (mConf t, Account t)
make model conf' = do
  let
    conf = handleInvitationIntent model conf'

  _claimedInvitations <- makeClaimedInvitations model
  let
    serverConfig' = answerInvitations conf

  subscriberConfig' <- subscribeInvitationClaims conf

  pure $ ( serverConfig' <> subscriberConfig'
         , Account {..}
         )

handleInvitationIntent :: ( Reflex t , HasModel model, HasConfig c)
                       => model t -> c t -> c t
handleInvitationIntent model conf
    = conf & onClaimInvitation %~ (<> fmapMaybe handleIntent (model ^. Host.onNewIntent) )
  where
    handleIntent :: Intent -> Maybe [InvitationSecret]
    handleIntent intent =
      let
        query = T.dropWhile (/= '?') intent
        encodedSecret = T.drop 1 . T.dropWhile (/= '=') $ query
        urlJson = T.encodeUtf8 encodedSecret
        json = urlDecode True urlJson
      in
        (:[]) <$> Aeson.decodeStrict json

-- | Provide the dynamic of claimed invitations.
--
--   TODO: Currently the backend does not have a way for retrieving claimed invitations,
--   so this Dynamic will only contain claimed invitations of the running session.
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

-- | Handle invitation answering.
--
--   The resulting ModelConfig will issue the needed server commands for
--   accepting/declining an invitation.
answerInvitations :: (Reflex t, HasConfig c, IsConfig mconf t, Server.HasConfig mconf)
  => c t -> mconf t
answerInvitations conf =
  let
    onAnswer = map (uncurry ReqAnswerInvitation) <$> conf^.onAnswerInvitation
  in
    mempty & Server.onRequest .~ onAnswer


-- | We currently subscribe invitation claims, to ensure they get through even on a lousy internet connection.
--
--   In the long run we should provide some means for reliably sending messages,
--   so we no longer have to misuse the subscriber functionality.
subscribeInvitationClaims :: (Reflex t, HasConfig c, MonadHold t m, MonadFix m, MonadJSM m
                             , IsConfig mconf t, Subscriber.HasConfig mconf)
                          => c t -> m (mconf t)
subscribeInvitationClaims conf = do
    mFromQueryString <- runMaybeT getInvitationSecret
    clearInvitationFromURL
    let initial = maybe Set.empty Set.singleton mFromQueryString

    invitationsToClaim <- foldDyn id initial
                          $ mergeWith (.) [ doAll Set.insert <$> conf ^. onClaimInvitation
                                          , doAll (Set.delete . fst) <$> conf ^. onAnswerInvitation
                                          ]

    pure $ mempty & Subscriber.subscriptions .~ fmap makeSubscriptions invitationsToClaim

  where
    makeSubscriptions :: Set InvitationSecret -> Set ServerRequest
    makeSubscriptions = Set.fromList . map ReqClaimInvitation . Set.toList

    doAll :: (a -> b -> b) -> [a] -> b -> b
    doAll f = foldr (.) id . map f

-- | Get the invitation secret from the query string in the browser address bar.
--
--   At the moment this function assumes that there will be no other queries than acceptInvitation.
getInvitationSecret :: forall m. (MonadPlus m, MonadJSM m) => m InvitationSecret
getInvitationSecret = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    queryString <- Location.getSearch location
    let secretString =
          let
            (_, startSecret) = T.drop 1 <$> T.breakOn "=" queryString
          in
            T.takeWhile (/='&') startSecret
    guard $ not (T.null secretString)
    let mDecoded = Aeson.decodeStrict . urlDecode True . T.encodeUtf8 $ secretString
    maybe mzero pure $ mDecoded

-- | Clear the accept invitation query from the browser address bar.
--
--   For the time being it simply clears the query string. This needs to be
--   fixed once we support other queries too.
clearInvitationFromURL :: forall m. (MonadJSM m) => m ()
clearInvitationFromURL = do
    window  <- DOM.currentWindowUnchecked
    location <- Window.getLocation window
    history <- Window.getHistory window
    href <- Location.getHref location
    emptyJSVal <- liftJSM $ toJSVal T.empty
    let withoutQuery = T.takeWhile (/= '?') href
    when (withoutQuery /= href) -- Setting same url caused app not loading with native Android (at some point) - yeah I know ...
      $ History.pushState history emptyJSVal ("gonimo" :: Text) (Just withoutQuery)


instance Server.HasConfig ModelConfig where
  config = serverConfig

instance Subscriber.HasConfig ModelConfig where
  config = subscriberConfig

-- Auto generated lenses:
-- Lenses for ModelConfig t:

subscriberConfig :: Lens' (ModelConfig t) (Subscriber.Config t)
subscriberConfig f modelConfig' = (\subscriberConfig' -> modelConfig' { _subscriberConfig = subscriberConfig' }) <$> f (_subscriberConfig modelConfig')

serverConfig :: Lens' (ModelConfig t) (Server.Config t)
serverConfig f modelConfig' = (\serverConfig' -> modelConfig' { _serverConfig = serverConfig' }) <$> f (_serverConfig modelConfig')


