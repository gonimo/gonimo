{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Parent.UI where

import           Control.Lens
import           Data.Foldable
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import qualified GHCJS.DOM.MediaStream            as MediaStream
import           Reflex.Dom.Core

import qualified Gonimo.Client.App.Types          as App
import qualified Gonimo.Client.Auth.Impl          as Auth
import           Gonimo.Client.ConfirmationButton (mayAddConfirmation,
                                                   mayAddConfirmation', _No,
                                                   _Yes)
import qualified Gonimo.Client.DeviceList         as DeviceList
import qualified Gonimo.Client.Host               as Host
import qualified Gonimo.Client.Invite             as Invite
import           Gonimo.Client.Model              (IsConfig)
import qualified Gonimo.Client.NavBar             as NavBar
import qualified Gonimo.Client.Parent.Connections as C
import           Gonimo.Client.Parent.UI.I18N
import           Gonimo.Client.Prelude
import           Gonimo.Client.Reflex
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Router             (HasRouter (..), Route (..),
                                                   onGoBack, onSetRoute)
import qualified Gonimo.Client.Router             as Router
import           Gonimo.Client.Server             hiding (HasModel)
import qualified Gonimo.Client.Server             as Server
import           Gonimo.Client.WebRTC.Channel     (Channel, ReceivingState (..),
                                                   worstState)
import qualified Gonimo.Client.WebRTC.Channel     as Channel
import           Gonimo.SocketAPI.Types           (DeviceId)
import           Gonimo.Types                     (_Baby)




type HasModel model = (Invite.HasModel model, HasServer model, Auth.HasAuth model, HasRouter model)

type HasModelConfig c t = (IsConfig c t, Server.HasConfig c, Router.HasConfig c, Host.HasConfig c)

ui :: forall model mConf m t. (HasModel model, GonimoM model t m, HasModelConfig mConf t)
            => App.Loaded t -> DeviceList.DeviceList t -> m (mConf t)
ui loaded deviceList = mdo
  model <- ask
  connections' <- C.connections $ C.Config { C._configResponse = model^.onResponse
                                           , C._configAuthData = loaded^.App.authData
                                           , C._configConnectBaby = devicesUI^.DeviceList.uiConnect
                                           , C._configDisconnectAll = leftmost [ onCleanupRequested
                                                                               , viewUI^.C.videoViewDisconnectAll
                                                                               ]
                                           , C._configDisconnectBaby = leftmost [ devicesUI^.DeviceList.uiDisconnect
                                                                                , viewUI^.C.videoViewDisconnectBaby
                                                                                ]
                                           }
  handleUnreliableAlert connections'

  let noStreamsLeft = fmap (const ()) . ffilter id $ Map.null <$> updated (connections'^.C.streams)

  let showParentView = const "isParentView" <$> leftmost [ devicesUI^.DeviceList.uiConnect
                                                         , devicesUI^.DeviceList.uiShowStream
                                                         ]
  let showParentManage = const "isParentManage" <$> leftmost [ viewUI^.C.videoViewNavBar^.NavBar.backClicked
                                                             , invite^.Invite.uiGoBack
                                                             , invite^.Invite.uiDone
                                                             , noStreamsLeft
                                                             ]
  let showInviteView = const "isInviteView" <$> inviteRequested

  let showBroken = fmap not $ (||) <$> connections'^.C.brokenConnections <*> model^.Auth.isOnline

  _ <- dyn $ Auth.connectionLossScreen' <$> showBroken

  selectedView <- holdDyn "isParentManage" $ leftmost [showParentView, showParentManage, showInviteView]
  let parentViewShown = (== "isParentView") <$> selectedView

  (navBar, devicesUI, inviteRequested) <-
    elDynClass "div" (pure "container has-footer parentManage " <> selectedView) $ do
      manageUi loaded deviceList connections'

  viewUI <-
    elDynClass "div" (pure "container parentView " <> selectedView) $ do
      viewUi loaded deviceList connections' parentViewShown

  invite <-
    elDynClass "div" (pure "container inviteView " <> selectedView) $ do
      firstCreation <- headE inviteRequested
      let inviteUI
            = Invite.ui loaded
              $ Invite.Config { Invite._configResponse = model^.onResponse
                              , Invite._configSelectedFamily = loaded^.App.selectedFamily
                              , Invite._configAuthenticated = model^.Auth.onAuthenticated
                              , Invite._configCreateInvitation = never
                              }
      dynInvite <- widgetHold (pure def) $ const inviteUI <$> firstCreation
      pure $ Invite.inviteSwitchPromptlyDyn dynInvite

  (onCleanupRequested, leaveConf) <- askLeaveConfirmation connections'

  let goHomeTrigger = leftmost [ navBar^.NavBar.backClicked
                               , navBar^.NavBar.homeClicked
                               , viewUI^.C.videoViewNavBar^.NavBar.homeClicked
                               ]
  let
    mConf = mempty & Router.onGoBack .~ goHomeTrigger
                   & Server.onRequest .~ connections'^.C.request
                                         <> devicesUI^.DeviceList.uiRequest
                                         <> invite^.Invite.request
                                         <> navBar^.NavBar.request
                                         <> viewUI^.C.videoViewNavBar.NavBar.request
                   & Host.appKillMask .~ (current $ Map.null <$> connections'^.C.streams)
  pure $ mconcat [ mConf
                 , leaveConf
                 ]

-- | Show confirmation dialog on history back navigation.
--
--   If confirmed, another history back event gets fired - finally leaving the parent station.
--   If not confirmed, push another `RouteParent` on the stack so we can ask again
--   for confirmation next time.

--   The returned event fires, if the user confirmed leaving, but before any
--   cleanup happened. Thus it can and must be used for triggering the needed
--   cleanup.
--
--   TODO: If the user presses the back button twice - we don't do any cleanup!
--   Any baby connections will stay alive!
askLeaveConfirmation :: forall model mConf m t.
  ( GonimoM model t m, HasRouter model
  , HasModelConfig mConf t
  )
  => C.Connections t -> m (Event t (), mConf t)
askLeaveConfirmation connections' = do
  (onAddRoute, triggerAddRoute) <- newTriggerEvent
  -- Add another page to the history stack, so we can ask for confirmation.
  liftIO $ triggerAddRoute RouteParent

  let openStreams = connections'^.C.streams
  route' <- view Router.route
  onWantsLeave <- everySecond -- Ignore building up events (onSetRoute below) ..
    $ push -- User pressed back button.
        (\newRoute -> do
            currentRoute <- sample $ current route'

            if currentRoute == RouteParent && newRoute == RouteParent
            then pure $ Just ()
            else pure $ Nothing
        )
        (updated route')
  confirmed <- mayAddConfirmation' leaveConfirmation onWantsLeave (not . Map.null <$> openStreams)

  let
    onLeaveReq = fmapMaybe (preview _Yes) confirmed
    onStay     = fmapMaybe (fmap (const RouteParent) . preview _No) confirmed

  -- Ensure cleanup!
  leaveRequested <- hold False $ const True <$> onLeaveReq
  let channelsEmpty = Map.null <$> connections' ^. C.channelMap
  let onLeave = fmap (const ()) . ffilter id
                $ leftmost [ tag (current channelsEmpty) onLeaveReq
                           , attachWith (&&) leaveRequested (updated channelsEmpty)
                           ]

  let conf = mempty & onSetRoute .~ leftmost [ onStay, onAddRoute ]
                    & onGoBack   .~ onLeave
  pure (onLeaveReq, conf)



manageUi :: forall model m t. GonimoM model t m
            => App.Loaded t -> DeviceList.DeviceList t -> C.Connections t -> m (NavBar.NavBar t, DeviceList.UI t, Event t ())
manageUi loaded deviceList connections' = do
      navBar <- NavBar.navBar (NavBar.Config loaded deviceList)
      let openStreams = connections'^.C.streams

      devicesUI <- DeviceList.ui loaded deviceList
                   (connections'^.C.channelMap)
                   (Set.fromList . Map.keys <$> openStreams)
      inviteRequested <- elClass "div" "footer" $
            makeClickable . elAttr' "div" (addBtnAttrs "device-add") $ trText Add_Device

      pure (navBar, devicesUI, inviteRequested)

viewUi :: forall model m t. GonimoM model t m
            => App.Loaded t -> DeviceList.DeviceList t -> C.Connections t
            -> Dynamic t Bool -> m (C.VideoView t)
viewUi loaded deviceList connections isShown = do
  let streams = connections^.C.streams
  let singleVideoClass = (\streams' -> if Map.size streams' == 1 then "single-video " else "") <$> streams
  elDynClass "div" (pure "parent " <> singleVideoClass) $ do
    navBar <- NavBar.navBar (NavBar.Config loaded deviceList)


    _ <- dyn $ renderFakeVideos connections
    closedsEvEv <- dyn $ renderVideos deviceList connections isShown
    let closedEvEv  = leftmost <$> closedsEvEv
    closedEv <- switchPromptly never closedEvEv
    stopAllClicked <- elClass "div" "stream-menu" $
        makeClickable . elAttr' "div" (addBtnAttrs "stop") $ trText Stop_All
    pure $ C.VideoView navBar closedEv stopAllClicked

renderFakeVideos :: forall model m t. GonimoM model t m => C.Connections t -> Dynamic t (m ())
renderFakeVideos connections =
  let
    renderFake stream = mediaVideo stream ("autoplay" =: "true" <> "style" =: "width:100%;height:100%;" <> "class" =: "fakeVideo" <> "muted" =: "true")
  in
    traverse_ renderFake . Map.elems <$> connections^.C.origStreams

renderVideos :: forall model m t. GonimoM model t m => DeviceList.DeviceList t -> C.Connections t
             -> Dynamic t Bool -> Dynamic t (m [Event t DeviceId])
renderVideos deviceList connections' isShown =
    uncurry renderVideosOrNone <$> shownStreams
  where
    shownStreams = (,) <$> isShown <*> streamsList
    streamsList = Map.toList <$> connections'^.C.streams

    dynChannelMap = connections'^.C.channelMap

    renderVideosOrNone :: Bool -> [(DeviceId, C.StreamData t)] -> m [Event t DeviceId]
    renderVideosOrNone False _ = pure []
    renderVideosOrNone _ [] = do
        let maxLoadingTime = 7 -- Seven seconds seems plenty ...
        errorEv <- delayed maxLoadingTime
        showError <- holdDyn False $ const True <$> errorEv
        _ <- dyn $ renderLoadingOrError <$> showError
        pure []
    renderVideosOrNone _ xs = traverse renderVideo xs

    renderLoadingOrError :: Bool -> m ()
    renderLoadingOrError False
      = elClass "div" "dismissible-overlay info-overlay"
        $ trText Loading_your_stream
    renderLoadingOrError True
      = elClass "div" "dismissible-overlay warning-overlay"
        $ trText Connectivity_issues

    renderVideo :: (DeviceId, C.StreamData t) -> m (Event t DeviceId)
    renderVideo (key, C.StreamData stream volEvent) = do
      hasVideo <- not . null <$> MediaStream.getVideoTracks stream
      let hasBackground = if hasVideo then "" else "justAudio "
      elDynClass "div" (dynConnectionClass key <> pure "stream-baby " <> pure hasBackground) $ do
        elClass "div" "broken-overlay" $ do
          elClass "div" "broken-message" $ trText Connection_Lost
        elClass "div" "stream-baby-heading" $ do
          elClass "div" "stream-baby-name" $ do
            el "h1" $ dynText ((^. at key._Just._Baby) <$> deviceList^.DeviceList.onlineDevices)
        mediaVideo stream ("autoplay" =: "true")
        closeClicked <- makeClickable $ elAttr' "div" (addBtnAttrs "btn-close-x") blank
        renderVolumemeter volEvent
        pure $ const key <$> closeClicked

    dynConnectionClass key = connectionClass key <$> dynChannelMap

    connectionClass :: DeviceId -> Map.Map DeviceId (Channel t) -> Text
    connectionClass key chanMap
      = if chanMap ^? at key . _Just . to Channel.needsAlert == Just True
        then "connectionBroken "
        else case chanMap ^? at key . _Just . to worstState of
                               Just StateUnreliable -> "connectionUnreliable "
                               _ -> ""

    -- isUnreliable Nothing = False
    -- isUnreliable (Just chan) = chan^.audioReceivingState == StateUnreliable
    --                            || chan^.videoReceivingState == StateUnreliable


leaveConfirmation :: GonimoM model t m => m ()
leaveConfirmation = do
    el "h3" $ trText Really_stop_parent_station
    el "p" $ trText All_open_streams_will_be_disconnected

handleUnreliableAlert :: forall model t m. GonimoM model t m => C.Connections t -> m ()
handleUnreliableAlert connections' = mdo
  let gotUnreliable = updated $ connections'^.C.unreliableConnections

  let
    renderAlert False = dismissibleOverlay "success-overlay " 4 $ do
      trText Connection_is_reliable
      el "br" blank
      trText Or_gone
    renderAlert True = dismissibleOverlay "warning-overlay " 6 $ do
      trText Connection_unreliable
      el "br" blank
      trText Might_break_unnoticed_no_alert
  _ <- widgetHold (pure ()) $ renderAlert <$> gotUnreliable
  pure ()

-- Old currently no longer used:
-- unreliableAlert :: forall t m. GonimoM model t m => m (Event t ())
-- unreliableAlert = do
--   elClass "div" "fullScreenOverlay" $ do
--     elClass "div" "container" $ do
--       el "h1" $ trText Connection_probably_unreliable
--       el "br" blank
--       el "br" blank
--       trText We_are_sorry_we_can_not_guarantee_a_reliable_connection
--       el "br" blank
--       trText This_is_indicated_by_a_red_border
--       el "br" blank
--       trText If_you_see_a_red_border
--       el "br" blank
--       el "h1" $ trText What_can_I_do
--       el "ul" $ do
--         el "li" $ trText Use_a_different_browser_currently_we_recommend_Chrome
--         el "li" $ trText Disconnect_Connect_periodically_to_be_sure_everything_is_alright
--         el "li" $ trText For_Audio_connections_have_some_sound_at_the_baby_side_e_g_open_the_window
--         el "li" $ trText For_Video_connections_have_some_constant_motion_in_the_picture_for_example_a_clock

--       el "br" blank
--       el "br" blank

--       okClicked <- makeClickable . elAttr' "div" (addBtnAttrs "btn-lang") $ trText OK
--       pure $ okClicked
