{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Gonimo.Client.Baby.Internal where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Maybe (catMaybes)
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Reflex.Dom.Core
import qualified Gonimo.Db.Entities         as Db

import           GHCJS.DOM.Types                   (MediaStream, MonadJSM, Dictionary)
import           GHCJS.DOM.NavigatorUserMediaError (UserMediaException(..))
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Gonimo.Client.Baby.Socket         as Socket
import qualified Gonimo.Types                      as Gonimo
import           Gonimo.Client.Util                (oyd, getVolumeInfo)
import           Control.Exception                 (try)
import qualified Language.Javascript.JSaddle.Monad as JS

data Config t
  = Config  { _configSelectCamera :: Event t Text
            , _configEnableCamera :: Event t Bool
            , _configEnableAutoStart :: Event t Bool
            , _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configStartMonitor  :: Event t ()
            , _configStopMonitor  :: Event t ()
            , _configSetBabyName :: Event t Text
            , _configSelectedFamily :: Dynamic t Db.FamilyId
            , _configGetUserMedia :: Event t () -- Get a new media stream, usefull for error handling.
            }

data Baby t
  = Baby { _videoDevices :: Dynamic t [MediaDeviceInfo]
         , _selectedCamera :: Dynamic t (Maybe Text)
         , _cameraEnabled :: Dynamic t Bool
         , _autoStartEnabled :: Dynamic t Bool
         , _mediaStream :: Dynamic t (Either UserMediaException MediaStream)
         , _socket :: Socket.Socket t
         , _name :: Dynamic t Text
         , _request :: Event t [API.ServerRequest]
         , _volumeLevel :: Event t Double
         }

data UI t
  = UI { _uiGoHome :: Event t ()
       , _uiStartMonitor  :: Event t ()
       , _uiStopMonitor  :: Event t ()
       , _uiEnableCamera :: Event t Bool
       , _uiEnableAutoStart :: Event t Bool
       , _uiSelectCamera  :: Event t Text
       , _uiSetBabyName :: Event t Text
       , _uiRequest :: Event t [API.ServerRequest]
       }

makeLenses ''Config
makeLenses ''Baby
makeLenses ''UI

uiSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (UI t) -> m (UI t)
uiSwitchPromptly ev
  = UI <$> switchPromptly never (_uiGoHome <$> ev)
       <*> switchPromptly never (_uiStartMonitor <$> ev)
       <*> switchPromptly never (_uiStopMonitor <$> ev)
       <*> switchPromptly never (_uiEnableCamera <$> ev)
       <*> switchPromptly never (_uiEnableAutoStart <$> ev)
       <*> switchPromptly never (_uiSelectCamera <$> ev)
       <*> switchPromptly never (_uiSetBabyName <$> ev)
       <*> switchPromptly never (_uiRequest <$> ev)

uiSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (UI t) -> UI t
uiSwitchPromptlyDyn ev
  = UI ( switchPromptlyDyn (_uiGoHome <$> ev) )
       ( switchPromptlyDyn (_uiStartMonitor <$> ev) )
       ( switchPromptlyDyn (_uiStopMonitor <$> ev) )
       ( switchPromptlyDyn (_uiEnableCamera <$> ev) )
       ( switchPromptlyDyn (_uiEnableAutoStart <$> ev) )
       ( switchPromptlyDyn (_uiSelectCamera <$> ev) )
       ( switchPromptlyDyn (_uiSetBabyName <$> ev) )
       ( switchPromptlyDyn (_uiRequest <$> ev) )

baby :: forall m t. (MonadWidget t m, HasWebView m)
        => Config t -> m (Baby t)
baby config = mdo
  badInit <- getInitialMediaStream -- IMPORTANT: This has to be before retrieving camera devices!
  initDevices <- enumerateDevices
  -- Get devicelist whenever stream changes - if only audio is requested the first time - the list will be empty.
  gotDevices <- performEvent $ const enumerateDevices <$> gotNewValidStream
  devices <- uniqDyn <$> holdDyn initDevices gotDevices
  let
    videoDevices' :: Dynamic t [MediaDeviceInfo]
    videoDevices' = filter ((== VideoInput) . mediaDeviceKind) <$> devices

  selected <- handleCameraSelect config devices
  enabled  <- handleCameraEnable config
  let mSelected = (\enabled' selected' -> if enabled' then selected' else Nothing)
                  <$> enabled <*> selected

  gotNewStream <- performEvent $ uncurry (getConstrainedMediaStream mediaStream')
                  <$> leftmost [ attach (current videoDevices') (updated mSelected)
                               , attach (current videoDevices') (tag (current mSelected) $ config^.configGetUserMedia)
                               ]
  let gotNewValidStream = fmapMaybeCheap (^?_Right) gotNewStream
  -- WARNING: Don't do that- -inifinte MonadFix loop will down on you!
  -- cSelected <- sample $ current selected
  mediaStream' :: Dynamic t (Either UserMediaException MediaStream) <- holdDyn badInit gotNewStream

  sockEnabled <- holdDyn Gonimo.NoBaby
                 $ leftmost [ Gonimo.Baby <$> tag (current babyName) (config^.configStartMonitor)
                            , const Gonimo.NoBaby <$> config^.configStopMonitor
                            ]

  socket' <- Socket.socket $ Socket.Config { Socket._configResponse = config^.configResponse
                                           , Socket._configAuthData = config^.configAuthData
                                           , Socket._configEnabled = sockEnabled
                                           , Socket._configMediaStream = mediaStream'
                                           }
  -- OYD integration, if enabled by user. (Currently a hidden feature, use has to set local storage object 'OYD')
  setOYDBabyNameEv <- performEvent $ uncurry oyd <$> attach (current babyName) gotNewValidStream
  setOYDBabyNameBeh <- hold (const (pure ())) setOYDBabyNameEv
  performEvent_ $ attachWith ($) setOYDBabyNameBeh (updated babyName)

  initAutoStart <- readAutoStart
  autoStart <- holdDyn initAutoStart $ config^.configEnableAutoStart
  performEvent_ $ writeAutoStart <$> updated autoStart

  initBabyName <- readLastBabyName
  babyName <- holdDyn initBabyName $ config^.configSetBabyName
  let mkSaveNameReq = API.ReqSaveBabyName <$> current (config^.configSelectedFamily)
  let saveNameReq = fmap (:[]) . attachWith ($) mkSaveNameReq
                    $ leftmost [ updated babyName
                               , tag (current babyName) (config^.configStartMonitor)
                               ]
  performEvent_ $ writeLastBabyName <$> updated babyName

  volEvent <- flip getVolumeLevel sockEnabled $ fmap (^?_Right) mediaStream'

  pure $ Baby { _videoDevices = videoDevices'
              , _selectedCamera = selected
              , _autoStartEnabled = autoStart
              , _cameraEnabled = enabled
              , _mediaStream = mediaStream'
              , _socket = socket'
              , _name = babyName
              , _request = saveNameReq
              , _volumeLevel = volEvent
              }

handleCameraEnable :: forall m t. (MonadHold t m, MonadJSM (Performable m), MonadJSM m, PerformEvent t m)
                      => Config t -> m (Dynamic t Bool)
handleCameraEnable config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastEnabled <- GStorage.getItem storage GStorage.cameraEnabled

    let initVal = fromMaybe True $ mLastEnabled

    enabled <- holdDyn initVal (config^.configEnableCamera)
    performEvent_
      $ GStorage.setItem storage GStorage.CameraEnabled <$> updated enabled
    pure enabled

handleCameraSelect :: forall m t. (MonadHold t m, MonadJSM (Performable m), MonadJSM m, PerformEvent t m, TriggerEvent t m)
                      => Config t -> Dynamic t [MediaDeviceInfo] -> m (Dynamic t (Maybe Text))
handleCameraSelect config devices = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastCameraLabel <- GStorage.getItem storage GStorage.selectedCamera

    let
      mLastUsedInfo :: Dynamic t (Maybe MediaDeviceInfo)
      mLastUsedInfo
        = case mLastCameraLabel of
            Nothing
              -> pure Nothing
            Just lastCameraLabel
              -> headMay . filter ((== lastCameraLabel) . mediaDeviceLabel) <$> devices -- Only if still valid!

    let initVal = runMaybeT $ mediaDeviceLabel <$> (MaybeT mLastUsedInfo <|> MaybeT (headMay <$> devices))

    (initEv, trigger) <- newTriggerEvent
    liftIO $ trigger () -- We need to trigger a change for selecting the right MediaStream, we cannot sample current because this would result in a MonadFix loop.
    let selectEvent = leftmost [Just <$> config^.configSelectCamera, tag (current initVal) initEv]
    withoutInit <- holdDyn Nothing selectEvent
    let selected = zipDynWith (<|>) withoutInit initVal
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.selectedCamera) <$> updated selected

    pure selected

getMediaDeviceByLabel :: Text -> [MediaDeviceInfo] -> Maybe MediaDeviceInfo
getMediaDeviceByLabel label infos =
  let
    withLabel = filter ((== label) . mediaDeviceLabel) infos
  in
    headMay withLabel

getInitialMediaStream :: forall m. MonadJSM m => m (Either UserMediaException MediaStream)
getInitialMediaStream = do
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- makeSimpleUserMediaDictionary True False
  getUserMedia' navigator $ Just constr

getConstrainedMediaStream :: forall m t. (MonadJSM m, Reflex t, MonadSample t m)
                         => Dynamic t (Either UserMediaException MediaStream) -> [MediaDeviceInfo] -> Maybe Text -> m (Either UserMediaException MediaStream)
getConstrainedMediaStream mediaStreams infos mLabel = do
  oldStream <- sample $ current mediaStreams
  either (const $ pure ()) stopMediaStream $ oldStream
  let mInfo = flip getMediaDeviceByLabel infos =<< mLabel
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- case (mInfo, mLabel) of
    (_, Nothing)        -> makeSimpleUserMediaDictionary True False
    (Nothing, Just _)   -> makeSimpleUserMediaDictionary True True
    (Just info, Just _) -> makeDictionaryFromVideoInfo info
  getUserMedia' navigator $ Just constr


stopMediaStream :: forall m. MonadJSM m => MediaStream -> m ()
stopMediaStream stream = do
  tracks <- catMaybes <$> MediaStream.getTracks stream
  traverse_ MediaStreamTrack.stop tracks

readAutoStart :: MonadJSM m => m Bool
readAutoStart = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  fromMaybe False <$> GStorage.getItem storage GStorage.autoStart

writeAutoStart :: MonadJSM m => Bool -> m ()
writeAutoStart haveAutoStart = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.autoStart haveAutoStart

readLastBabyName :: MonadJSM m => m Text
readLastBabyName = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  fromMaybe "baby" <$> GStorage.getItem storage GStorage.lastBabyName

writeLastBabyName :: MonadJSM m => Text -> m ()
writeLastBabyName lastBabyName = do
  storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.lastBabyName lastBabyName


getVolumeLevel :: forall m t. (MonadWidget t m, HasWebView m)
                  => Dynamic t (Maybe MediaStream)  -> Dynamic t Gonimo.DeviceType -> m (Event t Double)
getVolumeLevel mediaStream' sockEnabled = do
    (volEvent, triggerVolumeEvent) <- newTriggerEvent
    let updateVolInfoEv = updated . fmap sequence $ zipDyn sockEnabled mediaStream'

    newCleanupEv <- performEvent $ getVolInfo triggerVolumeEvent <$> updateVolInfoEv
    mCleanup <- holdDyn Nothing $ newCleanupEv
    let doCleanup = push (\_ -> sample $ current mCleanup) (updated mCleanup)
    performEvent_ $ doCleanup

    pure volEvent
  where
    getVolInfo :: forall m1. (MonadJSM m1)
                  => (Double -> IO ()) -> Maybe (Gonimo.DeviceType, MediaStream) -> m1 (Maybe (m1 ()))
    getVolInfo _ Nothing = pure Nothing
    getVolInfo triggerVolumeEvent (Just (Gonimo.NoBaby, stream')) = Just <$> getVolumeInfo stream' (liftIO . triggerVolumeEvent)
    getVolInfo _ _ = pure Nothing


getUserMedia' :: (MonadIO m, MonadJSM m) => Navigator.Navigator -> Maybe Dictionary -> m (Either UserMediaException MediaStream)
getUserMedia' nav md = do
  jsm <- JS.askJSM
  liftIO . try $ JS.runJSM (Navigator.getUserMedia nav md) jsm
