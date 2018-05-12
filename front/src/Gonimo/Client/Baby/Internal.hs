{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Gonimo.Client.Baby.Internal where

import           Gonimo.Client.Prelude


import           Control.Exception                 (try)
import qualified Data.Text                         as T
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.MediaDevices            as MediaDevices
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import qualified GHCJS.DOM.Navigator               as Navigator
import           GHCJS.DOM.Types                   (MediaStream,
                                                    MediaStreamConstraints (..),
                                                    MonadJSM)
import qualified GHCJS.DOM.Types                   as JS hiding (askJSM, runJSM)
import qualified GHCJS.DOM.Window                  as Window
import qualified Language.Javascript.JSaddle.Monad as JS

import qualified Gonimo.Client.Baby.Socket         as Socket
import qualified Gonimo.Client.Host                as Host
import           Gonimo.Client.Model               (IsConfig)
import qualified Gonimo.Client.Router              as Router
import qualified Gonimo.Client.Server              as Server
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.Client.Subscriber          as Subscriber
import           Gonimo.Client.Util                (getVolumeInfo, oyd)
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import qualified Gonimo.Types                      as Gonimo

data Config t
  = Config  { _configNextCamera      :: Event t ()
            , _configEnableCamera    :: Event t Bool
            , _configEnableAutoStart :: Event t Bool
            , _configResponse        :: Event t API.ServerResponse
            , _configAuthData        :: Dynamic t API.AuthData
            , _configStartMonitor    :: Event t ()
            , _configStopMonitor     :: Event t ()
            , _configSetBabyName     :: Event t Text
            , _configSelectedFamily  :: Dynamic t API.FamilyId
            , _configGetUserMedia    :: Event t () -- Get a new media stream, useful for error handling.
            }

data Baby t
  = Baby { _selectedCamera :: Dynamic t [Text] -- ^ Cycled list of cameras. The
                                               --   first is the currently selected one.
         , _cameraEnabled    :: Dynamic t Bool
         , _cameraCount      :: Int -- ^ How many cameras do we have?
         , _autoStartEnabled :: Dynamic t Bool
         , _mediaStream      :: Dynamic t (Either JS.PromiseRejected MediaStream)
         , _socket           :: Socket.Socket t
         , _name             :: Dynamic t Text
         , _request          :: Event t [API.ServerRequest]
         , _volumeLevel      :: Event t Double
         }

data UI t
  = UI { _uiGoHome          :: Event t ()
       , _uiStartMonitor    :: Event t ()
       , _uiStopMonitor     :: Event t ()
       , _uiEnableCamera    :: Event t Bool
       , _uiEnableAutoStart :: Event t Bool
       , _uiSelectCamera    :: Event t ()
       , _uiSetBabyName     :: Event t Text
       , _uiRequest         :: Event t [API.ServerRequest]
       }

type HasModel model = (Socket.HasModel model, Router.HasRouter model)

type HasModelConfig c t = (IsConfig c t, Server.HasConfig c, Router.HasConfig c, Subscriber.HasConfig c, Host.HasConfig c)

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

baby :: forall model m t. (HasModel model, GonimoM model t m)
        => Config t -> m (Baby t)
baby config = mdo
  createStreamLabels <- getInitialMediaStream -- IMPORTANT: This has to be before retrieving camera devices!
  devices <- fixEmptyLabels <$> enumerateDevices

  let cameraCount' = length . filter ((== VideoInput) . mediaDeviceKind) $ devices

  selected <- handleCameraSelect config devices

  enabled  <- handleCameraEnable config mediaStream'
  let mSelected = (\enabled' selected' -> if enabled' then headMay selected' else Nothing)
                  <$> enabled <*> selected

  initialSelected <- sample $ current mSelected
  goodInit <- getConstrainedMediaStream (pure createStreamLabels) devices initialSelected

  gotNewStream <- performEvent $ getConstrainedMediaStream mediaStream' devices
                  <$> leftmost [ updated mSelected
                               , tag (current mSelected) $ config^.configGetUserMedia
                               ]
  let gotNewValidStream = fmapMaybeCheap (^?_Right) gotNewStream
  -- WARNING: Don't do that- -inifinte MonadFix loop will dawn on you!
  -- cSelected <- sample $ current selected
  mediaStream' :: Dynamic t (Either JS.PromiseRejected MediaStream) <- holdDyn goodInit gotNewStream

  sockEnabled <- holdDyn Gonimo.NoBaby
                 $ leftmost [ Gonimo.Baby <$> tag (current babyName) (config^.configStartMonitor)
                            , const Gonimo.NoBaby <$> config^.configStopMonitor
                            ]

  socket' <- Socket.socket $ Socket.Config { Socket._configResponse = config^.configResponse
                                           , Socket._configAuthData = config^.configAuthData
                                           , Socket._configEnabled = sockEnabled
                                           , Socket._configMediaStream = mediaStream'
                                           }
  -- OYD integration, if enabled by user. (Currently a hidden feature, user has to set local storage object 'OYD')
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

  pure $ Baby { _selectedCamera = selected
              , _autoStartEnabled = autoStart
              , _cameraCount = cameraCount'
              , _cameraEnabled = enabled
              , _mediaStream = mediaStream'
              , _socket = socket'
              , _name = babyName
              , _request = saveNameReq
              , _volumeLevel = volEvent
              }

handleCameraEnable :: forall model m t. GonimoM model t m => Config t -> Dynamic t (Either JS.PromiseRejected MediaStream) -> m (Dynamic t Bool)
handleCameraEnable config mediaStream' = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    mLastEnabled <- GStorage.getItem storage GStorage.cameraEnabled

    let initVal = fromMaybe True $ mLastEnabled

    let
      getStreamFailed = push pure $ either (const (Just False)) (const Nothing) <$> updated mediaStream'

    enabled <- (holdUniqDyn <=< holdDyn initVal)
               $ leftmost [ getStreamFailed
                          , config^.configEnableCamera
                          ]
    performEvent_
      $ GStorage.setItem storage GStorage.CameraEnabled <$> updated enabled
    pure enabled

handleCameraSelect :: forall model m t. GonimoM model t m => Config t -> [MediaDeviceInfo] -> m (Dynamic t [Text])
handleCameraSelect config devices = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    mLastCamera <- GStorage.getItem storage GStorage.selectedCamera

    let
      videoDevices :: [Text]
      videoDevices = map mediaDeviceLabel . filter ((== VideoInput) . mediaDeviceKind) $ devices

      initiallySelected :: [Text]
      initiallySelected = prependIfNotEqual mLastCamera . cycleDef [] $ videoDevices

      selectEvent = config^.configNextCamera

    selected <- foldDyn (\() cs -> drop 1 cs) initiallySelected selectEvent
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.selectedCamera) <$> updated (listToMaybe <$> selected)
    pure selected

  where
    prependIfNotEqual :: Eq a => Maybe a -> [a] -> [a]
    prependIfNotEqual _ [] = []
    prependIfNotEqual Nothing xs = xs
    prependIfNotEqual (Just a) xs@(x:_)
      | a /= x = a:xs
      | otherwise = xs

getMediaDeviceByLabel :: Text -> [MediaDeviceInfo] -> Maybe MediaDeviceInfo
getMediaDeviceByLabel label infos =
  let
    withLabel = filter ((== label) . mediaDeviceLabel) infos
  in
    headMay withLabel

-- On Android with Webview we can't use id's because they change on every start
-- and in addition the labels stay empty, so in order to identify cameras on
-- Android we hope that the order in the list stays the same.
fixEmptyLabels :: [MediaDeviceInfo] -> [MediaDeviceInfo]
fixEmptyLabels = zipWith fixLabel [1 :: Int ..]
  where
    fixLabel num info = if mediaDeviceLabel info == ""
                        then info { mediaDeviceLabel = "Device " <> T.pack (show num) }
                        else info

getInitialMediaStream :: forall m. MonadJSM m => m (Either JS.PromiseRejected MediaStream)
getInitialMediaStream = do
  navigator <- Window.getNavigator =<< DOM.currentWindowUnchecked
  constr <- makeSimpleMediaStreamConstraints True False
  getUserMedia' navigator $ Just constr

getConstrainedMediaStream :: forall m t. (MonadJSM m, Reflex t, MonadSample t m)
                         => Dynamic t (Either JS.PromiseRejected MediaStream) -> [MediaDeviceInfo] -> Maybe Text -> m (Either JS.PromiseRejected MediaStream)
getConstrainedMediaStream mediaStreams infos mLabel = do
  oldStream <- sample $ current mediaStreams
  either (const $ pure ()) stopMediaStream $ oldStream
  let mInfo = flip getMediaDeviceByLabel infos =<< mLabel
  navigator <- Window.getNavigator =<< DOM.currentWindowUnchecked
  constr <- case (mInfo, mLabel) of
    (_, Nothing)        -> makeSimpleMediaStreamConstraints True False
    (Nothing, Just _)   -> makeSimpleMediaStreamConstraints True True
    (Just info, Just _) -> makeConstraintsFromVideoInfo info
  getUserMedia' navigator $ Just constr


-- TODO: Check whether MediaStreamTrack.stop can throw and catch exceptions.
-- Depends on: https://github.com/ghcjs/ghcjs-dom/issues/78
stopMediaStream :: forall m. MonadJSM m => MediaStream -> m ()
stopMediaStream stream = do
    tracks <- MediaStream.getTracks stream
    traverse_ MediaStreamTrack.stop tracks
  -- where
  --   stopOrDoNothing action = action `JS.catch` handleException
  --   handleException :: SomeException

readAutoStart :: MonadJSM m => m Bool
readAutoStart = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  fromMaybe False <$> GStorage.getItem storage GStorage.autoStart

writeAutoStart :: MonadJSM m => Bool -> m ()
writeAutoStart haveAutoStart = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.autoStart haveAutoStart

readLastBabyName :: MonadJSM m => m Text
readLastBabyName = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  fromMaybe "baby" <$> GStorage.getItem storage GStorage.lastBabyName

writeLastBabyName :: MonadJSM m => Text -> m ()
writeLastBabyName lastBabyName = do
  storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
  GStorage.setItem storage GStorage.lastBabyName lastBabyName


getVolumeLevel :: forall model m t. GonimoM model t m
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


getUserMedia' :: (MonadIO m, MonadJSM m) => Navigator.Navigator -> Maybe MediaStreamConstraints -> m (Either JS.PromiseRejected MediaStream)
getUserMedia' nav md = do
  mediaDevices <- Navigator.getMediaDevices nav
  jsm <- JS.askJSM
  liftIO . try $ JS.runJSM (MediaDevices.getUserMedia mediaDevices md) jsm


-- Lenses for Config t:

configNextCamera :: Lens' (Config t) (Event t ())
configNextCamera f config' = (\configNextCamera' -> config' { _configNextCamera = configNextCamera' }) <$> f (_configNextCamera config')

configEnableCamera :: Lens' (Config t) (Event t Bool)
configEnableCamera f config' = (\configEnableCamera' -> config' { _configEnableCamera = configEnableCamera' }) <$> f (_configEnableCamera config')

configEnableAutoStart :: Lens' (Config t) (Event t Bool)
configEnableAutoStart f config' = (\configEnableAutoStart' -> config' { _configEnableAutoStart = configEnableAutoStart' }) <$> f (_configEnableAutoStart config')

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configAuthData :: Lens' (Config t) (Dynamic t API.AuthData)
configAuthData f config' = (\configAuthData' -> config' { _configAuthData = configAuthData' }) <$> f (_configAuthData config')

configStartMonitor :: Lens' (Config t) (Event t ())
configStartMonitor f config' = (\configStartMonitor' -> config' { _configStartMonitor = configStartMonitor' }) <$> f (_configStartMonitor config')

configStopMonitor :: Lens' (Config t) (Event t ())
configStopMonitor f config' = (\configStopMonitor' -> config' { _configStopMonitor = configStopMonitor' }) <$> f (_configStopMonitor config')

configSetBabyName :: Lens' (Config t) (Event t Text)
configSetBabyName f config' = (\configSetBabyName' -> config' { _configSetBabyName = configSetBabyName' }) <$> f (_configSetBabyName config')

configSelectedFamily :: Lens' (Config t) (Dynamic t API.FamilyId)
configSelectedFamily f config' = (\configSelectedFamily' -> config' { _configSelectedFamily = configSelectedFamily' }) <$> f (_configSelectedFamily config')

configGetUserMedia :: Lens' (Config t) (Event t ())
configGetUserMedia f config' = (\configGetUserMedia' -> config' { _configGetUserMedia = configGetUserMedia' }) <$> f (_configGetUserMedia config')

-- Lenses for Baby t:

selectedCamera :: Lens' (Baby t) (Dynamic t [Text])
selectedCamera f baby' = (\selectedCamera' -> baby' { _selectedCamera = selectedCamera' }) <$> f (_selectedCamera baby')

cameraEnabled :: Lens' (Baby t) (Dynamic t Bool)
cameraEnabled f baby' = (\cameraEnabled' -> baby' { _cameraEnabled = cameraEnabled' }) <$> f (_cameraEnabled baby')

cameraCount :: Lens' (Baby t) Int
cameraCount f baby' = (\cameraCount' -> baby' { _cameraCount = cameraCount' }) <$> f (_cameraCount baby')

autoStartEnabled :: Lens' (Baby t) (Dynamic t Bool)
autoStartEnabled f baby' = (\autoStartEnabled' -> baby' { _autoStartEnabled = autoStartEnabled' }) <$> f (_autoStartEnabled baby')

mediaStream :: Lens' (Baby t) (Dynamic t (Either JS.PromiseRejected MediaStream))
mediaStream f baby' = (\mediaStream' -> baby' { _mediaStream = mediaStream' }) <$> f (_mediaStream baby')

socket :: Lens' (Baby t) (Socket.Socket t)
socket f baby' = (\socket' -> baby' { _socket = socket' }) <$> f (_socket baby')

name :: Lens' (Baby t) (Dynamic t Text)
name f baby' = (\name' -> baby' { _name = name' }) <$> f (_name baby')

request :: Lens' (Baby t) (Event t [API.ServerRequest])
request f baby' = (\request' -> baby' { _request = request' }) <$> f (_request baby')

volumeLevel :: Lens' (Baby t) (Event t Double)
volumeLevel f baby' = (\volumeLevel' -> baby' { _volumeLevel = volumeLevel' }) <$> f (_volumeLevel baby')



-- Lenses for UI t:

uiGoHome :: Lens' (UI t) (Event t ())
uiGoHome f uI' = (\uiGoHome' -> uI' { _uiGoHome = uiGoHome' }) <$> f (_uiGoHome uI')

uiStartMonitor :: Lens' (UI t) (Event t ())
uiStartMonitor f uI' = (\uiStartMonitor' -> uI' { _uiStartMonitor = uiStartMonitor' }) <$> f (_uiStartMonitor uI')

uiStopMonitor :: Lens' (UI t) (Event t ())
uiStopMonitor f uI' = (\uiStopMonitor' -> uI' { _uiStopMonitor = uiStopMonitor' }) <$> f (_uiStopMonitor uI')

uiEnableCamera :: Lens' (UI t) (Event t Bool)
uiEnableCamera f uI' = (\uiEnableCamera' -> uI' { _uiEnableCamera = uiEnableCamera' }) <$> f (_uiEnableCamera uI')

uiEnableAutoStart :: Lens' (UI t) (Event t Bool)
uiEnableAutoStart f uI' = (\uiEnableAutoStart' -> uI' { _uiEnableAutoStart = uiEnableAutoStart' }) <$> f (_uiEnableAutoStart uI')

uiSelectCamera :: Lens' (UI t) (Event t ())
uiSelectCamera f uI' = (\uiSelectCamera' -> uI' { _uiSelectCamera = uiSelectCamera' }) <$> f (_uiSelectCamera uI')

uiSetBabyName :: Lens' (UI t) (Event t Text)
uiSetBabyName f uI' = (\uiSetBabyName' -> uI' { _uiSetBabyName = uiSetBabyName' }) <$> f (_uiSetBabyName uI')

uiRequest :: Lens' (UI t) (Event t [API.ServerRequest])
uiRequest f uI' = (\uiRequest' -> uI' { _uiRequest = uiRequest' }) <$> f (_uiRequest uI')



cycleDef :: [a] -> [a] -> [a]
cycleDef d [] = d
cycleDef _ xs = cycle xs
