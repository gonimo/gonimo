{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Gonimo.Client.Host.Impl
Description : Host specific data is served by this module.
Copyright   : (c) Robert Klotzner, 2018
At the moment this is only about managing of claimed invitations.
-}
module Gonimo.Client.Host.Impl ( -- * Interface
                               module API
                               -- * Types
                             , HostVars(..)
                             , onStartVar
                             , onStopVar
                             , onNewIntentVar
                             , useBrowserHistory
                               -- * Creation
                             , makeEmptyHostVars
                             , make
                             ) where


import           Control.Concurrent          (forkIO)
import           Control.Concurrent.MVar
import           Data.Time.Clock
import           GHCJS.DOM.Types             (MonadJSM, liftJSM, toJSVal)
import qualified Language.Javascript.JSaddle as JS


import           Gonimo.Client.Host          as API
import           Gonimo.Client.Prelude



-- | The HostVars, provides a means for passing in the necessary host functionality.
data HostVars
  = HostVars { _onStartVar        :: MVar ()
             , _onStopVar         :: MVar ()
             , _onNewIntentVar    :: MVar Intent
             , _useBrowserHistory :: Bool
             }

-- | Make a default host config.
makeEmptyHostVars :: IO HostVars
makeEmptyHostVars = do
  HostVars <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar <*> pure True

-- | Time after onStop when app should be killed if possible.
--
--   This is for power savings on Android. We currently default to two minutes.
quitDelay :: NominalDiffTime
quitDelay = 120

-- | JavaScript object providing native functionality.
nativeHost :: Text
nativeHost = "nativeHost"

-- | Create a Host.
--
make :: ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
        , TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
        , PostBuild t m
        , HasConfig c
        )
     => HostVars -> c t -> m (Host t)
make hostConf conf = do
  _onStop      <- mvarToEvent . _onStopVar $ hostConf
  _onStart     <- mvarToEvent . _onStartVar $ hostConf
  onNewIntent <- mvarToEvent . _onNewIntentVar $ hostConf
  let
    onKill = () <$ ffilter (== "killGonimo") onNewIntent
    _onNewIntent = ffilter (/= "killGonimo") onNewIntent
    _needsNativeHistory = not . _useBrowserHistory $ hostConf
    _onKillRequested =
      fmap (const ()) . ffilter not . tag (current $ conf ^. appKillMask) $ onKill

  let host' = Host {..}

  quitAppDelayed host' conf

  -- Kill on Kill request ...
  killRequested <- hold False $ True <$ onKill
  let
    onDirectQuit =
      ffilter id . tag (current $ conf ^. appKillMask) $ leftmost [conf ^. onKillApp, onKill]
    onDelayedQuit = gate killRequested . ffilter id $ updated $ conf ^. appKillMask
  performEvent_ $ quitApp <$ leftmost [onDirectQuit, onDelayedQuit]

  setKillable host' conf
  pure host'

-- | Quit the app `onStop` with `quitDelay` if no `onStart` occurred.
quitAppDelayed :: forall t m c.
                  ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
                  , TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
                  , MonadIO (Performable m), HasConfig c
                  )
               => Host t -> c t -> m ()
quitAppDelayed host' conf = do
    now <- liftIO getCurrentTime
    onStartWithTime <- performEvent $ liftIO getCurrentTime <$ host' ^. onStart
    lastStartTime <- hold now $ onStartWithTime

    let
      -- | Forward stop event if later then lastStartTime + quitDelay, otherwise block it.
      --
      --   Also block the event if appKillMask is False.
      filterQuitEvent :: UTCTime -> PushM t (Maybe ())
      filterQuitEvent stopTime = do
        startTime <- sample lastStartTime
        canKill   <- sample $ current $ conf ^. appKillMask
        if diffUTCTime stopTime startTime >= quitDelay && canKill
          then pure $ Just ()
          else pure Nothing

    onDelayedStop <- delay quitDelay $ host' ^. onStop
    onDelayedStopWithTime <- performEvent $ liftIO getCurrentTime <$ onDelayedStop

    performEvent_ $ quitApp <$ push filterQuitEvent onDelayedStopWithTime


-- | Tell OS whether or not it is a good idea to kill the app.
setKillable :: forall t m c.
                  ( Reflex t, MonadHold t m, MonadFix m, MonadJSM m
                  , TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
                  , PostBuild t m
                  , MonadIO (Performable m), HasConfig c
                  )
               => Host t -> c t -> m ()
setKillable host' conf = do
  onKillMask <- tagOnPostBuild $ conf ^. appKillMask

  performEvent_ $ avoidKill <$ ffilter not onKillMask
  performEvent_ $ allowKill <$ ffilter id onKillMask

-- | Call avoidKill on host.
avoidKill :: forall m. MonadJSM m => m ()
avoidKill = void . runMaybeT $ do
  nativeHost' <- getNativeHost
  liftJSM $ nativeHost' ^. JS.js0 ("avoidKill" :: Text)

-- | Call allowKill on host.
allowKill :: forall m. MonadJSM m => m ()
allowKill = void . runMaybeT $ do
  nativeHost' <- getNativeHost
  liftJSM $ nativeHost' ^. JS.js0 ("allowKill" :: Text)

-- | Quit the application, if the host supports it:
quitApp :: forall m. MonadJSM m => m ()
quitApp = void . runMaybeT $ do
  nativeHost' <- getNativeHost
  liftJSM $ nativeHost' ^. JS.js0 ("killApp" :: Text)

-- | Get access to the native host JS object, if available.
getNativeHost :: forall m. (MonadJSM m, MonadPlus m) => m JS.JSVal
getNativeHost = do
    win    <- liftJSM $ JS.jsg ("window"    :: Text)
    mfromMaybe =<< (liftJSM $ JS.maybeNullOrUndefined =<< win JS.! nativeHost)
  where
    mfromMaybe ::Maybe a -> m a
    mfromMaybe = maybe mzero return

-- | Get an Event from an MVar ..
--
--   Trigger an Event whenever the MVar gets filled.
mvarToEvent :: (Reflex t, MonadIO m, TriggerEvent t m)
              => MVar a -> m (Event t a)
mvarToEvent var' = do
    (onEvent, triggerEv) <- newTriggerEvent
    run (triggerEv =<< takeMVar var')
    pure onEvent
  where
    run = liftIO . void . forkIO . forever

-- Auto generated lenses:

-- Lenses for HostVars:

onStartVar :: Lens' HostVars (MVar ())
onStartVar f hostVars' = (\onStartVar' -> hostVars' { _onStartVar = onStartVar' }) <$> f (_onStartVar hostVars')

onStopVar :: Lens' HostVars (MVar ())
onStopVar f hostVars' = (\onStopVar' -> hostVars' { _onStopVar = onStopVar' }) <$> f (_onStopVar hostVars')

onNewIntentVar :: Lens' HostVars (MVar Intent)
onNewIntentVar f hostVars' = (\onNewIntentVar' -> hostVars' { _onNewIntentVar = onNewIntentVar' }) <$> f (_onNewIntentVar hostVars')

useBrowserHistory :: Lens' HostVars Bool
useBrowserHistory f hostVars' = (\useBrowserHistory' -> hostVars' { _useBrowserHistory = useBrowserHistory' }) <$> f (_useBrowserHistory hostVars')


