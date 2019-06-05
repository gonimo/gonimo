{-# LANGUAGE CPP #-}
import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend

#if defined(ANDROID)

import           Reflex.Dom.Android.MainWidget
import           Android.HaskellActivity
import           Control.Concurrent
import           Control.Concurrent.MVar       (putMVar)
import           Data.Default
import           Data.Monoid
import           Data.String
import qualified Data.Text                     as T
import           System.IO
import           Control.Lens
import           Control.Monad

#else

import Reflex.Dom

#endif

import qualified Gonimo.Client.Host.Impl       as Gonimo

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
-- TODO: This #if defined does not seem to work as expected. (False on Android - yeah - we need to define it in cabal file!
#if defined (ANDROID)
  hostVars <- (\c -> c { Gonimo._useBrowserHistory = False}) <$> Gonimo.makeEmptyHostVars
  let
    handleIntent :: String -> IO ()
    handleIntent = putMVar (hostVars ^. Gonimo.onNewIntentVar) . T.pack

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreateWithIntent = \_ intent intentData -> do
        a <- getHaskellActivity
        let startPage = fromString $ "file:///index.html" ++ dropWhile (/= '?') intentData
        putStrLn $ "gonimo:  intent: " <> intent
        putStrLn $ "gonimo: intent data: " <> intentData
        startMainWidget a startPage $
          runFrontend validFullEncoder $ makeFrontend hostVars
    , _activityCallbacks_onNewIntent = \_ intentData -> handleIntent intentData
    , _activityCallbacks_onStart     = putMVar (hostVars ^. Gonimo.onStartVar) ()
    , _activityCallbacks_onStop      = putMVar (hostVars ^. Gonimo.onStopVar) ()
    }
  forever $ threadDelay 1000000000
#else
  hostVars <- Gonimo.makeEmptyHostVars
  run $ runFrontend validFullEncoder $ makeFrontend hostVars
#endif
