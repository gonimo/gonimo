{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}


import           Android.HaskellActivity
import           Control.Concurrent
import           Control.Concurrent.MVar       (putMVar)
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Monoid
import           Data.String
import qualified Data.Text                     as T
import           Reflex.Dom.Android.MainWidget
import           System.IO

import qualified Gonimo.Client.Host.Impl       as Gonimo
import qualified Gonimo.Client.Main            as Gonimo
import           Gonimo.Types                  ()

main :: IO ()
main = do
  conf <- (\c -> c { Gonimo._useBrowserHistory = False}) <$> Gonimo.makeEmptyHostVars
  let
    handleIntent :: String -> IO ()
    handleIntent = putMVar (conf ^. Gonimo.onNewIntentVar) . T.pack

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreateWithIntent = \_ intent intentData -> do
        a <- getHaskellActivity
        let startPage = fromString $ "file:///index.html" ++ dropWhile (/= '?') intentData
        putStrLn $ "gonimo:  intent: " <> intent
        putStrLn $ "gonimo: intent data: " <> intentData
        startMainWidget a startPage (Gonimo.main conf)
    , _activityCallbacks_onNewIntent = \_ intentData -> handleIntent intentData
    , _activityCallbacks_onStart     = putMVar (conf ^. Gonimo.onStartVar) ()
    , _activityCallbacks_onStop      = putMVar (conf ^. Gonimo.onStopVar) ()
    }
  forever $ threadDelay 1000000000


-- setSearchIntent :: String -> JSM ()
-- setSearchIntent uri = do
--   let query = dropWhile (/= '?') uri
--   window  <- DOM.currentWindowUnchecked
--   location <- Window.getLocation window
--   Location.setSearch location query
--   liftIO $ threadDelay 1000000
