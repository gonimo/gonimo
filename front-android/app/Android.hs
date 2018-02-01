{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}


import qualified Gonimo.Client.Main as Gonimo
import Reflex.Dom.Android.MainWidget
import Android.HaskellActivity
import Control.Monad
import Control.Concurrent
import Data.String
import System.IO
import Language.Javascript.JSaddle (JSM)
import Data.Monoid
import Data.Default
-- import qualified GHCJS.DOM          as DOM
-- import qualified GHCJS.DOM.Window   as Window
-- import qualified GHCJS.DOM.Location as Location
-- import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = run Gonimo.main

run :: JSM () -> IO ()
run jsm = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreateWithIntent = \_ intent intentData -> do
        a <- getHaskellActivity
        let startPage = fromString $ "file:///android_asset/index.html" ++ dropWhile (/= '?') intentData
        putStrLn $ "gonimo:  intent: " <> intent
        putStrLn $ "gonimo: intent data: " <> intentData
        startMainWidget a startPage jsm
    }
  forever $ threadDelay 1000000000

-- setSearchIntent :: String -> JSM ()
-- setSearchIntent uri = do
--   let query = dropWhile (/= '?') uri
--   window  <- DOM.currentWindowUnchecked
--   location <- Window.getLocation window
--   Location.setSearch location query
--   liftIO $ threadDelay 1000000
