{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}


import           Android.HaskellActivity
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Default
import           Data.Monoid
import           Data.String
import           Language.Javascript.JSaddle   (JSM)
import           Network.HTTP.Types            (urlDecode)
import           Reflex.Dom.Android.MainWidget
import           System.IO
import           Control.Concurrent.MVar (putMVar)

import qualified Gonimo.Client.Main            as Gonimo
import           Gonimo.Types                  ()

main :: IO ()
main = do
  conf <- Gonimo.mkEmptyConfig
  let
    handleIntent url = do
      let
        query = dropWhile (/= '?') intentData
        encodedSecret = dropWhile (/= '=') query
        urlJson = BS.fromString encodedSecret
        jsonStrict = urlDecode True
        json = BL.fromStrict jsonStrict
      case Aeson.decode json of
        Just val -> do
          putMVar (conf ^. newInvitation) val
        Nothing -> do
          putStrLn "gonimo: Warning: Received intentent that could not be handled: '" <> url <> "'"

  runWithIntent handleIntent Gonimo.main

runWithIntent :: (String -> JSM) -> JSM () -> IO ()
runWithIntent handleIntent jsm = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreateWithIntent = \_ intent intentData -> do
        a <- getHaskellActivity
        let startPage = fromString $ "file:///android_asset/index.html" ++ dropWhile (/= '?') intentData
        putStrLn $ "gonimo:  intent: " <> intent
        putStrLn $ "gonimo: intent data: " <> intentData
        startMainWidget a startPage jsm
    , _activityCallbacks_onNewIntent = \intent intentData -> handleIntent intentData
    }
  forever $ threadDelay 1000000000


-- setSearchIntent :: String -> JSM ()
-- setSearchIntent uri = do
--   let query = dropWhile (/= '?') uri
--   window  <- DOM.currentWindowUnchecked
--   location <- Window.getLocation window
--   Location.setSearch location query
--   liftIO $ threadDelay 1000000
