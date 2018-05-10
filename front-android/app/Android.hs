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
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.UTF8          as BS
import           Data.Default
import           Data.Monoid
import           Data.String
import           Language.Javascript.JSaddle   (JSM)
import           Network.HTTP.Types            (urlDecode)
import           Reflex.Dom.Android.MainWidget
import           System.IO

import qualified Gonimo.Client.Main            as Gonimo
import           Gonimo.Types                  ()

main :: IO ()
main = do
  conf <- (\c -> c { Gonimo._useBrowserHistory = False}) <$> Gonimo.mkEmptyConfig
  let
    handleIntent :: String -> IO ()
    handleIntent url = do
      putStrLn $ "gonimo, handling intent: " <> url
      let
        query = dropWhile (/= '?') url
        encodedSecret = drop 1 . dropWhile (/= '=') $ query
        urlJson = BS.fromString encodedSecret
        json = urlDecode True urlJson
      case Aeson.decodeStrict json of
        Just val
          -> putMVar (conf ^. Gonimo.newInvitation) val
        Nothing
          -> putStrLn $ "gonimo: Warning: Received intentent that could not be handled: '" <> url <> "'"

  runWithIntent handleIntent $ Gonimo.main conf

runWithIntent :: (String -> IO ()) -> JSM () -> IO ()
runWithIntent handleIntent jsm = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  continueWithCallbacks $ def
    { _activityCallbacks_onCreateWithIntent = \_ intent intentData -> do
        a <- getHaskellActivity
        let startPage = fromString $ "file:///index.html" ++ dropWhile (/= '?') intentData
        putStrLn $ "gonimo:  intent: " <> intent
        putStrLn $ "gonimo: intent data: " <> intentData
        startMainWidget a startPage jsm
    , _activityCallbacks_onNewIntent = \_ intentData -> handleIntent intentData
    }
  forever $ threadDelay 1000000000


-- setSearchIntent :: String -> JSM ()
-- setSearchIntent uri = do
--   let query = dropWhile (/= '?') uri
--   window  <- DOM.currentWindowUnchecked
--   location <- Window.getLocation window
--   Location.setSearch location query
--   liftIO $ threadDelay 1000000
