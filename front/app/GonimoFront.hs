{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

#ifndef ghcjs_HOST_OS
import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets
import           Network.Wai.Middleware.Static
#endif

import qualified Gonimo.Client.Main as Gonimo

-- import qualified GHCJS.DOM.Types as JS


main :: IO ()
-- main = run 3709 $ mainWidget app
main = gonimoRun 3709 $ Gonimo.main


#ifdef ghcjs_HOST_OS
gonimoRun :: Int -> IO () -> IO ()
gonimoRun _port = id
#else
gonimoRun :: Int -> JSM () -> IO ()
gonimoRun port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) gonimoApp
  where
    gonimoApp = staticPolicy (addBase "devRoot" <|> addSlash) jsaddleApp
#endif
