{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets
import           Network.Wai.Middleware.Static
import           System.Directory
import           System.FilePath                        (splitFileName)
import           System.IO

import qualified Gonimo.Client.Host.Impl                as Gonimo
import qualified Gonimo.Client.Main                     as Gonimo


main :: IO ()
-- main = run 3709 $ mainWidget app
main = do
  conf <- Gonimo.makeEmptyHostVars
  gonimoRun 3709 $ Gonimo.main conf


gonimoRun :: Int -> JSM () -> IO ()
gonimoRun port f = do
    checkAndFixCurrentDirectory
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) gonimoApp
  where
    gonimoApp = staticPolicy (addBase "../front/static" <|> addSlash) jsaddleApp


-- Yeah this is a hack ...
-- for convenience so we can run gonimo-front both in gonimo and gonimo/front-warp folders.
-- Copied and adatped from ../back/app/GonimoBack.sh
checkAndFixCurrentDirectory :: IO ()
checkAndFixCurrentDirectory = do
  wd <- getCurrentDirectory
  let (_, fileName) = splitFileName wd
  case fileName of
    "front-warp" -> pure ()
    "gonimo" -> setCurrentDirectory "./front-warp"
    _ -> hPutStrLn stderr "Warning, you have to run gonimo-front-warp from either gonimo or the gonimo/front-warp directory!"
