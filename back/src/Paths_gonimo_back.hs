-- | Find Paths during development.

module Paths_gonimo_back where

import           Control.Exception
import           System.Directory
import           System.FilePath   (splitFileName, (</>))

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = do
  wd <- getCurrentDirectory
  let (_, fileName) = splitFileName wd
  case fileName of
    "back"   -> pure f
    "gonimo" -> pure $ "back" </> f
    _        -> throwIO  . userError $ "Warning, gonimo-back hast to be started either from the gonimo or the gonimo/back folder!"
