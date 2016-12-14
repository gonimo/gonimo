module Gonimo.Server.InitDb where


import           Data.Text      (Text)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           Data.Maybe     (mapMaybe)
import           Gonimo.Server.Effects (FamilyName, FamilyNames, Predicates)

import           Gonimo.Server.Db.Entities
import           Gonimo.Server.Types
import           Database.Persist.Sqlite
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.IO.Class     (liftIO)

import           Paths_gonimo_back
import           System.IO.Error

loadFamilies :: IO FamilyNames
loadFamilies = do
  fileName <- getDataFileName "data/families"
  let
    parseList :: [Text] -> Maybe FamilyName
    parseList [ mN, fN ] = Just $ FamilyName mN fN
    parseList _ = Nothing
  let parseLine = parseList . map T.strip . T.splitOn ","
  let parse = mapMaybe parseLine . T.lines
  V.fromList . parse <$> T.readFile fileName


loadPredicates :: IO Predicates
loadPredicates = do
  fileName <- getDataFileName "data/predicates"
  let parse = map T.strip . T.lines
  V.fromList . parse <$> T.readFile fileName
