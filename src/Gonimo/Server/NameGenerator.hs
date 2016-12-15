module Gonimo.Server.NameGenerator ( loadFamilies
                                   , loadPredicates
                                   , generateFamilyName
                                   , generateDeviceName
                                   , makeDeviceName
                                   , getRandomVectorElement
                                   ) where

import           System.Random   (getStdRandom, randomR)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text      (Text)
import           Data.Monoid
import           Data.Vector      (Vector, (!))
import qualified Data.Vector      as V
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           Gonimo.Server.Types (FamilyName(..), FamilyNames, Predicates, parseFamilyName)

import           Paths_gonimo_back

loadFamilies :: IO FamilyNames
loadFamilies = do
  fileName <- getDataFileName "data/families.txt"
  let parse = map parseFamilyName . T.lines
  V.fromList . parse <$> T.readFile fileName


loadPredicates :: IO Predicates
loadPredicates = do
  fileName <- getDataFileName "data/predicates.txt"
  let parse = map T.strip . T.lines
  V.fromList . parse <$> T.readFile fileName

generateFamilyName :: MonadIO m => Predicates -> FamilyNames -> m FamilyName
generateFamilyName predicates familyNames = do
  predicate <- getRandomVectorElement predicates
  fName <- getRandomVectorElement familyNames
  pure $ fName { familyName = predicate <> " " <> familyName fName }

generateDeviceName :: MonadIO m => Predicates -> FamilyName -> m Text
generateDeviceName predicates f = do
  predicate <- getRandomVectorElement predicates
  pure $ makeDeviceName predicate f

makeDeviceName :: Text -> FamilyName -> Text
makeDeviceName predicate f = predicate <> " " <> familyMemberName f

-- Internal helper
getRandomVectorElement :: MonadIO m => Vector a -> m a
getRandomVectorElement pool = do
  let upperBound = V.length pool - 1
  (pool !) <$> liftIO (getStdRandom (randomR (0, upperBound)))
