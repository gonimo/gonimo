module Gonimo.Server.InitDb where


import           Prelude        hiding (words, unwords, lines)
import           Data.Text      hiding (count, map)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)

import           Gonimo.Server.Types
import           Gonimo.Server.DbEntities
import           Database.Persist.Sqlite
import           Servant.Subscriber
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad

import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import           Gonimo.Server
import           Gonimo.Server.DbEntities


-- TODO: use cabal's data-files instead of optimism to find the file there.
pathOfFunnyWordsPreset :: FilePath
pathOfFunnyWordsPreset = "data/funnywords.txt"


-- If the FunnyWord table is empty, populate it with data.
-- TODO: consider taking a more refined migration approach one day.
initDb :: ReaderT SqlBackend IO ()
initDb = do
  funnyWordCount <- count ([] :: [Filter FunnyWord])
  when (funnyWordCount == 0)
    populateFunnyWords


populateFunnyWords :: ReaderT SqlBackend IO ()
populateFunnyWords = do
  funnyWordsPreset <- liftIO (readFile pathOfFunnyWordsPreset)
  let funnyWords = parseFunnyWordPreset (pack funnyWordsPreset)
  insertMany_ funnyWords


parseFunnyWordPreset :: Text -> [FunnyWord]
parseFunnyWordPreset =
  let lineToWord line = partsToWord (words line)
      partsToWord (w:ws) =
        FunnyWord {
          funnyWordWord     = unwords ws,
          funnyWordWordType = read (unpack w)
        }
  in  map lineToWord . lines
