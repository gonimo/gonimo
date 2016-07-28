module Gonimo.Server.InitDb where


import           Prelude        hiding (words, unwords, lines, readFile)
import           Data.Text      hiding (count, map)
import           Data.Text.IO

import           Gonimo.Server.DbEntities
import           Database.Persist.Sqlite
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.IO.Class     (liftIO)

import           Paths_gonimo_back


-- TODO: use cabal's data-files instead of optimism to find the file there.
pathOfFunnyWordsPreset :: IO FilePath
pathOfFunnyWordsPreset = getDataFileName "data/funnywords.txt"


-- Delete the FunnyWord table and refill it with our preset.
initDb :: ReaderT SqlBackend IO ()
initDb = do
  deleteWhere ([] :: [Filter FunnyWord])
  populateFunnyWords


populateFunnyWords :: ReaderT SqlBackend IO ()
populateFunnyWords = do
  funnyWordsPreset <- liftIO $ pathOfFunnyWordsPreset >>= readFile
  let funnyWords    = parseFunnyWordPreset funnyWordsPreset
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
