module Gonimo.Server.InitDb where


import           Prelude        hiding (words, unwords, lines, readFile)
import           Data.Text      hiding (count, map, concat)
import           Data.Text.IO

import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import           Database.Persist.Sqlite
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.IO.Class     (liftIO)

import           Paths_gonimo_back


funnyWordsDataFile :: FunnyWordType -> FilePath
funnyWordsDataFile typ =
  "data/" ++ show typ ++ ".txt"


-- Delete the FunnyWord table and refill it with our preset.
initDb :: ReaderT SqlBackend IO ()
initDb = do
  deleteWhere ([] :: [Filter FunnyWord])
  populateFunnyWords


populateFunnyWords :: ReaderT SqlBackend IO ()
populateFunnyWords =
  let types                 = [FunnyPrefix, FunnyCharacter, FunnySuffix]
      lineToRecord typ line = FunnyWord {
                                funnyWordWord = line,
                                funnyWordWordType = typ
                              }
      readPreset typ        = map (lineToRecord typ) . lines <$> do
                                fileName <- getDataFileName (funnyWordsDataFile typ)
                                readFile fileName
  in  do
        funnyWords   <- liftIO $ concat <$> mapM readPreset types
        insertMany_     funnyWords
