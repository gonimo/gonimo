module Gonimo.Server.InitDb where


import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)

import           Gonimo.Server.Types
import           Gonimo.Server.DbEntities
import           Database.Persist.Sqlite
import           Servant.Subscriber
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad (when)

import           Gonimo.Server.DbEntities
import           Gonimo.Server.Types
import           Gonimo.Server
import           Gonimo.Server.DbEntities


-- If the FunnyWord table is empty, populate it with data.
-- TODO: consider taking a more refined migration approach.
initDb :: ReaderT SqlBackend IO ()
initDb = do
  funnyWordCount <- count ([] :: [Filter FunnyWord])
  when (funnyWordCount == 0)
    populateFunnyWords


populateFunnyWords :: ReaderT SqlBackend IO ()
populateFunnyWords = do
  -- TODO: populate data from a file instead
  let funnyWords = [
        FunnyWord { funnyWordWord = "didldidumpty", funnyWordWordType = FunnyPrefix1 },
        FunnyWord { funnyWordWord = "woopidoo",     funnyWordWordType = FunnyPrefix1 },
        FunnyWord { funnyWordWord = "krawoo",       funnyWordWordType = FunnyPrefix1 }]
  insertMany_ funnyWords
