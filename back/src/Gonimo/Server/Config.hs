{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Gonimo.Server.Config
Description : Server configuration.
Copyright   : (c) Robert Klotzner, 2017

All things needed for server operation are contained in 'Config' in addition
their are some helper functions doing basic operations based on this config.
-}
module Gonimo.Server.Config ( -- * Types and classes
                              Config(..)
                            , HasConfig(..)
                            -- * Functions
                            , runDb
                            , mayRunDb
                            , generateSecret
                            ) where


import           Control.Lens
import           Control.Logging.Extended
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                     (Text)
import           Database.Persist.Sql          (SqlBackend, runSqlPool)

import           Gonimo.Server.Config.Internal
import           Gonimo.SocketAPI.Model        (Secret(..))

-- | Log messages from this module will be tagged with this string.
logSource :: Text
logSource = "Server.Config"

runDb :: (MonadIO m, HasConfig c) => c -> ReaderT SqlBackend IO a -> m a
runDb c = liftIO . flip runSqlPool (c^.dbPool)

-- | Run a Db transaction.
--
--   In case of an exception, it gets logged and 'mayRunDb' will return 'Nothing'.
mayRunDb :: forall m c a. (MonadIO m, HasConfig c) => c -> ReaderT SqlBackend IO a -> m (Maybe a)
mayRunDb c = liftIO . logExceptionS logSource "runDb failed" . runDb c

-- | What length do our secrets need?
secretLength :: Int
secretLength = 16

-- | Generate a new random secret for usage as 'AuthToken' for example.
generateSecret :: (MonadIO m, HasConfig c) => c -> m Secret
generateSecret c = Secret <$> genRandomBytes c secretLength

