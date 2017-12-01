{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Control.Logging.Extended
Description : Some convenience functions for logging missing from Control.Logging.
Copyright   : (c) Robert Klotzner, 2017

-}
module Control.Logging.Extended ( -- * Re-exported modules
                              module Logging
                            -- * Functions
                            , logExceptionS
                            ) where


import           Control.Arrow
import           Control.Exception
import           Control.Logging   as Logging
import           Control.Monad
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T


type LogSource = Text
type Message = Text

-- | Log any exception at level 'LevelError' and return Nothing.
logExceptionS :: LogSource -> Message -> IO a -> IO (Maybe a)
logExceptionS source msg = (errorToNothing ||| pure . Just) <=< try
  where
    errorToNothing :: SomeException -> IO (Maybe a)
    errorToNothing = fmap (const Nothing) . logException

    logException :: SomeException -> IO ()
    logException = errorS source . makeErrorMsg

    makeErrorMsg = ((msg <> ":\n") <>) . T.pack . displayException

errorS :: LogSource -> Message  -> IO ()
errorS = loggingLogger LevelError
