{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Utils where

import           Control.Exception             (SomeException, throw)
import           Control.Monad.Freer           (Eff, run)
import           Control.Monad.Freer.Exception (Exc, runError)
import           Control.Monad.Freer.Reader    (Reader, runReader)

testExceptEff :: Eff '[Exc SomeException] a -> IO a
testExceptEff = either throw return . run . runError

testReaderExceptEff :: Eff '[Reader r, Exc SomeException] a -> r -> IO a
testReaderExceptEff eff env = either throw return $ run . runError $ runReader eff env

