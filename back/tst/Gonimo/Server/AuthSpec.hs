{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# ANN module "HLint: ignore Redundant do" #-}
module Gonimo.Server.AuthSpec where

import           Gonimo.Server.Auth
import           Gonimo.Util

import           Servant.Server
import           Test.Hspec (Spec, describe, it )
{-import           Test.Hspec-}
import           Test.Hspec.Expectations.Pretty
import           Test.QuickCheck
import           Test.Utils
import           Test.DBSetup

spec :: Spec
spec = describe "test authorizations"
          groupAuthorize

groupAuthorize :: Spec
groupAuthorize = do
  describe "authorize" $ do
    it "should `return ()` on success." $
      testExceptEff (authorize ('0'==) '0') `shouldReturn` ()

    {-it "should `throw err403` on fail." $-}
      {-testExceptEff (authorize ('0'==) '1') `shouldThrow` (== ServantException err403)-}

  describe "authorizeJust" $ do
    it "should `return x` on `Just x`." $
      testExceptEff (authorizeJust Just 'x') `shouldReturn` 'x'

    {-it "should `throw err403` on fail." $-}
      {-testExceptEff (authorizeJust (const Nothing) 'x') `shouldThrow` (== ServantException err403)-}

  describe "authorizeAuthData" $
    it "should `return ()` on success." $ do
        ad <- setupDB
        testReaderExceptEff (authorizeAuthData (const True)) ad `shouldReturn` ()

