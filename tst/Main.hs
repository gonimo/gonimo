{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Test.Tasty
import           Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit
import           Test.Tasty.Hspec

import Data.Maybe (isJust, isNothing)
import Control.Exception

main :: IO ()
main = do hspecTests <- hSpec
          let unitTests = testGroup "Properties" [hunitTests, hspecTests]
          defaultMain $ testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ testGroup "testGroup1"
      [ QC.testProperty "ok" $ \b ->
          (not . not) b == (b :: Bool)
      ]
  , testGroup "testGroup2"
      [ QC.testProperty "fail" $ \b ->
          (not . not) b == (not b :: Bool)
      ]
  ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ testGroup "testGroup1"
      [ SC.testProperty "ok" $ \b ->
          (not . not) b == (b :: Bool)
      ]
  , testGroup "testGroup2"
      [ SC.testProperty "fail" $ \b ->
          (not . not) b == (not b :: Bool)
      ]
  ]

hunitTests :: TestTree
hunitTests = testGroup "(checked by HUnit)"
  [ testGroup "testGroup1"
     [testCase "ok" $ True @=? True
     ]
  , testGroup "testGroup2"
    [testCase "fail" $ True @=? False
    ]
  ]
hSpec :: IO TestTree
hSpec = testSpec "(checked by HSpec)"  spec

spec :: Spec
spec = do
    describe "testGroup 1" $ do
        it "ok" $ True `shouldBe` True
    describe "testGroup2"$ do
        it "fail" $ True `shouldBe` False
