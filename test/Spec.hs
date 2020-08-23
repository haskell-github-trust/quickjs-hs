{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty              (TestTree, testGroup, defaultMain, after, DependencyType(..))
import           Test.Tasty.HUnit        (testCase)
import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=))
import qualified Test.QuickCheck         as QC
import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Catch     (SomeException, MonadCatch(..))
import           Data.Text               (pack)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Vector             as V
import           Quickjs


eval_1_plus_2 :: Assertion
eval_1_plus_2 = quickjs $ do
  v <- eval "1 + 2;"
  liftIO $ v @?= Number 3

genText = do 
  k <- QC.choose (0,200) 
  pack <$> QC.vectorOf k (QC.oneof $ map pure $ ['0'..'~'])

genVal 0 = QC.oneof
  [ 
    String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
genVal n | n > 0 = QC.oneof
  [ 
    do { k <- QC.choose (0,n) ; Object . HM.fromList <$> (zip <$> QC.vectorOf k genText <*> QC.vectorOf k genVal') }      
  , do { k <- QC.choose (0,n) ; Array . V.fromList <$> QC.vectorOf k genVal' }
  , String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
  where genVal' = genVal (n `div` 2)

instance QC.Arbitrary Value where
  arbitrary = QC.sized genVal




marshall_to_from_JSValue :: Value -> QC.Property    
marshall_to_from_JSValue val = QC.monadicIO $ do
  val' <- QC.run $ quickjs $ withJSValue val $ \jsval ->
    fromJSValue_ jsval
  QC.assert $ val == val'

tests :: TestTree
tests = 
  -- adjustOption (\_ -> QuickCheckTests 10) $
  -- adjustOption (\_ -> QuickCheckVerbose True) $  
  testGroup "Quickjs"
    [ testCase "empty quickjs call" (quickjs $ pure ())
    , after AllSucceed "empty quickjs call" $ testCase "eval '1 + 2;'" eval_1_plus_2
    , after AllSucceed "eval" $ testProperty "marshalling Value to JSValue and back" marshall_to_from_JSValue
    ]

main = defaultMain tests