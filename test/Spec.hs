{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad.Catch     (try, SomeException, MonadCatch(..))
import           Data.Text               (pack)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.Aeson.Key             as K
import qualified Data.Vector             as V
import           Quickjs
import Test.HUnit (assertFailure)
import Quickjs.Error (SomeJSRuntimeException)
import Control.Monad(guard)
import qualified Data.Text as T

eval_1_plus_2 :: Assertion
eval_1_plus_2 = quickjsMultithreaded $ do
  v <- eval "1 + 2;"
  liftIO $ v @?= Number 3


eval_throw :: Assertion
eval_throw = quickjsMultithreaded $
  try (eval "throw 'Error'") >>= \case
    Left (_ :: SomeJSRuntimeException) -> return ()
    Right _ -> liftIO $ assertFailure "should fail with an Exception..."

genText  = do
  k <- QC.choose (0,200)
  t <- pack <$> QC.vectorOf k (QC.oneof $ map pure $ ['0'..'~'])
  pure t


genVal 0 = QC.oneof
  [
    String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
genVal n | n > 0 = QC.oneof
  [
    do { k <- QC.choose (0,n) ; Object . KM.fromList <$> (zip <$> QC.vectorOf k (K.fromText <$> genText) <*> QC.vectorOf k genVal') }
  , do { k <- QC.choose (0,n) ; Array . V.fromList <$> QC.vectorOf k genVal' }
  , String <$> genText
  , Number . fromInteger <$> QC.arbitrary
  , Bool <$> QC.arbitrary
  , pure Null
  ]
  where genVal' = genVal (n `div` 2)

-- | There's an Arbitrary instance for Value floating around, but our tests don't pass
--   with it because it produces null characters in text. These are not valid
--   javascript strings, so we have our own.
newtype GenVal = GenVal Value deriving Show
instance QC.Arbitrary GenVal where
  arbitrary = GenVal <$> QC.sized genVal

marshall_to_from_JSValue :: GenVal -> QC.Property
marshall_to_from_JSValue (GenVal val) = QC.monadicIO $ do
  val' <- QC.run $ quickjsMultithreaded $ withJSValue val $ \jsval ->
    fromJSValue_ jsval
  pure $ (val QC.=== val')

tests :: TestTree
tests =
  -- adjustOption (\_ -> QuickCheckTests 10) $
  -- adjustOption (\_ -> QuickCheckVerbose True) $
  testGroup "Quickjs"
    [ testCase "empty quickjs call" (quickjsMultithreaded $ pure ())
    , testCase "eval '1 + 2;'" eval_1_plus_2
    , testCase "eval throw" eval_throw
    , testProperty "marshalling Value to JSValue and back" marshall_to_from_JSValue
    ]

main = defaultMain tests
