{-# LANGUAGE ExistentialQuantification, DuplicateRecordFields, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, RecordWildCards #-}

module Quickjs.Error where
import           Control.Exception   (Exception(..), SomeException)
import           Data.Typeable       (cast)
import           Data.Text           (Text)
import           Data.String.Conv    (toS)
import           Type.Reflection     (Typeable)
import           GHC.Generics
import           Foreign.C.Types
import           Data.Aeson          (ToJSON(..))

import           Quickjs.Types


data SomeJSRuntimeException = forall e . Exception e => SomeJSRuntimeException e deriving Typeable

instance Show SomeJSRuntimeException where
    show (SomeJSRuntimeException e) = show e

instance Exception SomeJSRuntimeException where
    toException = jsRuntimeExceptionToException
    fromException = jsRuntimeExceptionFromException


jsRuntimeExceptionToException :: Exception e => e -> SomeException
jsRuntimeExceptionToException = toException . SomeJSRuntimeException

jsRuntimeExceptionFromException :: Exception e => SomeException -> Maybe e
jsRuntimeExceptionFromException x = do
    SomeJSRuntimeException a <- fromException x
    cast a



instance ToJSON CLong where
  toJSON cl = toJSON (fromIntegral cl :: Integer)

data UnknownJSTag = UnknownJSTag {raw_tag :: !CLong} 
  deriving (Generic, Typeable)

instance Exception UnknownJSTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


instance Show UnknownJSTag where
  show UnknownJSTag{..} = "Uknown JS tag: " ++ show raw_tag


data UnsupportedTypeTag = UnsupportedTypeTag {_tag :: JSTagEnum} 
  deriving (Generic, Typeable)

instance Exception UnsupportedTypeTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException

instance Show UnsupportedTypeTag where
  show UnsupportedTypeTag{..} = "Unsupported type tag: " ++ show _tag


data JSException = JSException {location :: Text, message :: Text} 
  deriving (Generic, Typeable)

instance Exception JSException where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


instance Show JSException where
    show JSException{..} = "JS runtime threw an exception in " ++ toS location ++ ":\n=================\n" ++ toS message ++ "\n=================\n"



data JSValueUndefined = JSValueUndefined {value :: Text} 
  deriving (Generic, Typeable)

instance Exception JSValueUndefined where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


instance Show JSValueUndefined where
  show JSValueUndefined{..} =  "The JS value '" ++ toS value ++ "' is undefined."


data JSValueIncorrectType = 
  JSValueIncorrectType {
    name :: Text
  , expected :: JSTypeEnum
  , found :: JSTypeEnum
  } 
  deriving (Generic, Typeable)

instance Exception JSValueIncorrectType where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


instance Show JSValueIncorrectType where
  show JSValueIncorrectType{..} = "Type mismatch of the JS value '" ++ toS name ++ "'. Expected: " ++ show expected ++ ", found: " ++ show found


data InternalError = InternalError { message :: Text } 
  deriving (Generic, Typeable)

instance Exception InternalError where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException

instance Show InternalError where
  show InternalError{..} = "Internal error occured:\n" ++ toS message
