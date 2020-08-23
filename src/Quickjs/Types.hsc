{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Quickjs.Types where

import qualified Data.Map as Map
import           GHC.Generics
import           Data.Aeson                (ToJSON(..))
import           Foreign.C.Types
import           Foreign.Ptr               (plusPtr)
import           Foreign.Storable          (Storable(..))
import qualified Language.Haskell.TH       as TH
import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types          as C

#include "quickjs.h"

data JSValue = JSValue
  { u :: {-# UNPACK #-} !CDouble
  , tag :: {-# UNPACK #-} !CLong
  } deriving (Show, Eq)

instance Storable JSValue where
  sizeOf _ = #{size JSValue}
  alignment _ = #{alignment JSValue}
  peek ptr = do
    u <- peek (#{ptr JSValue, u} ptr)
    tag <- peek (#{ptr JSValue, tag} ptr)
    Prelude.pure (JSValue u tag)
  poke ptr (JSValue u tag) = do
    poke (#{ptr JSValue, u} ptr) u
    poke (#{ptr JSValue, tag} ptr) tag

type JSValueConst = JSValue

newtype JSRuntime = JSRuntime { _unusedRuntime :: CUChar }

newtype JSContext = JSContext { _unusedContext :: CUChar }


type JSBool = CInt

type JSAtom = CUInt


data JSPropertyEnum = JSPropertyEnum
  { is_enumerable :: {-# UNPACK #-} !JSBool
  , atom :: {-# UNPACK #-} !JSAtom
  } deriving (Show, Eq)

instance Storable JSPropertyEnum where
  sizeOf _ = #{size JSPropertyEnum}
  alignment _ = #{alignment JSPropertyEnum}
  peek ptr = do
    is_enumerable <- peek (#{ptr JSPropertyEnum, is_enumerable} ptr)
    atom <- peek (#{ptr JSPropertyEnum, atom} ptr)
    Prelude.pure (JSPropertyEnum is_enumerable atom)
  poke ptr (JSPropertyEnum is_enumerable atom) = do
    poke (#{ptr JSPropertyEnum, is_enumerable} ptr) is_enumerable
    poke (#{ptr JSPropertyEnum, atom} ptr) atom



data JSRefCountHeader = JSRefCountHeader
  { ref_count :: {-# UNPACK #-} !CInt
  } deriving (Show, Eq)

instance Storable JSRefCountHeader where
  sizeOf _ = #{size JSRefCountHeader}
  alignment _ = #{alignment JSRefCountHeader}
  peek ptr = do
    ref_count <- peek (#{ptr JSRefCountHeader, ref_count} ptr)
    Prelude.pure (JSRefCountHeader ref_count)
  poke ptr (JSRefCountHeader ref_count) = do
    poke (#{ptr JSRefCountHeader, ref_count} ptr) ref_count



class ToCType ty cty where
  toCType :: ty -> cty


class FromCType ty cty where
  fromCType :: cty -> Maybe ty


data JSTagEnum =  JSTagFirst
                | JSTagBigDecimal
                | JSTagBigInt
                | JSTagBigFloat
                | JSTagSymbol
                | JSTagString
                | JSTagModule
                | JSTagFunctionBytecode
                | JSTagObject
                | JSTagInt
                | JSTagBool
                | JSTagNull
                | JSTagUndefined
                | JSTagUninitialized
                | JSTagCatchOffset
                | JSTagException
                | JSTagFloat64
  deriving (Show, Eq, Generic, ToJSON)
  
instance Num a => ToCType JSTagEnum a where
  toCType JSTagFirst            = -11
  toCType JSTagBigDecimal       = -11
  toCType JSTagBigInt           = -10
  toCType JSTagBigFloat         = -9
  toCType JSTagSymbol           = -8
  toCType JSTagString           = -7 
  toCType JSTagModule           = -3 
  toCType JSTagFunctionBytecode = -2
  toCType JSTagObject           = -1
  toCType JSTagInt              = 0  
  toCType JSTagBool             = 1 
  toCType JSTagNull             = 2    
  toCType JSTagUndefined        = 3  
  toCType JSTagUninitialized    = 4 
  toCType JSTagCatchOffset      = 5
  toCType JSTagException        = 6
  toCType JSTagFloat64          = 7


instance (Eq a, Num a) => FromCType JSTagEnum a where
  fromCType (-11) = Just JSTagBigDecimal
  fromCType (-10) = Just JSTagBigInt
  fromCType  (-9) = Just JSTagBigFloat
  fromCType  (-8) = Just JSTagSymbol
  fromCType  (-7) = Just JSTagString
  fromCType  (-3) = Just JSTagModule
  fromCType  (-2) = Just JSTagFunctionBytecode
  fromCType  (-1) = Just JSTagObject
  fromCType   0 = Just JSTagInt
  fromCType   1 = Just JSTagBool
  fromCType   2 = Just JSTagNull
  fromCType   3 = Just JSTagUndefined
  fromCType   4 = Just JSTagUninitialized
  fromCType   5 = Just JSTagCatchOffset
  fromCType   6 = Just JSTagException 
  fromCType   7 = Just JSTagFloat64
  fromCType   _ = Nothing

data JSTypeEnum = JSTypeFromTag JSTagEnum
                | JSIsNumber
                | JSIsArray
                | JSIsDate
                | JSIsError
  deriving Show

instance ToJSON JSTypeEnum where
  toJSON (JSTypeFromTag t) = toJSON t
  toJSON JSIsNumber = toJSON ("JSIsNumber" :: String)
  toJSON JSIsArray = toJSON ("JSIsArray" :: String)
  toJSON JSIsDate = toJSON ("JSIsDate" :: String)
  toJSON JSIsError = toJSON ("JSIsError" :: String)


data JSEvalType = Global | Module

  
instance Num a => ToCType JSEvalType a where
  toCType Global = 0
  toCType Module = 1


data JSGPNMask = JSGPNStringMask | JSGPNSymbolMask | JSGPNPrivateMask | JSGPNEnumOnly | JSGPNSetEnum
  -- deriving (Num, Eq, Bits)

instance Num a => ToCType JSGPNMask a where
  toCType JSGPNStringMask = 1
  toCType JSGPNSymbolMask = 2
  toCType JSGPNPrivateMask = 4
  toCType JSGPNEnumOnly = 16
  toCType JSGPNSetEnum = 32


quickjsCtx :: Context
quickjsCtx = baseCtx <> fptrCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = quickjsTypesTable
      }

quickjsTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
quickjsTypesTable = Map.fromList
  [ 
    (C.TypeName "JSValue", [t| JSValue |])
  , (C.TypeName "JSValueConst", [t| JSValueConst |])
  , (C.TypeName "JSRuntime", [t| JSRuntime |])
  , (C.TypeName "JSContext", [t| JSContext |])
  , (C.TypeName "JSBool", [t| JSBool |])
  , (C.TypeName "JSAtom", [t| JSAtom |])
  , (C.TypeName "JSPropertyEnum", [t| JSPropertyEnum |])
  ]

