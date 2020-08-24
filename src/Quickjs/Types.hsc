{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Quickjs.Types where

import qualified Data.Map                  as Map
import           Data.Bits                 (Bits)
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


data JSTagEnum = JSTagFirst
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
  deriving (Show, Eq)
  
instance Num a => ToCType JSTagEnum a where
  toCType JSTagFirst            = #{const JS_TAG_FIRST}
  toCType JSTagBigDecimal       = #{const JS_TAG_BIG_DECIMAL}
  toCType JSTagBigInt           = #{const JS_TAG_BIG_INT}
  toCType JSTagBigFloat         = #{const JS_TAG_BIG_FLOAT}
  toCType JSTagSymbol           = #{const JS_TAG_SYMBOL}
  toCType JSTagString           = #{const JS_TAG_STRING}
  toCType JSTagModule           = #{const JS_TAG_MODULE}
  toCType JSTagFunctionBytecode = #{const JS_TAG_FUNCTION_BYTECODE}
  toCType JSTagObject           = #{const JS_TAG_OBJECT}
  toCType JSTagInt              = #{const JS_TAG_INT}
  toCType JSTagBool             = #{const JS_TAG_BOOL}
  toCType JSTagNull             = #{const JS_TAG_NULL}
  toCType JSTagUndefined        = #{const JS_TAG_UNDEFINED}
  toCType JSTagUninitialized    = #{const JS_TAG_UNINITIALIZED}
  toCType JSTagCatchOffset      = #{const JS_TAG_CATCH_OFFSET}
  toCType JSTagException        = #{const JS_TAG_EXCEPTION}
  toCType JSTagFloat64          = #{const JS_TAG_FLOAT64}


instance (Eq a, Num a) => FromCType JSTagEnum a where
  fromCType (#{const JS_TAG_BIG_DECIMAL}) = Just JSTagBigDecimal
  fromCType (#{const JS_TAG_BIG_INT}) = Just JSTagBigInt
  fromCType (#{const JS_TAG_BIG_FLOAT}) = Just JSTagBigFloat
  fromCType (#{const JS_TAG_SYMBOL}) = Just JSTagSymbol
  fromCType (#{const JS_TAG_STRING}) = Just JSTagString
  fromCType (#{const JS_TAG_MODULE}) = Just JSTagModule
  fromCType (#{const JS_TAG_FUNCTION_BYTECODE}) = Just JSTagFunctionBytecode
  fromCType (#{const JS_TAG_OBJECT}) = Just JSTagObject
  fromCType (#{const JS_TAG_INT}) = Just JSTagInt
  fromCType (#{const JS_TAG_BOOL}) = Just JSTagBool
  fromCType (#{const JS_TAG_NULL}) = Just JSTagNull
  fromCType (#{const JS_TAG_UNDEFINED}) = Just JSTagUndefined
  fromCType (#{const JS_TAG_UNINITIALIZED}) = Just JSTagUninitialized
  fromCType (#{const JS_TAG_CATCH_OFFSET}) = Just JSTagCatchOffset
  fromCType (#{const JS_TAG_EXCEPTION}) = Just JSTagException 
  fromCType (#{const JS_TAG_FLOAT64}) = Just JSTagFloat64
  fromCType _ = Nothing

data JSTypeEnum = JSTypeFromTag JSTagEnum
                | JSIsNumber
                | JSIsArray
                | JSIsDate
                | JSIsError
  deriving Show

data JSEvalType = Global | Module

  
instance Num a => ToCType JSEvalType a where
  toCType Global = #{const JS_EVAL_TYPE_GLOBAL}
  toCType Module = #{const JS_EVAL_TYPE_MODULE}


newtype JSGPNMask = JSGPNMask { unJSGPNMask :: CInt }
  deriving (Eq, Bits)

#{enum JSGPNMask, JSGPNMask
, jsGPNStringMask  = JS_GPN_STRING_MASK
, jsGPNSymbolMask  = JS_GPN_SYMBOL_MASK
, jsGPNPrivateMask = JS_GPN_PRIVATE_MASK
, jsGPNEnumOnly    = JS_GPN_ENUM_ONLY
, jsGPNSetEnum     = JS_GPN_SET_ENUM
}


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

