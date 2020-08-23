{-# LANGUAGE BangPatterns, QuasiQuotes, TemplateHaskell, OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

{-|
Module      : Quickjs
Description : Haskell bindings to the [QuickJS](https://bellard.org/quickjs/) library
Copyright   : (c) Samuel Balco, 2020
License     : MIT
Maintainer  : sam@definitelynotspam.email

This is a very basic wrapper for the [QuickJS](https://bellard.org/quickjs/) .

The current functionality includes evaluating JS code, calling a JS function in the global scope
and marshalling 'Value's to and from 'JSValue's.
-}
module Quickjs (JSValue, JSContextPtr, quickjs, call, eval, eval_, withJSValue, fromJSValue_) where

import           Foreign
import           Foreign.C                   (CString, CInt, CDouble, CSize)
import           Data.ByteString             (ByteString, useAsCString, useAsCStringLen, packCString)
import           Data.Text.Encoding          (encodeUtf8)
import qualified Language.C.Inline           as C
import           Control.Monad.Catch         (MonadThrow(..), MonadCatch(..), MonadMask(..), finally)
import           Control.Monad               (when, forM_)
import           Control.Monad.Reader        (MonadReader, runReaderT, ask)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson                  (Value(..), encode, toJSON)
import qualified Data.Aeson                  as Aeson
import           Data.Scientific             (fromFloatDigits, toRealFloat, toBoundedInteger, isInteger)
import           Data.Text                   (Text)
import           Data.Vector                 (fromList, imapM_)
import           Data.HashMap.Strict         (HashMap, empty, insert, toList)
import           Data.String.Conv            (toS)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Control.Concurrent          (rtsSupportsBoundThreads, runInBoundThread)

import           Quickjs.Types
import           Quickjs.Error


C.context quickjsCtx
C.include "quickjs.h"
C.include "quickjs-libc.h"


foreign import ccall "JS_NewRuntime"
  jsNewRuntime :: IO (Ptr JSRuntime)

foreign import ccall "JS_FreeRuntime"
  jsFreeRuntime :: Ptr JSRuntime -> IO ()



foreign import ccall "JS_NewContext"
  jsNewContext :: Ptr JSRuntime -> IO (Ptr JSContext)

foreign import ccall "JS_FreeContext"
  jsFreeContext :: Ptr JSContext -> IO ()



jsFreeValue :: JSContextPtr -> JSValue -> IO ()
jsFreeValue ctx val = with val $ \v -> [C.block| void {
    if (JS_VALUE_HAS_REF_COUNT(*$(JSValue *v))) {
      JSRefCountHeader *p = (JSRefCountHeader *)JS_VALUE_GET_PTR(*$(JSValue *v));
      if (--p->ref_count <= 0) {
        __JS_FreeValue($(JSContext *ctx), *$(JSValue *v));
      }
    }
  } |]



type JSContextPtr = Ptr JSContext
type JSValueConstPtr = Ptr JSValueConst

jsIs_ :: (MonadIO m, Storable p, Eq n, Num n) => p -> (Ptr p -> IO n) -> m Bool
jsIs_ val fun = do
  b <- liftIO $ with val fun
  return $ b == 1

-- jsIsNumber :: MonadIO m => JSValue -> m Bool
-- jsIsNumber val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsNumber(*$(JSValueConst *valPtr)); } |]

jsIsArray :: MonadIO m => JSContextPtr -> JSValue -> m Bool
jsIsArray ctxPtr val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsArray($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]

jsIsDate :: MonadIO m => JSContextPtr -> JSValue -> m Bool
jsIsDate ctxPtr val = do
  globalObject <- liftIO $ C.withPtr_ $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *globalObjectPtr) = JS_GetGlobalObject($(JSContext *ctxPtr)); } |]
  dateConstructor <- jsGetPropertyStr ctxPtr globalObject "Date"
  liftIO $ do
    jsFreeValue ctxPtr globalObject
    res <- with val $ \valPtr -> with dateConstructor $ \dateCPtr -> 
      [C.block| int { return JS_IsInstanceOf($(JSContext *ctxPtr), *$(JSValueConst *valPtr), *$(JSValueConst *dateCPtr)); } |]
    jsFreeValue ctxPtr dateConstructor
    return $ res > 0


jsIsTryAll :: MonadThrow m =>
  JSValue -> [JSValue -> m Bool] -> [JSTypeEnum] -> JSTypeEnum -> m JSTypeEnum
jsIsTryAll _ [] _ def = return def
jsIsTryAll jsval (f:funs)(l:lbls) def = do
  b <- f jsval
  if b then return l else jsIsTryAll jsval funs lbls def
jsIsTryAll _ _ _ _ = throwM $ InternalError $ "jsIsTryAll_ unreachable case"


jsIs :: (MonadIO m, MonadThrow m) => JSContextPtr -> JSValue -> m JSTypeEnum
jsIs ctx jsval = case fromCType $ tag jsval of
  Just JSTagObject -> 
    jsIsTryAll jsval [jsIsArray ctx, jsIsDate ctx] [JSIsArray, JSIsDate] (JSTypeFromTag JSTagObject)
  Just t | t == JSTagBigDecimal || 
           t == JSTagBigInt ||
           t == JSTagBigFloat ||
           t == JSTagInt || 
           t == JSTagFloat64 -> return JSIsNumber
         | otherwise -> return $ JSTypeFromTag t
  Nothing -> throwM $ UnknownJSTag (tag jsval)
 


jsNullValue :: JSValue
jsNullValue = JSValue { u = 0, tag = toCType JSTagNull }

jsNewBool :: JSContextPtr -> Bool -> IO JSValue
jsNewBool ctxPtr bool = do
  let b = if bool then 1 else 0
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewBool($(JSContext *ctxPtr), $(int b)); } |]

jsNewFloat64 :: JSContextPtr -> CDouble -> IO JSValue
jsNewFloat64 ctxPtr num =
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewFloat64($(JSContext *ctxPtr), $(double num)); } |]

jsNewInt64 :: JSContextPtr -> Int64 -> IO JSValue
jsNewInt64 ctxPtr num = do
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewInt64($(JSContext *ctxPtr), $(int64_t num)); } |]

jsNewString :: JSContextPtr -> ByteString -> IO JSValue
jsNewString ctxPtr s = C.withPtr_ $ \ptr -> useAsCStringLen s $ \(cstringPtr, cstringLen) -> do
  let len = fromIntegral cstringLen
  [C.block| void { *$(JSValue *ptr) = JS_NewStringLen($(JSContext *ctxPtr), $(const char *cstringPtr), $(size_t len)); } |]



checkIsException :: (MonadThrow m, MonadIO m) => Text -> JSContextPtr -> JSValue -> m ()
checkIsException loc ctxPtr val =
  case fromCType $ tag val of
    Just JSTagException -> do
      err <- getErrorMessage ctxPtr 
      liftIO $ jsFreeValue ctxPtr val
      throwM $ JSException loc err
    _ -> pure ()



jsonToJSValue :: (MonadThrow m, MonadIO m) => JSContextPtr -> Value -> m JSValue
jsonToJSValue _ Null = pure jsNullValue
jsonToJSValue ctx (Bool b) = liftIO $ jsNewBool ctx b
jsonToJSValue ctx (Number n) = 
  if not (isInteger n) then liftIO $ jsNewFloat64 ctx (toRealFloat n)
  else case toBoundedInteger n of
    Just i -> liftIO $ jsNewInt64 ctx i
    Nothing -> throwM $ InternalError "Value does not fit in Int64"
jsonToJSValue ctx (String s) = liftIO $ jsNewString ctx $ toS s
jsonToJSValue ctxPtr (Array xs) = do
  arrVal <- liftIO (C.withPtr_ $ \arrValPtr -> [C.block| void { *$(JSValueConst *arrValPtr) = JS_NewArray($(JSContext *ctxPtr)); } |])
  
  checkIsException "jsonToJSValue/Array/1" ctxPtr arrVal

  flip imapM_ xs $ \index value -> do 
    val <- jsonToJSValue ctxPtr value
    checkIsException "jsonToJSValue/Array/2" ctxPtr val

    let idx = fromIntegral index
    code <- liftIO (with arrVal $ \arrValPtr -> with val $ \valPtr -> 
      [C.block| int { return JS_DefinePropertyValueUint32(
        $(JSContext *ctxPtr), 
        *$(JSValueConst *arrValPtr),
        $(uint32_t idx),
        *$(JSValueConst *valPtr),
        JS_PROP_C_W_E
      ); } |])
    return ()

    if (code < 0) then do
      liftIO $ jsFreeValue ctxPtr arrVal
      throwM $ InternalError "Could not append element to array"
    else return ()

  return arrVal
jsonToJSValue ctxPtr (Object o) = do
  objVal <- liftIO (C.withPtr_ $ \objValPtr -> 
    [C.block| void { *$(JSValueConst *objValPtr) = JS_NewObject($(JSContext *ctxPtr)); } |])

  checkIsException "jsonToJSValue/Object/1" ctxPtr objVal
  
  forM_ (toList o) $ \(key,value) -> do
    val <- jsonToJSValue ctxPtr value
    checkIsException "jsonToJSValue/Object/2" ctxPtr val

    code <- liftIO (with objVal $ \objValPtr -> with val $ \valPtr -> 
      useAsCString (encodeUtf8 key) $ \cstringPtr -> do
        [C.block| int { 
          return JS_DefinePropertyValueStr(
            $(JSContext *ctxPtr), 
            *$(JSValueConst *objValPtr),
            $(const char *cstringPtr),
            *$(JSValueConst *valPtr),
            JS_PROP_C_W_E
          ); 
        } |])

    when (code < 0) $ do
      liftIO $ jsFreeValue ctxPtr objVal
      throwM $ InternalError "Could not add add property to object"

  return objVal


jsToBool :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Bool
jsToBool ctxPtr val = do
    code <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_ToBool($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    case code of
        -1 -> getErrorMessage ctxPtr >>= throwM . JSException "jsToBool"
        0 -> return False
        _ -> return True

jsToInt64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Int64
jsToInt64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \intPtr -> with val $ \valPtr -> [C.block| int { return JS_ToInt64($(JSContext *ctxPtr), $(int64_t *intPtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwM . JSException "jsToInt64"


jsToFloat64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m CDouble
jsToFloat64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \doublePtr -> with val $ \valPtr -> [C.block| int { return JS_ToFloat64($(JSContext *ctxPtr), $(double *doublePtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwM . JSException "jsToFloat64"



jsToString :: MonadIO m => JSContextPtr -> JSValue -> m ByteString
jsToString ctxPtr val = liftIO $ do
    cstring <- with val $ \valPtr -> [C.block| const char * { return JS_ToCString($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    if cstring == nullPtr then return ""
    else do
      string <- packCString cstring
      jsFreeCString ctxPtr cstring
      return string


foreign import ccall "JS_FreeCString"
  jsFreeCString :: JSContextPtr -> CString -> IO ()


jsToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m Value
jsToJSON ctx jsval = do
  ty <- jsIs ctx jsval
  case ty of
    JSTypeFromTag JSTagException -> do
      err <- getErrorMessage ctx 
      liftIO $ jsFreeValue ctx jsval
      throwM $ JSException "jsToJSON/JSTagException" err
    JSTypeFromTag JSTagNull -> return Null
    JSTypeFromTag JSTagUndefined -> return Null
    JSTypeFromTag JSTagBool -> do
      b <- jsToBool ctx jsval
      return $ Bool b
    JSIsNumber -> do
      n <- jsToFloat64 ctx jsval
      return $ Number $ fromFloatDigits n
    JSTypeFromTag JSTagString -> do
      s <- jsToString ctx jsval
      return $ String $ toS s
    JSIsArray -> do
      len <- do
        lenVal <- jsGetPropertyStr ctx jsval "length" 
        len' <- jsToInt64 ctx lenVal
        liftIO $ jsFreeValue ctx lenVal
        return len'
      vs <- jsArrayToJSON ctx jsval 0 (fromIntegral len)
      return $ Array $ fromList vs
    JSIsDate -> do
      getter <- jsGetPropertyStr ctx jsval "getTime" 

      timestampRaw <- liftIO $ C.withPtr_ $ \res -> with getter $ \getterPtr -> with jsval $ \jsvalPtr -> 
        [C.block| void { *$(JSValue *res) = JS_Call($(JSContext *ctx), *$(JSValueConst *getterPtr), *$(JSValueConst *jsvalPtr), 0, NULL); } |]

      timestamp <- jsToFloat64 ctx timestampRaw
      liftIO $ do
        jsFreeValue ctx getter
        jsFreeValue ctx timestampRaw
      return $ toJSON $ posixSecondsToUTCTime $ realToFrac $ timestamp / 1000
    JSTypeFromTag JSTagObject -> do
      o <- jsObjectToJSON ctx jsval
      return $ Object o 
    JSTypeFromTag f -> throwM $ UnsupportedTypeTag f
    JSIsError -> throwM $ InternalError "JSIsError unreachable"


jsArrayToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> Int -> Int -> m [Value]
jsArrayToJSON ctxPtr jsval index len = 
  if index < len then do
    v <- do
      let idx = fromIntegral index
      val <- liftIO $ C.withPtr_ $ \ptr -> with jsval $ \jsvalPtr -> 
        [C.block| void { *$(JSValue *ptr) = JS_GetPropertyUint32($(JSContext *ctxPtr), *$(JSValueConst *jsvalPtr), $(uint32_t idx)); } |]

      checkIsException "jsArrayToJSON" ctxPtr val
      res <- jsToJSON ctxPtr val
      liftIO $ jsFreeValue ctxPtr val
      return res

    vs <- jsArrayToJSON ctxPtr jsval (index+1) len
    return $ v:vs
  else return []






forLoop :: (Num a, Ord a, Monad m) => a -> (a -> m ()) -> m ()
forLoop end f = go 0
  where
    go !x | x < end   = f x >> go (x+1)
          | otherwise = return ()




jsObjectToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m (HashMap Text Value)
jsObjectToJSON ctxPtr obj = do
    let flags = toCType JSGPNStringMask .|. toCType JSGPNSymbolMask .|. toCType JSGPNEnumOnly
    properties <- liftIO $ malloc
    plen <- jsGetOwnPropertyNames ctxPtr obj properties flags 
      `catch` (\(e::SomeJSRuntimeException) -> do
        liftIO $ free properties
        throwM e
      )
    objPtr <- liftIO $ malloc
    liftIO $ poke objPtr obj

    res <- collectVals properties objPtr 0 plen `catch` (\(e::SomeJSRuntimeException) -> do
        liftIO $ free objPtr
        throwM e
      )
    cleanup properties plen
    return res
  where
    collectVals :: (MonadCatch m, MonadIO m) => Ptr (Ptr JSPropertyEnum) -> JSValueConstPtr -> Int -> Int -> m (HashMap Text Value)
    collectVals properties objPtr !index end 
      | index < end = do
        let i = fromIntegral index

        key <- do
          key' <- liftIO $ C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_AtomToString($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
          checkIsException "jsObjectToJSON/collectVals/1" ctxPtr key'
          res <- jsToJSON ctxPtr key'
          liftIO $ jsFreeValue ctxPtr key'
          return res

        case key of 
          String k -> do
            val <-  do
              val' <- liftIO $ C.withPtr_ $ \ptr ->
                [C.block| void { *$(JSValue *ptr) = JS_GetProperty($(JSContext *ctxPtr), *$(JSValueConst *objPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
              checkIsException "jsObjectToJSON/collectVals/2" ctxPtr val'
              res <- jsToJSON ctxPtr val'
              liftIO $ jsFreeValue ctxPtr val'
              return res

            xs <- collectVals properties objPtr (index+1) end
            return $ insert k val xs
          x -> throwM $ InternalError $ "Could not get property name" <> toS (encode x)

      | otherwise = return empty

    cleanup :: MonadIO m => Ptr (Ptr JSPropertyEnum) -> Int -> m ()
    cleanup properties plen = liftIO $ do
      forLoop plen $ \index -> do
        let i = fromIntegral index
        [C.block| void { JS_FreeAtom($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); }|]

      let void_ptr = castPtr properties
      [C.block| void { js_free($(JSContext *ctxPtr), *$(void **void_ptr)); }|]

      free properties



getErrorMessage :: MonadIO m => JSContextPtr -> m Text
getErrorMessage ctxPtr = liftIO $ do
  ex <- C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_GetException($(JSContext *ctxPtr)); } |]
  res <- jsToString ctxPtr ex
  jsFreeValue ctxPtr ex
  return $ toS res



jsGetPropertyStr :: MonadIO m => JSContextPtr -> JSValue -> ByteString -> m JSValue
jsGetPropertyStr ctxPtr val str = liftIO $
  C.withPtr_ $ \ptr -> useAsCString str $ \prop -> with val $ \valPtr ->
    [C.block| void { *$(JSValue *ptr) = JS_GetPropertyStr($(JSContext *ctxPtr), *$(JSValueConst *valPtr), $(const char *prop)); } |]


jsGetOwnPropertyNames :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> Ptr (Ptr JSPropertyEnum) -> CInt -> m Int
jsGetOwnPropertyNames ctxPtr val properties flags = do
  (len,code) <- liftIO $ C.withPtr $ \plen -> with val $ \valPtr -> 
    [C.block| int { return JS_GetOwnPropertyNames($(JSContext *ctxPtr), $(JSPropertyEnum **properties), $(uint32_t *plen), *$(JSValueConst *valPtr), $(int flags)); } |]
  if code == 0 then return (fromIntegral len)
  else throwM $ InternalError "Could not get object properties"


jsCall :: JSContextPtr -> JSValue -> CInt -> (Ptr JSValue) -> IO JSValue
jsCall ctxt fun_obj argc argv = C.withPtr_ $ \res -> with fun_obj $ \funPtr -> 
  [C.block| void { *$(JSValue *res) = JS_Call($(JSContext *ctxt), *$(JSValueConst *funPtr), JS_NULL, $(int argc), $(JSValueConst *argv)); } |]


jsEval :: JSContextPtr -> CString -> CSize -> CString -> CInt -> IO JSValue
jsEval ctxPtr input input_len filename eval_flags = C.withPtr_ $ \ptr -> 
  [C.block| void { *$(JSValue *ptr) = JS_Eval($(JSContext *ctxPtr), $(const char *input), $(size_t input_len), $(const char *filename), $(int eval_flags)); } |]


evalRaw :: JSContextPtr -> JSEvalType -> ByteString -> IO JSValue
evalRaw ctx eTyp code = 
    useAsCString "script.js" $ \cfilename ->
        useAsCStringLen code $ \(ccode, ccode_len) -> 
            jsEval ctx ccode (fromIntegral ccode_len) cfilename (toCType eTyp)




evalAs :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => JSEvalType -> ByteString -> m Value
evalAs eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  -- checkIsException "evalAs" ctx val
  jsToJSON ctx val `finally` freeJSValue val



{-|
Evaluates the given string and returns a 'Value' (if the result can be converted).
-}
eval :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> m Value
eval = evalAs Global

evalAs_ :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSEvalType -> ByteString -> m ()
evalAs_ eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  checkIsException "evalAs_" ctx val
  freeJSValue val



{-|
More efficient than 'eval' if we don't care about the value of the expression, 
e.g. if we are evaluating a function definition or performing other side-effects such as
printing to console/modifying state.
-}
eval_ :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> m ()
eval_ = evalAs_ Global


fromJSValue_ :: (MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m Value
fromJSValue_ val = do
  ctx <- ask
  jsToJSON ctx val



-- fromJSValue :: (Aeson.FromJSON a, MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m a
-- fromJSValue val = do
--   jsonval <- fromJSValue_ val

--   case Aeson.fromJSON jsonval of
--     Aeson.Success a -> return a
--     Aeson.Error err -> throwM $ InternalError err



{-|
Takes a value with a defined 'ToJSON' instance. This value is marshalled to a 'JSValue'
and passed as an argument to the callback function, provided as the second argument to 'withJSValue'
-}
withJSValue :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m, Aeson.ToJSON a) => a -> (JSValue -> m b) -> m b
withJSValue v f = do

  ctx <- ask
  val <- jsonToJSValue ctx (Aeson.toJSON v)
  f val `finally` freeJSValue val




callRaw :: (MonadThrow m, MonadIO m) => JSContextPtr -> ByteString -> [JSValue] -> m JSValue
callRaw ctxPtr funName args = do
    globalObject <- liftIO $ C.withPtr_ $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *globalObjectPtr) = JS_GetGlobalObject($(JSContext *ctxPtr)); } |]

    fun <- jsGetPropertyStr ctxPtr globalObject funName

    liftIO $ jsFreeValue ctxPtr globalObject

    ty <- jsIs ctxPtr fun
    case ty of
      JSTypeFromTag JSTagException -> do
        err <- getErrorMessage ctxPtr 
        liftIO $ jsFreeValue ctxPtr fun
        throwM $ JSException "callRaw" err
      JSTypeFromTag JSTagUndefined -> throwM $ JSValueUndefined $ toS funName
      JSTypeFromTag JSTagObject -> do
        res <- liftIO $ withArrayLen args $ \len argv -> jsCall ctxPtr fun (fromIntegral $ len) argv
        liftIO $ jsFreeValue ctxPtr fun
        return res
      _ -> throwM $ JSValueIncorrectType {name = toS funName, expected = JSTypeFromTag JSTagObject, found = ty }


-- call :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => String -> [JSValue] -> m JSValue
-- call funName args = do
--   ctx <- ask
--   val <- callRaw ctx funName args
--   checkIsException ctx val
--   return val



call :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => ByteString -> [JSValue] -> m Value
call funName args = do
  ctx <- ask
  val <- callRaw ctx funName args
  jsToJSON ctx val `finally` freeJSValue val


freeJSValue :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m ()
freeJSValue val = do
  ctx <- ask
  liftIO $ jsFreeValue ctx val

{-|
This function initialises a new JS runtime and performs the given computation within this context.

For example, we can evaluate an expression:

>quickjs $ do
>  res <- eval "1+2"
>  liftIO $ print res

Declare a function and call it on an argument:

>quickjs $ do
>  _ <- eval_ "f = (x) => x+1"
>  res <- eval "f(2)"
>  liftIO $ print res

Pass a Haskell value to the JS runtime:

>quickjs $ do
>  _ <- eval_ "f = (x) => x+1"
>  res <- withJSValue (3::Int) $ \x -> call "f" [x]
>  liftIO $ print res

-}
quickjs :: MonadIO m => ReaderT (Ptr JSContext) m b -> m b
quickjs f = do
  (rt, ctx) <- liftIO $ do
    _rt <- jsNewRuntime
    _ctx <- jsNewContext _rt

    [C.block| void { 
      js_std_add_helpers($(JSContext *_ctx), -1, NULL);
    } |]
    return (_rt, _ctx)

  res <- runReaderT f ctx
  cleanup ctx rt
  return res
  where
    cleanup ctx rt = liftIO $ do
      jsFreeContext ctx
      jsFreeRuntime rt