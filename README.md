# quickjs-hs

![Tests Cabal](https://github.com/goodlyrottenapple/quickjs-hs/workflows/Tests%20Cabal/badge.svg)


This package provides a Haskell wrapper for the [QuickJS](https://bellard.org/quickjs/) Javascript Engine. It has been inspired by the [quickjs-rs](https://github.com/theduke/quickjs-rs) and [ocaml-quickjs](https://github.com/dhcmrlchtdj/ocaml-quickjs) libraries.
## Features

The functionality is quite basic and is currently limited to:
- evaluating JS code
- calling a JS function in the global scope
- marshalling [Aeson Values](https://hackage.haskell.org/package/aeson-1.5.3.0/docs/Data-Aeson.html#t:Value) to and from JSValues.


## Examples
Evaluate an expression:

```haskell
import Quickjs

one_plus_two = quickjs $ do
  res <- eval "1+2"
  liftIO $ print res
```

Declare a function and call it on an argument:

```haskell
call_f = quickjs $ do
  _ <- eval_ "f = (x) => x+1"
  res <- eval "f(2)"
  liftIO $ print res
```

Pass a Haskell value (which has a [ToJSON](https://hackage.haskell.org/package/aeson-1.5.3.0/docs/Data-Aeson.html#t:ToJSON) instance) to the JS runtime:

```haskell
aeson_marshall = quickjs $ do
  _ <- eval_ "f = (x) => x+1"
  res <- withJSValue (3::Int) $ \x -> call "f" [x]
  liftIO $ print res
```

## Contributing

Please feel free to report bugs/submit feature requests via the [github issue tracker](https://github.com/goodlyrottenapple/quickjs-hs/issues) and submit any pull requests to the [git repository](https://github.com/goodlyrottenapple/quickjs-hs/)
