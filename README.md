# Base64-lens

[![Build Status](https://github.com/emilypi/base64-lens/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/emilypi/base64-lens/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/base64-lens.svg)](https://hackage.haskell.org/package/base64-lens)

Optics and bidirectional patterns for
[base64](https://hackage.haskell.org/package/base64), covering strict, lazy, and
short `ByteString` and `Text` values. Version 0.4 supports `base64` 2.0,
`lens` 5.0–5.3, and GHC 9.6–9.14.

## Encoding and decoding

The string modules expose typed isomorphisms. Viewing decodes a `Base64` value;
reviewing encodes the payload and returns a wrapper with the matching alphabet.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Base64.Types (Alphabet(..), Base64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.Lens as B64

encoded :: Base64 'StdPadded ByteString
encoded = B64._Base64 # "Sun"

-- "Sun"
decoded = encoded ^. B64._Base64

-- Right "Sun": validate external input before using the typed optic.
checked = fmap (view B64._Base64) (Base64.parseBase64 "U3Vu")
```

Each string module provides these optics, where `a` is its string type:

| Optic | Type |
| --- | --- |
| `_Base64` | `Iso' (Base64 'StdPadded a) a` |
| `_Base64Url` | `Iso' (Base64 'UrlPadded a) a` |
| `_Base64UrlUnpadded` | `Iso' (Base64 'UrlUnpadded a) a` |
| `_Base64Lenient` | `Iso' (Base64 'NonStandard a) a` |
| `_Base64UrlLenient` | `Iso' (Base64 'NonStandard a) a` |

The corresponding patterns construct encoded wrappers and match decoded
payloads. For example, `B64.Base64 "Sun"` has type
`Base64 'StdPadded ByteString`, and matching `B64.Base64 bytes` binds the
decoded bytes. Each pattern is complete for its input type.

The lenient optics ignore invalid Base64 characters, stop at padding, and
construct canonical padded output tagged as `NonStandard`. They are not lawful
isomorphisms for arbitrary input.

The `Text` optics expect the decoded payload to be UTF-8, as in the underlying
library. Invalid UTF-8 may throw an exception. Use the `ByteString` optics for
arbitrary binary payloads.

## Wrapper and alphabet optics

`Data.Base64.Types.Lens` gives the wrapper its own namespace. Its `_Base64`
accesses the **encoded** payload and can change its representation while
preserving the alphabet index:

```haskell
import qualified Data.Base64.Types.Lens as Wrapped
import qualified Data.ByteString.Lazy as BL

-- "U3Vu": the encoded payload
payload = encoded ^. Wrapped._Base64

lazyEncoded :: Base64 'StdPadded BL.ByteString
lazyEncoded = encoded & Wrapped._Base64 %~ BL.fromStrict
```

Importing this module also enables the standard `Wrapped` and `Rewrapped`
instances, so `_Wrapped` and `_Wrapped'` work with `Base64`. Wrapping uses
`assertBase64`: it does not encode or validate. Payload changes must preserve
the claimed encoding if the result will be decoded. These optics keep the
alphabet index fixed; they do not silently change standard Base64 into URL-safe
Base64.

The same module supplies `_StdPadded`, `_UrlPadded`, `_UrlUnpadded`, and
`_NonStandard`, each a `Prism' Alphabet ()`.

`Data.Text.Encoding.Base64.Error.Lens` provides `_DecodeError` and a
type-changing `_ConversionError` prism for mapping conversion failures.

## Migrating from 0.3

The string optics now operate on `Base64` wrappers instead of encoded strings.
Use `view` or `(^.)` for typed decoding. Replace standard-alphabet validation
through `preview _Base64` with `parseBase64`, then decode the returned wrapper.
To pass encoded output to an API expecting an ordinary string, extract it with
`Wrapped._Base64` or `extractBase64`.

The lenient patterns also change direction: construction encodes and matching
decodes, consistently with the other patterns.

## Development

```sh
cabal update
cabal test --test-show-details=direct
cabal haddock
cabal check
```

To exercise the optional SIMD backend in `base64`:

```sh
cabal test --constraint='base64 +simd' --test-show-details=direct
```
