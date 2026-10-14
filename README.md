# Base64-lens

[![Build Status](https://github.com/emilypi/base64-lens/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/emilypi/base64-lens/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/base64-lens.svg)](https://hackage.haskell.org/package/base64-lens)

This package provides optics and bidirectional pattern synonyms for the
[base64](https://hackage.haskell.org/package/base64) library, covering strict,
lazy, and short `ByteString` and `Text` values.

Version 0.4 supports `base64` 2.0, `lens` 5.0–5.3, and GHC 9.6–9.14.
The public optics still consume and produce ordinary strings; the typed
`Base64` wrappers used by the underlying library stay inside the implementation.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Lens
import qualified Data.ByteString.Base64.Lens as B64

-- "U3Vu"
encoded = B64._Base64 # "Sun"

-- Just "Sun"
decoded = "U3Vu" ^? B64._Base64

-- Nothing: non-canonical padding bits
rejected = "U3V=" ^? B64._Base64

-- "U3Vu": pattern construction encodes
patternEncoded = B64.Base64 "Sun"
```

`_Base64` checks the standard alphabet, padding, and canonicity, using
`base64` 2.0's checked `decodeBase64Untyped` decoder. It accepts the same
encodings as `parseBase64` and returns the decoded payload as the focus.
`_Base64Url` accepts padded or unpadded URL-safe input and constructs padded
output. `_Base64UrlUnpadded` constructs unpadded output and rejects padded input.

The lenient optics ignore invalid Base64 characters when decoding. They are not
lawful isomorphisms for arbitrary input. Their patterns encode when constructed
and decode when matched; version 0.4 corrects the reversed behavior of
`Base64Lenient` and `Base64UrlLenient` in earlier versions.

The `Text` optics expect the decoded payload to be UTF-8, as in the underlying
library. Invalid UTF-8 may throw an exception. Use the `ByteString` optics for
arbitrary binary payloads.

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
