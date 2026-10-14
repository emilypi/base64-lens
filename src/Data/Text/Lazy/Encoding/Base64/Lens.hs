{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base64.Lens
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- Optics and patterns for Base64-encoded 'Text' values.
-- The alphabet is recorded in the t'Base64' wrapper. Validate untrusted
-- standard-alphabet input with 'B64TL.parseBase64' before decoding.
-- For access to the encoded payload itself, see "Data.Base64.Types.Lens".
--
module Data.Text.Lazy.Encoding.Base64.Lens
( -- * Encoding isomorphisms
  _Base64
, _Base64Url
, _Base64UrlUnpadded
  -- * Lenient decoding
, _Base64Lenient
, _Base64UrlLenient
  -- * Patterns
, pattern Base64
, pattern Base64Url
, pattern Base64UrlUnpadded
, pattern Base64Lenient
, pattern Base64UrlLenient
) where

import Control.Lens (Iso', iso, view, (#))
import Data.Base64.Types (Alphabet(..), Base64, coerceBase64, extractBase64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding.Base64 as B64TL
import qualified Data.Text.Lazy.Encoding.Base64.URL as B64TLU

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Base64.Types (assertBase64)
-- >>> import Data.Text.Lazy.Encoding.Base64.Lens
-- >>> :set -XOverloadedStrings

-- | Decode standard, padded Base64, or encode in reverse.
--
-- >>> _Base64 # "Sun"
-- "U3Vu"
--
-- >>> (_Base64 # "Sun") ^. _Base64
-- "Sun"
--
_Base64 :: Iso' (Base64 'StdPadded Text) Text
_Base64 = iso B64TL.decodeBase64 B64TL.encodeBase64
{-# INLINE _Base64 #-}

-- | Decode padded URL-safe Base64, or encode in reverse.
--
-- >>> _Base64Url # "Sun"
-- "U3Vu"
--
-- >>> (_Base64Url # "Sun") ^. _Base64Url
-- "Sun"
--
_Base64Url :: Iso' (Base64 'UrlPadded Text) Text
_Base64Url = iso B64TLU.decodeBase64 B64TLU.encodeBase64
{-# INLINE _Base64Url #-}

-- | Decode unpadded URL-safe Base64, or encode in reverse.
--
-- >>> _Base64UrlUnpadded # "Sun"
-- "U3Vu"
--
-- >>> (_Base64UrlUnpadded # "Sun") ^. _Base64UrlUnpadded
-- "Sun"
--
_Base64UrlUnpadded :: Iso' (Base64 'UrlUnpadded Text) Text
_Base64UrlUnpadded = iso B64TLU.decodeBase64Unpadded B64TLU.encodeBase64Unpadded
{-# INLINE _Base64UrlUnpadded #-}

-- | Decode lenient standard-alphabet Base64, or encode in reverse.
--
-- Ignores invalid characters and stops at the first padding character.
-- This is not a lawful 'Iso'' for arbitrary input. Construction produces
-- canonical padded output tagged as 'NonStandard'.
--
-- >>> _Base64Lenient # "Sun"
-- "U3Vu"
--
-- >>> view _Base64Lenient (assertBase64 "U3!Vu")
-- "Sun"
--
_Base64Lenient :: Iso' (Base64 'NonStandard Text) Text
_Base64Lenient = iso (B64TL.decodeBase64Lenient . extractBase64)
    (coerceBase64 . B64TL.encodeBase64)
{-# INLINE _Base64Lenient #-}

-- | Decode lenient URL-safe Base64, or encode in reverse.
--
-- Ignores invalid characters and stops at the first padding character.
-- This is not a lawful 'Iso'' for arbitrary input. Construction produces
-- canonical padded output tagged as 'NonStandard'.
--
-- >>> _Base64UrlLenient # "Sun"
-- "U3Vu"
--
-- >>> view _Base64UrlLenient (assertBase64 "U3!Vu")
-- "Sun"
--
_Base64UrlLenient :: Iso' (Base64 'NonStandard Text) Text
_Base64UrlLenient = iso (B64TLU.decodeBase64Lenient . extractBase64)
    (coerceBase64 . B64TLU.encodeBase64)
{-# INLINE _Base64UrlLenient #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64 :: Text -> Base64 'StdPadded Text
pattern Base64 a <- (view _Base64 -> a) where
    Base64 a = _Base64 # a
{-# COMPLETE Base64 #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64Url :: Text -> Base64 'UrlPadded Text
pattern Base64Url a <- (view _Base64Url -> a) where
    Base64Url a = _Base64Url # a
{-# COMPLETE Base64Url #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64UrlUnpadded :: Text -> Base64 'UrlUnpadded Text
pattern Base64UrlUnpadded a <- (view _Base64UrlUnpadded -> a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a
{-# COMPLETE Base64UrlUnpadded #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64Lenient :: Text -> Base64 'NonStandard Text
pattern Base64Lenient a <- (view _Base64Lenient -> a) where
    Base64Lenient a = _Base64Lenient # a
{-# COMPLETE Base64Lenient #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64UrlLenient :: Text -> Base64 'NonStandard Text
pattern Base64UrlLenient a <- (view _Base64UrlLenient -> a) where
    Base64UrlLenient a = _Base64UrlLenient # a
{-# COMPLETE Base64UrlLenient #-}
