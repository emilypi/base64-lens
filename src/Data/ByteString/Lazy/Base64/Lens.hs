{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base64.Lens
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- Optics and patterns for Base64-encoded 'ByteString' values.
-- The alphabet is recorded in the t'Base64' wrapper. Validate untrusted
-- standard-alphabet input with 'BL64.parseBase64' before decoding.
-- For access to the encoded payload itself, see "Data.Base64.Types.Lens".
--
module Data.ByteString.Lazy.Base64.Lens
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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base64 as BL64
import qualified Data.ByteString.Lazy.Base64.URL as BL64U

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Base64.Types (assertBase64)
-- >>> import Data.ByteString.Lazy.Base64.Lens
-- >>> :set -XOverloadedStrings

-- | Decode standard, padded Base64, or encode in reverse.
--
-- >>> _Base64 # "Sun"
-- "U3Vu"
--
-- >>> (_Base64 # "Sun") ^. _Base64
-- "Sun"
--
_Base64 :: Iso' (Base64 'StdPadded ByteString) ByteString
_Base64 = iso BL64.decodeBase64 BL64.encodeBase64'
{-# INLINE _Base64 #-}

-- | Decode padded URL-safe Base64, or encode in reverse.
--
-- >>> _Base64Url # "Sun"
-- "U3Vu"
--
-- >>> (_Base64Url # "Sun") ^. _Base64Url
-- "Sun"
--
_Base64Url :: Iso' (Base64 'UrlPadded ByteString) ByteString
_Base64Url = iso BL64U.decodeBase64 BL64U.encodeBase64'
{-# INLINE _Base64Url #-}

-- | Decode unpadded URL-safe Base64, or encode in reverse.
--
-- >>> _Base64UrlUnpadded # "Sun"
-- "U3Vu"
--
-- >>> (_Base64UrlUnpadded # "Sun") ^. _Base64UrlUnpadded
-- "Sun"
--
_Base64UrlUnpadded :: Iso' (Base64 'UrlUnpadded ByteString) ByteString
_Base64UrlUnpadded = iso BL64U.decodeBase64Unpadded BL64U.encodeBase64Unpadded'
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
_Base64Lenient :: Iso' (Base64 'NonStandard ByteString) ByteString
_Base64Lenient = iso (BL64.decodeBase64Lenient . extractBase64)
    (coerceBase64 . BL64.encodeBase64')
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
_Base64UrlLenient :: Iso' (Base64 'NonStandard ByteString) ByteString
_Base64UrlLenient = iso (BL64U.decodeBase64Lenient . extractBase64)
    (coerceBase64 . BL64U.encodeBase64')
{-# INLINE _Base64UrlLenient #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64 :: ByteString -> Base64 'StdPadded ByteString
pattern Base64 a <- (view _Base64 -> a) where
    Base64 a = _Base64 # a
{-# COMPLETE Base64 #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64Url :: ByteString -> Base64 'UrlPadded ByteString
pattern Base64Url a <- (view _Base64Url -> a) where
    Base64Url a = _Base64Url # a
{-# COMPLETE Base64Url #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64UrlUnpadded :: ByteString -> Base64 'UrlUnpadded ByteString
pattern Base64UrlUnpadded a <- (view _Base64UrlUnpadded -> a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a
{-# COMPLETE Base64UrlUnpadded #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64Lenient :: ByteString -> Base64 'NonStandard ByteString
pattern Base64Lenient a <- (view _Base64Lenient -> a) where
    Base64Lenient a = _Base64Lenient # a
{-# COMPLETE Base64Lenient #-}

-- | Construct an encoded value or match its decoded payload.
--
pattern Base64UrlLenient :: ByteString -> Base64 'NonStandard ByteString
pattern Base64UrlLenient a <- (view _Base64UrlLenient -> a) where
    Base64UrlLenient a = _Base64UrlLenient # a
{-# COMPLETE Base64UrlLenient #-}
