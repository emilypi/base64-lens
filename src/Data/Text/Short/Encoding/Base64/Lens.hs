{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.Text.Short.Encoding.Base64.Lens
-- Copyright 	: (c) 2019-2026 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism's and 'Iso''s Base64-encoding and
-- decoding 'ShortText' values.
--
module Data.Text.Short.Encoding.Base64.Lens
( -- * Prisms
  _Base64
, _Base64Url
, _Base64UrlUnpadded
, _Base64Lenient
, _Base64UrlLenient
  -- * Patterns
, pattern Base64
, pattern Base64Url
, pattern Base64UrlUnpadded
, pattern Base64Lenient
, pattern Base64UrlLenient
) where

import Control.Lens

import Data.Base64.Types (extractBase64)

import Data.Text.Short (ShortText)
import qualified Data.Text.Short.Encoding.Base64 as TS64
import qualified Data.Text.Short.Encoding.Base64.URL as TS64U


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.Text.Short.Encoding.Base64.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism' into the Base64 encoding of a 'ShortText' value.
--
-- >>> _Base64 # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^? _Base64
-- Just "Sun"
--
_Base64 :: Prism' ShortText ShortText
_Base64 = prism' (extractBase64 . TS64.encodeBase64) $ \s ->
    case TS64.decodeBase64Untyped s of
        Left _ -> Nothing
        Right a -> Just a
{-# INLINE _Base64 #-}

-- | A 'Prism' into the Base64-url encoding of a 'ShortText' value.
--
-- >>> _Base64Url # "Sun"
-- "U3Vu"
--
-- >>> "PDw_Pz8-Pg==" ^? _Base64Url
-- Just "<<???>>"
--
_Base64Url :: Prism' ShortText ShortText
_Base64Url = prism' (extractBase64 . TS64U.encodeBase64) $ \s ->
    case TS64U.decodeBase64Untyped s of
        Left _ -> Nothing
        Right a -> Just a
{-# INLINE _Base64Url #-}

-- | A 'Prism' into the Base64-url encoding of a 'ShortText' value.
--
-- Encodes without padding and rejects padded input when decoding.
--
-- >>> _Base64UrlUnpadded # "<<??>>"
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^? _Base64UrlUnpadded
-- Just "<<??>>"
--
_Base64UrlUnpadded :: Prism' ShortText ShortText
_Base64UrlUnpadded = prism' (extractBase64 . TS64U.encodeBase64Unpadded) $ \s ->
    case TS64U.decodeBase64UnpaddedUntyped s of
        Left _ -> Nothing
        Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}

-- | An 'Iso'' into the Base64 encoding of a 'ShortText' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso' in general. Please take care!
--
-- >>> _Base64Lenient # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^. _Base64Lenient
-- "Sun"
--
_Base64Lenient :: Iso' ShortText ShortText
_Base64Lenient = iso TS64.decodeBase64Lenient (extractBase64 . TS64.encodeBase64)

-- | An 'Iso'' into the Base64url encoding of a 'ShortText' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso' in general. Please take care!
--
-- >>> _Base64UrlLenient # "<<??>>"
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^. _Base64UrlLenient
-- "<<??>>"
--
_Base64UrlLenient :: Iso' ShortText ShortText
_Base64UrlLenient = iso TS64U.decodeBase64Lenient (extractBase64 . TS64U.encodeBase64)

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for base64-encoded 'ShortText' values.
--
pattern Base64 :: ShortText -> ShortText
pattern Base64 a <- (preview _Base64 -> Just a) where
    Base64 a = _Base64 # a

-- | Bidirectional pattern synonym for base64url-encoded 'ShortText' values.
--
pattern Base64Url :: ShortText -> ShortText
pattern Base64Url a <- (preview _Base64Url -> Just a) where
    Base64Url a = _Base64Url # a

-- | Bidirectional pattern synonym for unpadded base64url-encoded 'ShortText' values.
--
pattern Base64UrlUnpadded :: ShortText -> ShortText
pattern Base64UrlUnpadded a <- (preview _Base64UrlUnpadded -> Just a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a

-- | Bidirectional pattern synonym for leniently Base64-encoded 'ShortText' values
--
pattern Base64Lenient :: ShortText -> ShortText
pattern Base64Lenient a <- (view _Base64Lenient -> a) where
    Base64Lenient a = _Base64Lenient # a
{-# COMPLETE Base64Lenient #-}

-- | Bidirectional pattern synonym for leniently Base64-encoded 'ShortText' values
--
pattern Base64UrlLenient :: ShortText -> ShortText
pattern Base64UrlLenient a <- (view _Base64UrlLenient -> a) where
    Base64UrlLenient a = _Base64UrlLenient # a
{-# COMPLETE Base64UrlLenient #-}
