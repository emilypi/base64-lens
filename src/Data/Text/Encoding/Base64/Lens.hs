{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism's and 'Iso''s Base64-encoding and
-- decoding 'Text' values.
--
module Data.Text.Encoding.Base64.Lens
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

import Data.Text (Text)
import qualified Data.Text.Encoding.Base64 as B64T
import qualified Data.Text.Encoding.Base64.URL as B64TU


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.Text.Encoding.Base64.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism' into the Base64 encoding of a 'Text' value.
--
-- >>> _Base64 # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^? _Base64
-- Just "Sun"
--
_Base64 :: Prism' Text Text
_Base64 = prism' B64T.encodeBase64 $ \s -> case B64T.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64 #-}

-- | A 'Prism' into the Base64-url encoding of a 'Text' value.
--
-- >>> _Base64Url # "Sun"
-- "U3Vu"
--
-- >>> "PDw_Pz8-Pg==" ^? _Base64Url
-- Just "<<???>>"
--
_Base64Url :: Prism' Text Text
_Base64Url = prism' B64TU.encodeBase64 $ \s -> case B64TU.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Url #-}

-- | A 'Prism' into the Base64-url encoding of a 'Text' value.
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base64UrlUnpadded # "<<??>>"
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^? _Base64UrlUnpadded
-- Just "<<??>>"
--
_Base64UrlUnpadded :: Prism' Text Text
_Base64UrlUnpadded = prism' B64TU.encodeBase64Unpadded $ \s -> case B64TU.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}

-- | An 'Iso'' into the Base64 encoding of a 'Text' value
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
_Base64Lenient :: Iso' Text Text
_Base64Lenient = iso B64T.decodeBase64Lenient B64T.encodeBase64

-- | An 'Iso'' into the Base64url encoding of a 'Text' value
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
_Base64UrlLenient :: Iso' Text Text
_Base64UrlLenient = iso B64TU.decodeBase64Lenient B64TU.encodeBase64

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Unidirectional pattern synonym for base64-encoded 'Text' values.
--
pattern Base64 :: Text -> Text
pattern Base64 a <- (preview _Base64 -> Just a) where
    Base64 a = _Base64 # a

-- | Unidirectional pattern synonym for base64url-encoded 'Text' values.
--
pattern Base64Url :: Text -> Text
pattern Base64Url a <- (preview _Base64Url -> Just a) where
    Base64Url a = _Base64Url # a

-- | Unidirectional pattern synonym for unpadded base64url-encoded 'Text' values.
--
pattern Base64UrlUnpadded :: Text -> Text
pattern Base64UrlUnpadded a <- (preview _Base64UrlUnpadded -> Just a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a

-- | Bidirectional pattern synonym for leniently Base64-encoded 'Text' values
--
pattern Base64Lenient :: Text -> Text
pattern Base64Lenient a <- (view (from _Base64Lenient) -> a) where
    Base64Lenient a = view _Base64Lenient a
{-# COMPLETE Base64Lenient #-}

-- | Bidirectional pattern synonym for leniently Base64-encoded 'Text' values
--
pattern Base64UrlLenient :: Text -> Text
pattern Base64UrlLenient a <- (view (from _Base64UrlLenient) -> a) where
    Base64UrlLenient a = view _Base64UrlLenient a
{-# COMPLETE Base64UrlLenient #-}
