{-# LANGUAGE CPP #-}
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
-- This module contains 'Prism''s for Base64-encoding and
-- decoding 'ByteString' values.
--
module Data.ByteString.Base64.Lens
( -- * Prisms
  _Base64
, _Base64Url
, _Base64Unpadded
, _Base64UrlUnpadded
#if MIN_VERSION_base64(0,3,0)
, _Base64Lenient
, _Base64UrlLenient
#endif
  -- * Patterns
, pattern Base64
, pattern Base64Url
, pattern Base64Unpadded
, pattern Base64UrlUnpadded
#if MIN_VERSION_base64(0,3,0)
, pattern Base64Lenient
, pattern Base64UrlLenient
#endif
) where


import Control.Lens

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.ByteString.Base64.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base64 encoding of a 'ByteString' value
--
-- >>> _Base64 # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^? _Base64
-- Just "Sun"
--
_Base64 :: Prism' ByteString ByteString
_Base64 = prism' B64.encodeBase64' $ \s -> case B64.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64 #-}

-- | A 'Prism'' into the Base64url encoding of a 'ByteString' value
--
-- >>> _Base64Url # "Sun"
-- "U3Vu"
--
-- >>> "PDw_Pz8-Pg==" ^? _Base64Url
-- Just "<<???>>"
--
_Base64Url :: Prism' ByteString ByteString
_Base64Url = prism' B64U.encodeBase64' $ \s -> case B64U.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Url #-}

-- | A 'Prism'' into the unpadded Base64 encoding of a
-- 'ByteString' value
--
-- Please note that unpadded variants should only be used
-- when assumptions about the data can be made. In particular, if the length of
-- the input is divisible by 3, then this is a safe function to call.
--
-- >>> _Base64Unpadded # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^? _Base64Unpadded
-- Just "Sun"
--
_Base64Unpadded :: Prism' ByteString ByteString
_Base64Unpadded = prism' B64.encodeBase64Unpadded' $ \s -> case B64.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Unpadded #-}

-- | A 'Prism'' into the Base64url encoding of a 'ByteString' value
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
_Base64UrlUnpadded :: Prism' ByteString ByteString
_Base64UrlUnpadded = prism' B64U.encodeBase64Unpadded' $ \s -> case B64U.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}

#if MIN_VERSION_base64(0,3,0)
-- | An 'Iso'' into the Base64 encoding of a 'ByteString' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso'.
--
-- >>> "Sun" ^. _Base64Lenient
-- "U3Vu"
--
-- >>> "U3Vu" ^. from _Base64Lenient
-- "Sun"
--
_Base64Lenient :: Iso' ByteString ByteString
_Base64Lenient = iso B64.encodeBase64' B64.decodeBase64Lenient

-- | An 'Iso'' into the Base64url encoding of a 'ByteString' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso'.
--
-- >>> "<<??>>" ^. _Base64UrlLenient
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^. from _Base64UrlLenient
-- "<<??>>"
--
_Base64UrlLenient :: Iso' ByteString ByteString
_Base64UrlLenient = iso B64U.encodeBase64' B64U.decodeBase64Lenient
#endif

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for base64-encoded 'ByteString' values.
--
pattern Base64 :: ByteString -> ByteString
pattern Base64 a <- (preview _Base64 -> Just a) where
    Base64 a = _Base64 # a

-- | Bidirectional pattern synonym for base64url-encoded 'ByteString' values.
--
pattern Base64Url :: ByteString -> ByteString
pattern Base64Url a <- (preview _Base64Url -> Just a) where
    Base64Url a = _Base64Url # a

-- | Bidirectional pattern synonym for unpadded base64-encoded 'ByteString' values.
--
pattern Base64Unpadded :: ByteString -> ByteString
pattern Base64Unpadded a <- (preview _Base64Unpadded -> Just a) where
    Base64Unpadded a = _Base64Unpadded # a

-- | Bidirectional pattern synonym for unpadded base64url-encoded 'ByteString' values.
--
pattern Base64UrlUnpadded :: ByteString -> ByteString
pattern Base64UrlUnpadded a <- (preview _Base64UrlUnpadded -> Just a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a

#if MIN_VERSION_base64(0,3,0)
-- | Bidirectional pattern synonym for leniently Base64-encoded 'ByteString' values
--
pattern Base64Lenient :: ByteString -> ByteString
pattern Base64Lenient a <- (view (from _Base64Lenient) -> a) where
    Base64Lenient a = view _Base64Lenient a
{-# COMPLETE Base64Lenient #-}

-- | Bidirectional pattern synonym for leniently Base64-encoded 'ByteString' values
--
pattern Base64UrlLenient :: ByteString -> ByteString
pattern Base64UrlLenient a <- (view (from _Base64UrlLenient) -> a) where
    Base64UrlLenient a = view _Base64UrlLenient a
{-# COMPLETE Base64UrlLenient #-}
#endif
