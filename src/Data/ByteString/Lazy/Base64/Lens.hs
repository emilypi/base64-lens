{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#if MIN_VERSION_lens(5,0,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module       : Data.ByteString.Lazy.Base64.Lens
-- Copyright 	: (c) 2019-2021 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism''s and 'Iso''s for Base64-encoding and
-- decoding 'ByteString' values.
--
module Data.ByteString.Lazy.Base64.Lens
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

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base64 as BL64
import qualified Data.ByteString.Lazy.Base64.URL as BL64U


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.ByteString.Lazy.Base64.Lens
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
_Base64 = prism' BL64.encodeBase64' $ \s -> case BL64.decodeBase64 s of
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
_Base64Url = prism' BL64U.encodeBase64' $ \s -> case BL64U.decodeBase64 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64Url #-}

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
_Base64UrlUnpadded = prism' BL64U.encodeBase64Unpadded' $ \s -> case BL64U.decodeBase64Unpadded s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base64UrlUnpadded #-}

-- | An 'Iso'' into the Base64 encoding of a 'ByteString' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso'. Please take care!
--
-- >>> _Base64Lenient # "Sun"
-- "U3Vu"
--
-- >>> "U3Vu" ^. _Base64Lenient
-- "Sun"
--
_Base64Lenient :: Iso' ByteString ByteString
_Base64Lenient = iso BL64.decodeBase64Lenient BL64.encodeBase64'

-- | An 'Iso'' into the Base64url encoding of a 'ByteString' value
-- using lenient decoding.
--
--
-- _Note:_ This is not a lawful 'Iso'. Please take care!
--
-- >>> _Base64UrlLenient # "<<??>>"
-- "PDw_Pz4-"
--
-- >>> "PDw_Pz4-" ^. _Base64UrlLenient
-- "<<??>>"
--
_Base64UrlLenient :: Iso' ByteString ByteString
_Base64UrlLenient = iso BL64U.decodeBase64Lenient BL64U.encodeBase64'

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

-- | Bidirectional pattern synonym for unpadded base64url-encoded 'ByteString' values.
--
pattern Base64UrlUnpadded :: ByteString -> ByteString
pattern Base64UrlUnpadded a <- (preview _Base64UrlUnpadded -> Just a) where
    Base64UrlUnpadded a = _Base64UrlUnpadded # a

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
