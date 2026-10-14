{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module       : Data.Base64.Types.Lens
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- Optics for the 'Base64' wrapper and 'Alphabet' constructors. Importing this
-- module also supplies 'Wrapped' and 'Rewrapped' instances for 'Base64'.
-- The wrapper optic accesses encoded data; the string-specific modules
-- contain the encoding and decoding optics.
--
module Data.Base64.Types.Lens
( -- * Encoded payload
  _Base64
  -- * Alphabets
, _StdPadded
, _UrlPadded
, _UrlUnpadded
, _NonStandard
) where

import Control.Lens (Iso, Prism', Wrapped(..), Rewrapped, _Wrapped, iso, prism')
import Data.Base64.Types (Alphabet(..), Base64, assertBase64, extractBase64)

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Base64.Types
-- >>> import Data.Base64.Types.Lens
-- >>> :set -XDataKinds -XOverloadedStrings

-- | Access the encoded payload, preserving the alphabet index while allowing
-- the payload type to change. This is also available as '_Wrapped'.
--
-- Like 'assertBase64', wrapping does not encode or validate the value.
-- Use @parseBase64@ to validate untrusted standard-alphabet input. Changes
-- through this optic must preserve the claimed encoding if it will be decoded.
--
-- >>> view _Base64 (assertBase64 "U3Vu" :: Base64 'StdPadded String)
-- "U3Vu"
--
_Base64 :: Iso (Base64 k a) (Base64 k b) a b
_Base64 = _Wrapped
{-# INLINE _Base64 #-}

-- | The encoded payload, without decoding or validation.
instance Wrapped (Base64 k a) where
    type Unwrapped (Base64 k a) = a
    _Wrapped' = iso extractBase64 assertBase64
    {-# INLINE _Wrapped' #-}

-- | Change the payload type without changing the alphabet index.
instance (t ~ Base64 k b) => Rewrapped (Base64 k a) t

-- | The standard, padded alphabet.
_StdPadded :: Prism' Alphabet ()
_StdPadded = prism' (const StdPadded) $ \case
    StdPadded -> Just ()
    _ -> Nothing
{-# INLINE _StdPadded #-}

-- | The padded URL-safe alphabet.
_UrlPadded :: Prism' Alphabet ()
_UrlPadded = prism' (const UrlPadded) $ \case
    UrlPadded -> Just ()
    _ -> Nothing
{-# INLINE _UrlPadded #-}

-- | The unpadded URL-safe alphabet.
_UrlUnpadded :: Prism' Alphabet ()
_UrlUnpadded = prism' (const UrlUnpadded) $ \case
    UrlUnpadded -> Just ()
    _ -> Nothing
{-# INLINE _UrlUnpadded #-}

-- | A non-standard encoding, suitable for lenient decoding only.
_NonStandard :: Prism' Alphabet ()
_NonStandard = prism' (const NonStandard) $ \case
    NonStandard -> Just ()
    _ -> Nothing
{-# INLINE _NonStandard #-}
