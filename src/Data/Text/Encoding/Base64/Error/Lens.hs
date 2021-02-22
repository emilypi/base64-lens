{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#if MIN_VERSION_lens(5,0,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module       : Data.Text.Encoding.Base64.Error.Lens
-- Copyright    : (c) 2019-2021 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- This module contains 'Prism''s for the `Data.Text.Encoding.Base64.Error.Base64Error`
-- datatype.
--
module Data.Text.Encoding.Base64.Error.Lens
( -- * Prisms
  _DecodeError
, _ConversionError
) where


import Control.Lens

import Data.Text (Text)
import Data.Text.Encoding.Base64.Error (Base64Error(..))


-- | A 'Prism'' into the 'DecodeError' case of a 'Base64Error'
--
_DecodeError :: Prism' (Base64Error err) Text
_DecodeError = prism' DecodeError $ \case
    DecodeError t -> Just t
    ConversionError{} -> Nothing

-- | A 'Prism'' into the 'ConversionError' case of a 'Base64Error'
--
_ConversionError :: Prism' (Base64Error err) err
_ConversionError = prism' ConversionError $ \case
    ConversionError err -> Just err
    DecodeError{} -> Nothing
