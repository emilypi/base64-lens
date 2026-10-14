{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base64.Error.Lens
-- Copyright    : (c) 2019-2026 Emily Pillmore
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


import Control.Lens (Prism, Prism', prism, prism')

import Data.Text (Text)
import Data.Text.Encoding.Base64.Error (Base64Error(..))


-- | A 'Prism'' into the 'DecodeError' case of a 'Base64Error'
--
_DecodeError :: Prism' (Base64Error err) Text
_DecodeError = prism' DecodeError $ \case
    DecodeError t -> Just t
    ConversionError{} -> Nothing

-- | Focus on the 'ConversionError' payload, allowing its type to change.
--
_ConversionError :: Prism (Base64Error a) (Base64Error b) a b
_ConversionError = prism ConversionError $ \case
    ConversionError err -> Right err
    DecodeError t -> Left (DecodeError t)
