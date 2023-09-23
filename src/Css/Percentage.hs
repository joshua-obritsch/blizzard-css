{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Percentage
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Percentage" module provides a set of functions for generating CSS \<percentage\> units.
module Css.Percentage
    ( -- * Types
      -- ** Percentage
      Percentage(..)

      -- * Units
      -- ** \<percentage\>
      -- *** \%
    , pct
    ) where


import Prelude hiding (rem)

import Css.Internal           (fromDouble)
import Data.Text.Lazy         (unpack)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Html                   (Buildable(..))


-- TYPES


newtype Percentage = Percentage { unPercentage :: Builder }


instance Buildable Percentage where
    build = unPercentage


instance Show Percentage where
    show = unpack . toLazyText . build


-- UNITS


-- | Generates a CSS @\%@ \<percentage\> unit.
pct :: Double -> Percentage
pct = Percentage . fromDouble "%"
