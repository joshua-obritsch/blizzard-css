-- | Module    : Css.ValueTypes
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.ValueTypes" module provides a set of types for representing CSS value types.
module Css.ValueTypes
    ( -- ** Color
      Color
      -- ** Colorspace
    , Colorspace

      -- * Classes
      -- ** NumberAngleNone
    , NumberAngleNone
      -- ** NumberPercentageNone
    , NumberPercentageNone
      -- ** PercentageNone
    , PercentageNone
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- TYPES


-- | Represents a CSS \<color\> value type.
newtype Color = Color { unColor :: Builder }


instance Buildable Color where build = unColor
instance Show      Color where show  = lazyShow


-- | Represents a CSS \<colorspace\> value type.
newtype Colorspace = Colorspace { unColorspace :: Builder }


instance Buildable Colorspace where build = unColorspace
instance Show      Colorspace where show  = lazyShow


-- CLASSES


-- | Represents a CSS \<number\>|\<angle\>|none value type.
class NumberAngleNone a where unNumberAngleNone :: a -> Builder


instance NumberAngleNone Percentage where unNumberAngleNone = build
instance NumberAngleNone None       where unNumberAngleNone = build


-- | Represents a CSS \<number\>|\<percentage\>|none value type.
class NumberPercentageNone a where unNumberPercentageNone :: a -> Builder


instance NumberPercentageNone Percentage where unNumberPercentageNone = build
instance NumberPercentageNone None       where unNumberPercentageNone = build


-- | Represents a CSS \<percentage\>|none value type.
class PercentageNone a where unPercentageNone :: a -> Builder


instance PercentageNone Percentage where unPercentageNone = build
instance PercentageNone None       where unPercentageNone = build
