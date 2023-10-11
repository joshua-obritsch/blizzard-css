{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module    : Css.DataTypes.Filter
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Filter" module provides a set of types and functions for generating filter-related data types in CSS.
module Css.DataTypes.Filter
    ( -- * Data Types
      -- ** \<filter-function\>
      FilterFunction
      -- ** \<number\>|\<percentage\>
    , NumberPercentage
      -- ** \<angle\>|\<zero\>
    , AngleZero

      -- * \<filter-function\>
      -- ** \<blur()\>
    , blur
      -- ** \<brightness()\>
    , brightness
      -- ** \<contrast()\>
    , contrast
      -- ** \<drop-shadow()\>
    , dropShadow
      -- ** \<grayscale()\>
    , grayscale
      -- ** \<hue-rotate()\>
    , hueRotate
      -- ** \<invert()\>
    , invert
      -- ** \<opacity()\>
    , opacity
      -- ** \<saturate()\>
    , saturate
      -- ** \<sepia()\>
    , sepia
    ) where


import Css.DataTypes.Color    (Color)
import Css.DataTypes.Numeric  (Angle, Length, Number, Percentage, Zero)
import Css.Internal
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<filter-function\>@ data type.
newtype FilterFunction = FilterFunction Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<number\>|\<percentage\>@ data type.
class Buildable a => NumberPercentage a


instance NumberPercentage Double
instance NumberPercentage Integer
instance NumberPercentage Percentage


-- | Represents the CSS @\<angle\>|\<zero\>@ data type.
class Buildable a => AngleZero a


instance NumberPercentage Angle
instance NumberPercentage Zero


-- * FILTER-FUNCTION


-- | Generates a CSS @\<blur()\>@ value.
blur :: Length -> FilterFunction
blur = FilterFunction . fromBuilder "blur("


-- | Generates a CSS @\<brightness()\>@ value.
brightness :: NumberPercentage a => a -> FilterFunction
brightness = FilterFunction . fromBuilder "brightness("


-- | Generates a CSS @\<contrast()\>@ value.
contrast :: NumberPercentage a => a -> FilterFunction
contrast = FilterFunction . fromBuilder "contrast("


-- | Generates a CSS @\<drop-shadow()\>@ value.
dropShadow :: Color -> Length -> Length -> Length -> FilterFunction
dropShadow color length1 length2 length3
    =  FilterFunction
    $  "drop-shadow("
    <> build color
    <> singleton ' '
    <> build length1
    <> singleton ' '
    <> build length2
    <> singleton ' '
    <> build length3
    <> singleton ')'


-- | Generates a CSS @\<grayscale()\>@ value.
grayscale :: NumberPercentage a => a -> FilterFunction
grayscale = FilterFunction . fromBuilder "grayscale("


-- | Generates a CSS @\<hue-rotate()\>@ value.
hueRotate :: AngleZero a => a -> FilterFunction
hueRotate = FilterFunction . fromBuilder "hue-rotate("


-- | Generates a CSS @\<invert()\>@ value.
invert :: NumberPercentage a => a -> FilterFunction
invert = FilterFunction . fromBuilder "invert("


-- | Generates a CSS @\<opacity()\>@ value.
opacity :: NumberPercentage a => a -> FilterFunction
opacity = FilterFunction . fromBuilder "opacity("


-- | Generates a CSS @\<saturate()\>@ value.
saturate :: NumberPercentage a => a -> FilterFunction
saturate = FilterFunction . fromBuilder "saturate("


-- | Generates a CSS @\<sepia()\>@ value.
sepia :: NumberPercentage a => a -> FilterFunction
sepia = FilterFunction . fromBuilder "sepia("


-- HELPER FUNCTIONS


fromBuilder :: Buildable a => Builder -> a -> Builder
fromBuilder prefix value = build prefix <> build value <> singleton ')'
