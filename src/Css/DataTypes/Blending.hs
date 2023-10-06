{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Blending
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Blending" module provides a set of types and functions for generating blending data types in CSS.
module Css.DataTypes.Blending
    ( -- * Data Types
      -- ** \<blend-mode\>
      BlendMode

      -- * \<blend-mode\>
      -- ** color
    , color
      -- ** color-burn
    , colorBurn
      -- ** color-dodge
    , colorDodge
      -- ** darken
    , darken
      -- ** difference
    , difference
      -- ** exclusion
    , exclusion
      -- ** hard-light
    , hardLight
      -- ** hue
    , hue
      -- ** lighten
    , lighten
      -- ** luminosity
    , luminosity
      -- ** multiply
    , multiply
      -- ** overlay
    , overlay
      -- ** saturation
    , saturation
      -- ** screen
    , screen
      -- ** soft-light
    , softLight

      -- * \<mix-blend-mode\>
      -- ** plus-darker
    , plusDarker
      -- ** plus-lighter
    , plusLighter
    ) where


import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<blend-mode\>@ data type.
newtype BlendMode = BlendMode Builder
    deriving (Buildable, Show)


-- * BLEND-MODE


-- | Generates the CSS @color@ @\<blend-mode\>@ value.
color :: BlendMode
color = BlendMode "color"
{-# INLINE color #-}


-- | Generates the CSS @color-burn@ @\<blend-mode\>@ value.
colorBurn :: BlendMode
colorBurn = BlendMode "color-burn"
{-# INLINE colorBurn #-}


-- | Generates the CSS @color-dodge@ @\<blend-mode\>@ value.
colorDodge :: BlendMode
colorDodge = BlendMode "color-dodge"
{-# INLINE colorDodge #-}


-- | Generates the CSS @darken@ @\<blend-mode\>@ value.
darken :: BlendMode
darken = BlendMode "darken"
{-# INLINE darken #-}


-- | Generates the CSS @difference@ @\<blend-mode\>@ value.
difference :: BlendMode
difference = BlendMode "difference"
{-# INLINE difference #-}


-- | Generates the CSS @exclusion@ @\<blend-mode\>@ value.
exclusion :: BlendMode
exclusion = BlendMode "exclusion"
{-# INLINE exclusion #-}


-- | Generates the CSS @hard-light@ @\<blend-mode\>@ value.
hardLight :: BlendMode
hardLight = BlendMode "hard-light"
{-# INLINE hardLight #-}


-- | Generates the CSS @hue@ @\<blend-mode\>@ value.
hue :: BlendMode
hue = BlendMode "hue"
{-# INLINE hue #-}


-- | Generates the CSS @lighten@ @\<blend-mode\>@ value.
lighten :: BlendMode
lighten = BlendMode "lighten"
{-# INLINE lighten #-}


-- | Generates the CSS @luminosity@ @\<blend-mode\>@ value.
luminosity :: BlendMode
luminosity = BlendMode "luminosity"
{-# INLINE luminosity #-}


-- | Generates the CSS @multiply@ @\<blend-mode\>@ value.
multiply :: BlendMode
multiply = BlendMode "multiply"
{-# INLINE multiply #-}


-- | Generates the CSS @overlay@ @\<blend-mode\>@ value.
overlay :: BlendMode
overlay = BlendMode "overlay"
{-# INLINE overlay #-}


-- | Generates the CSS @saturation@ @\<blend-mode\>@ value.
saturation :: BlendMode
saturation = BlendMode "saturation"
{-# INLINE saturation #-}


-- | Generates the CSS @screen@ @\<blend-mode\>@ value.
screen :: BlendMode
screen = BlendMode "screen"
{-# INLINE screen #-}


-- | Generates the CSS @soft-light@ @\<blend-mode\>@ value.
softLight :: BlendMode
softLight = BlendMode "soft-light"
{-# INLINE softLight #-}


-- * MIX-BLEND-MODE


-- | Generates the CSS @plus-darker@ @\<blend-mode\>@ value.
plusDarker :: BlendMode
plusDarker = BlendMode "plus-darker"
{-# INLINE plusDarker #-}


-- | Generates the CSS @plus-lighter@ @\<blend-mode\>@ value.
plusLighter :: BlendMode
plusLighter = BlendMode "plus-lighter"
{-# INLINE plusLighter #-}
