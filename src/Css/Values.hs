{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Values
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Values" module provides a set of functions for generating CSS property values.
module Css.Values
    ( -- * Property Values
      -- ** auto
      --auto
      -- ** color
      color
      -- ** inherit
    , inherit
      -- ** initial
    , initial
    ) where


import Data.Text.Lazy.Builder (Builder)


-- PROPERTY VALUES


-- | Generates the CSS @alternate@ property value.
alternate :: Builder
alternate = "alternate"
{-# INLINE alternate #-}


-- | Generates the CSS @alternate-reverse@ property value.
alternateReverse :: Builder
alternateReverse = "alternate-reverse"
{-# INLINE alternateReverse #-}


-- | Generates the CSS @auto@ property value.
auto :: Builder
auto = "auto"
{-# INLINE auto #-}


-- | Generates the CSS @backwards@ property value.
backwards :: Builder
backwards = "backwards"
{-# INLINE backwards #-}


-- | Generates the CSS @baseline@ property value.
baseline :: Builder
baseline = "baseline"
{-# INLINE baseline #-}


-- | Generates the CSS @border-box@ property value.
borderBox :: Builder
borderBox = "border-box"
{-# INLINE borderBox #-}


-- | Generates the CSS @both@ property value.
both :: Builder
both = "both"
{-# INLINE both #-}


-- | Generates the CSS @center@ property value.
center :: Builder
center = "center"
{-# INLINE center #-}


-- | Generates the CSS @color@ property value.
color :: Builder
color = "color"
{-# INLINE color #-}


-- | Generates the CSS @color-dodge@ property value.
colorDodge :: Builder
colorDodge = "colorDodge"
{-# INLINE colorDodge #-}


-- | Generates the CSS @content-box@ property value.
contentBox :: Builder
contentBox = "content-box"
{-# INLINE contentBox #-}


-- | Generates the CSS @darken@ property value.
darken :: Builder
darken = "darken"
{-# INLINE darken #-}


-- | Generates the CSS @ease@ property value.
ease :: Builder
ease = "ease"
{-# INLINE ease #-}


-- | Generates the CSS @ease-in@ property value.
easeIn :: Builder
easeIn = "ease-in"
{-# INLINE easeIn #-}


-- | Generates the CSS @ease-in-out@ property value.
easeInOut :: Builder
easeInOut = "ease-in-out"
{-# INLINE easeInOut #-}


-- | Generates the CSS @ease-out@ property value.
easeOut :: Builder
easeOut = "ease-out"
{-# INLINE easeOut #-}


-- | Generates the CSS @end@ property value.
end :: Builder
end = "end"
{-# INLINE end #-}


-- | Generates the CSS @fixed@ property value.
fixed :: Builder
fixed = "fixed"
{-# INLINE fixed #-}


-- | Generates the CSS @flex-end@ property value.
flexEnd :: Builder
flexEnd = "flex-end"
{-# INLINE flexEnd #-}


-- | Generates the CSS @flex-start@ property value.
flexStart :: Builder
flexStart = "flex-start"
{-# INLINE flexStart #-}


-- | Generates the CSS @forwards@ property value.
forwards :: Builder
forwards = "forwards"
{-# INLINE forwards #-}


-- | Generates the CSS @hidden@ property value.
hidden :: Builder
hidden = "hidden"
{-# INLINE hidden #-}


-- | Generates the CSS @infinite@ property value.
infinite :: Builder
infinite = "infinite"
{-# INLINE infinite #-}


-- | Generates the CSS @inherit@ property value.
inherit :: Builder
inherit = "inherit"
{-# INLINE inherit #-}


-- | Generates the CSS @initial@ property value.
initial :: Builder
initial = "initial"
{-# INLINE initial #-}


-- | Generates the CSS @lighten@ property value.
lighten :: Builder
lighten = "lighten"
{-# INLINE lighten #-}


-- | Generates the CSS @linear@ property value.
linear :: Builder
linear = "linear"
{-# INLINE linear #-}


-- | Generates the CSS @local@ property value.
local :: Builder
local = "local"
{-# INLINE local #-}


-- | Generates the CSS @luminosity@ property value.
luminosity :: Builder
luminosity = "luminosity"
{-# INLINE luminosity #-}


-- | Generates the CSS @multiply@ property value.
multiply :: Builder
multiply = "multiply"
{-# INLINE multiply #-}


-- | Generates the CSS @none@ property value.
none :: Builder
none = "none"
{-# INLINE none #-}


-- | Generates the CSS @normal@ property value.
normal :: Builder
normal = "normal"
{-# INLINE normal #-}


-- | Generates the CSS @overlay@ property value.
overlay :: Builder
overlay = "overlay"
{-# INLINE overlay #-}


-- | Generates the CSS @padding-box@ property value.
paddingBox :: Builder
paddingBox = "padding-box"
{-# INLINE paddingBox #-}


-- | Generates the CSS @paused@ property value.
paused :: Builder
paused = "paused"
{-# INLINE paused #-}


-- | Generates the CSS @reverse@ property value.
reverse :: Builder
reverse = "reverse"
{-# INLINE reverse #-}


-- | Generates the CSS @running@ property value.
running :: Builder
running = "running"
{-# INLINE running #-}


-- | Generates the CSS @saturation@ property value.
saturation :: Builder
saturation = "saturation"
{-# INLINE saturation #-}


-- | Generates the CSS @screen@ property value.
screen :: Builder
screen = "screen"
{-# INLINE screen #-}


-- | Generates the CSS @scroll@ property value.
scroll :: Builder
scroll = "scroll"
{-# INLINE scroll #-}


-- | Generates the CSS @space-around@ property value.
spaceAround :: Builder
spaceAround = "space-around"
{-# INLINE spaceAround #-}


-- | Generates the CSS @space-between@ property value.
spaceBetween :: Builder
spaceBetween = "space-between"
{-# INLINE spaceBetween #-}


-- | Generates the CSS @space-evenly@ property value.
spaceEvenly :: Builder
spaceEvenly = "space-evenly"
{-# INLINE spaceEvenly #-}


-- | Generates the CSS @start@ property value.
start :: Builder
start = "start"
{-# INLINE start #-}


-- | Generates the CSS @step-end@ property value.
stepEnd :: Builder
stepEnd = "step-end"
{-# INLINE stepEnd #-}


-- | Generates the CSS @step-start@ property value.
stepStart :: Builder
stepStart = "step-start"
{-# INLINE stepStart #-}


-- | Generates the CSS @stretch@ property value.
stretch :: Builder
stretch = "stretch"
{-# INLINE stretch #-}


-- | Generates the CSS @transparent@ property value.
transparent :: Builder
transparent = "transparent"
{-# INLINE transparent #-}


-- | Generates the CSS @unset@ property value.
unset :: Builder
unset = "unset"
{-# INLINE unset #-}


-- | Generates the CSS @visible@ property value.
visible :: Builder
visible = "visible"
{-# INLINE visible #-}
