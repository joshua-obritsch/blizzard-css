{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Units
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Units" module provides a set of functions for CSS units.
module Css.Units
    ( -- * Units
      -- ** ch
      ch
      -- ** cm
    , cm
      -- ** deg
    , deg
      -- ** em
    , em
      -- ** ex
    , ex
      -- ** grad
    , grad
      -- ** in
    , in_
      -- ** mm
    , mm
      -- ** ms
    , ms
      -- ** pc
    , pc
      -- ** percent
    , pct
      -- ** pt
    , pt
      -- ** px
    , px
      -- ** q
    , q
      -- ** rad
    , rad
      -- ** rem
    , rem
      -- ** s
    , s
      -- ** turn
    , turn
      -- ** vh
    , vh
      -- ** vmax
    , vmax
      -- ** vmin
    , vmin
      -- ** vw
    , vw
    ) where


import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.RealFloat (realFloat)


-- UNITS


-- | Generates a CSS @ch@ unit.
ch :: Double -> Builder
ch = unit "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ unit.
cm :: Double -> Builder
cm = unit "cm"
{-# INLINE cm #-}


-- | Generates a CSS @deg@ unit.
deg :: Double -> Builder
deg = unit "deg"
{-# INLINE deg #-}


-- | Generates a CSS @em@ unit.
em :: Double -> Builder
em = unit "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ unit.
ex :: Double -> Builder
ex = unit "ex"
{-# INLINE ex #-}


-- | Generates a CSS @grad@ unit.
grad :: Double -> Builder
grad = unit "grad"
{-# INLINE grad #-}


-- | Generates a CSS @in@ unit.
in_ :: Double -> Builder
in_ = unit "in"
{-# INLINE in_ #-}


-- | Generates a CSS @mm@ unit.
mm :: Double -> Builder
mm = unit "mm"
{-# INLINE mm #-}


-- | Generates a CSS @ms@ unit.
ms :: Double -> Builder
ms = unit "ms"
{-# INLINE ms #-}


-- | Generates a CSS @pc@ unit.
pc :: Double -> Builder
pc = unit "pc"
{-# INLINE pc #-}


-- | Generates a CSS @\%@ unit.
pct :: Double -> Builder
pct = unit "%"
{-# INLINE pct #-}


-- | Generates a CSS @pt@ unit.
pt :: Double -> Builder
pt = unit "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ unit.
px :: Double -> Builder
px = unit "px"
{-# INLINE px #-}


-- | Generates a CSS @q@ unit.
q :: Double -> Builder
q = unit "q"
{-# INLINE q #-}


-- | Generates a CSS @rad@ unit.
rad :: Double -> Builder
rad = unit "rad"
{-# INLINE rad #-}


-- | Generates a CSS @rem@ unit.
rem :: Double -> Builder
rem = unit "rem"
{-# INLINE rem #-}


-- | Generates a CSS @s@ unit.
s :: Double -> Builder
s = unit "s"
{-# INLINE s #-}


-- | Generates a CSS @turn@ unit.
turn :: Double -> Builder
turn = unit "turn"
{-# INLINE turn #-}


-- | Generates a CSS @vh@ unit.
vh :: Double -> Builder
vh = unit "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vmax@ unit.
vmax :: Double -> Builder
vmax = unit "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ unit.
vmin :: Double -> Builder
vmin = unit "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ unit.
vw :: Double -> Builder
vw = unit "vw"
{-# INLINE vw #-}


-- HELPER FUNCTIONS


unit :: Builder -> Double -> Builder
unit suffix value = realFloat value <> suffix
