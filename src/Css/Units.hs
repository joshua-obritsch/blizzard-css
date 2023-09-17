{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Units
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Units" module provides a set of functions for generating CSS units.
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


import Prelude hiding (rem)

import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Text.Lazy.Builder.RealFloat (realFloat)


-- UNITS


-- | Generates a CSS @ch@ unit.
ch :: Double -> Builder
ch = units "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ unit.
cm :: Double -> Builder
cm = units "cm"
{-# INLINE cm #-}


-- | Generates a CSS @deg@ unit.
deg :: Double -> Builder
deg = units "deg"
{-# INLINE deg #-}


-- | Generates a CSS @em@ unit.
em :: Double -> Builder
em = units "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ unit.
ex :: Double -> Builder
ex = units "ex"
{-# INLINE ex #-}


-- | Generates a CSS @grad@ unit.
grad :: Double -> Builder
grad = units "grad"
{-# INLINE grad #-}


-- | Generates a CSS @in@ unit.
in_ :: Double -> Builder
in_ = units "in"
{-# INLINE in_ #-}


-- | Generates a CSS @mm@ unit.
mm :: Double -> Builder
mm = units "mm"
{-# INLINE mm #-}


-- | Generates a CSS @ms@ unit.
ms :: Double -> Builder
ms = units "ms"
{-# INLINE ms #-}


-- | Generates a CSS @pc@ unit.
pc :: Double -> Builder
pc = units "pc"
{-# INLINE pc #-}


-- | Generates a CSS @\%@ unit.
pct :: Double -> Builder
pct = unitc '%'
{-# INLINE pct #-}


-- | Generates a CSS @pt@ unit.
pt :: Double -> Builder
pt = units "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ unit.
px :: Double -> Builder
px = units "px"
{-# INLINE px #-}


-- | Generates a CSS @q@ unit.
q :: Double -> Builder
q = units "q"
{-# INLINE q #-}


-- | Generates a CSS @rad@ unit.
rad :: Double -> Builder
rad = units "rad"
{-# INLINE rad #-}


-- | Generates a CSS @rem@ unit.
rem :: Double -> Builder
rem = units "rem"
{-# INLINE rem #-}


-- | Generates a CSS @s@ unit.
s :: Double -> Builder
s = unitc 's'
{-# INLINE s #-}


-- | Generates a CSS @turn@ unit.
turn :: Double -> Builder
turn = units "turn"
{-# INLINE turn #-}


-- | Generates a CSS @vh@ unit.
vh :: Double -> Builder
vh = units "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vmax@ unit.
vmax :: Double -> Builder
vmax = units "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ unit.
vmin :: Double -> Builder
vmin = units "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ unit.
vw :: Double -> Builder
vw = units "vw"
{-# INLINE vw #-}


-- HELPER FUNCTIONS


unitc :: Char -> Double -> Builder
unitc suffix value = realFloat value <> singleton suffix


units :: Builder -> Double -> Builder
units suffix value = realFloat value <> suffix
