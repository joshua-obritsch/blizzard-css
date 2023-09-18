{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Units
-- Copyright   : (c) Joshua Obritsch, 2022
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

import Data.Text.Lazy.Builder           (Builder, singleton)
import Data.Text.Lazy.Builder.Int       (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)


-- UNITS


-- | Generates a CSS @ch@ unit.
ch :: Builder -> Builder
ch = units "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ unit.
cm :: Builder -> Builder
cm = units "cm"
{-# INLINE cm #-}


-- | Generates a CSS @deg@ unit.
deg :: Builder -> Builder
deg = units "deg"
{-# INLINE deg #-}


-- | Generates a CSS @em@ unit.
em :: Builder -> Builder
em = units "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ unit.
ex :: Builder -> Builder
ex = units "ex"
{-# INLINE ex #-}


-- | Generates a CSS @grad@ unit.
grad :: Builder -> Builder
grad = units "grad"
{-# INLINE grad #-}


-- | Generates a CSS @in@ unit.
in_ :: Builder -> Builder
in_ = units "in"
{-# INLINE in_ #-}


-- | Generates a CSS @mm@ unit.
mm :: Builder -> Builder
mm = units "mm"
{-# INLINE mm #-}


-- | Generates a CSS @ms@ unit.
ms :: Builder -> Builder
ms = units "ms"
{-# INLINE ms #-}


-- | Generates a CSS @pc@ unit.
pc :: Builder -> Builder
pc = units "pc"
{-# INLINE pc #-}


-- | Generates a CSS @\%@ unit.
pct :: Builder -> Builder
pct = unitc '%'
{-# INLINE pct #-}


-- | Generates a CSS @pt@ unit.
pt :: Builder -> Builder
pt = units "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ unit.
px :: Builder -> Builder
px = units "px"
{-# INLINE px #-}


-- | Generates a CSS @q@ unit.
q :: Builder -> Builder
q = units "q"
{-# INLINE q #-}


-- | Generates a CSS @rad@ unit.
rad :: Builder -> Builder
rad = units "rad"
{-# INLINE rad #-}


-- | Generates a CSS @rem@ unit.
rem :: Builder -> Builder
rem = units "rem"
{-# INLINE rem #-}


-- | Generates a CSS @s@ unit.
s :: Builder -> Builder
s = unitc 's'
{-# INLINE s #-}


-- | Generates a CSS @turn@ unit.
turn :: Builder -> Builder
turn = units "turn"
{-# INLINE turn #-}


-- | Generates a CSS @vh@ unit.
vh :: Builder -> Builder
vh = units "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vmax@ unit.
vmax :: Builder -> Builder
vmax = units "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ unit.
vmin :: Builder -> Builder
vmin = units "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ unit.
vw :: Builder -> Builder
vw = units "vw"
{-# INLINE vw #-}


-- HELPER FUNCTIONS


unitc :: Char -> Builder -> Builder
unitc suffix value = value <> singleton suffix


units :: Builder -> Builder -> Builder
units suffix value = value <> suffix
