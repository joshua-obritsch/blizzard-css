{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Numeric
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Numeric" module provides a set of functions for generating CSS numeric types.
module Css.Numeric
    ( -- * Types
      -- ** Angle
      Angle
      -- ** Length
    , Length
      -- ** Percentage
    , Percentage
      -- ** Resolution
    , Resolution
      -- ** Time
    , Time

      -- * Units

      -- ** \<angle\>
      -- *** deg
    , deg
      -- *** grad
    , grad
      -- *** rad
    , rad
      -- *** turn
    , turn

      -- ** \<length\>
      -- *** ch
    , ch
      -- *** cm
    , cm
      -- *** cqb
    , cqb
      -- *** cqh
    , cqh
      -- *** cqi
    , cqi
      -- *** cqmax
    , cqmax
      -- *** cqmin
    , cqmin
      -- *** cqw
    , cqw
      -- *** dvb
    , dvb
      -- *** dvh
    , dvh
      -- *** dvi
    , dvi
      -- *** dvmax
    , dvmax
      -- *** dvmin
    , dvmin
      -- *** dvw
    , dvw
      -- *** em
    , em
      -- *** ex
    , ex
      -- *** ic
    , ic
      -- *** in
    , in_
      -- *** lh
    , lh
      -- *** lvb
    , lvb
      -- *** lvh
    , lvh
      -- *** lvi
    , lvi
      -- *** lvmax
    , lvmax
      -- *** lvmin
    , lvmin
      -- *** lvw
    , lvw
      -- *** mm
    , mm
      -- *** pc
    , pc
      -- *** pt
    , pt
      -- *** px
    , px
      -- *** Q
    , q
      -- *** rem
    , rem
      -- *** rlh
    , rlh
      -- *** svb
    , svb
      -- *** svh
    , svh
      -- *** svi
    , svi
      -- *** svmax
    , svmax
      -- *** svmin
    , svmin
      -- *** svw
    , svw
      -- *** vb
    , vb
      -- *** vh
    , vh
      -- *** vi
    , vi
      -- *** vmax
    , vmax
      -- *** vmin
    , vmin
      -- *** vw
    , vw

      -- ** \<percentage\>
    , pct

      -- ** \<resolution\>
      -- *** dpcm
    , dpcm
      -- *** dpi
    , dpi
      -- *** dppx
    , dppx
      -- *** x
    , x

      -- ** \<time\>
      -- *** ms
    , ms
      -- *** s
    , s
    ) where


import Prelude hiding (rem)

import Css.Internal           (fromDouble, lazyShow)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- TYPES


newtype Angle = Angle { unAngle :: Builder }


instance Buildable Angle where build = unAngle
instance Show      Angle where show  = lazyShow


newtype Length = Length { unLength :: Builder }


instance Buildable Length where build = unLength
instance Show      Length where show  = lazyShow


newtype Percentage = Percentage { unPercentage :: Builder }


instance Buildable Percentage where build = unPercentage
instance Show      Percentage where show  = lazyShow


newtype Resolution = Resolution { unResolution :: Builder }


instance Buildable Resolution where build = unResolution
instance Show      Resolution where show  = lazyShow


newtype Time = Time { unTime :: Builder }


instance Buildable Time where build = unTime
instance Show      Time where show  = lazyShow


-- ANGLE UNITS


-- | Generates a CSS @deg@ \<angle\> unit.
deg :: Double -> Angle
deg = Angle . fromDouble "deg"
{-# INLINE deg #-}


-- | Generates a CSS @grad@ \<angle\> unit.
grad :: Double -> Angle
grad = Angle . fromDouble "grad"
{-# INLINE grad #-}


-- | Generates a CSS @rad@ \<angle\> unit.
rad :: Double -> Angle
rad = Angle . fromDouble "rad"
{-# INLINE rad #-}


-- | Generates a CSS @turn@ \<angle\> unit.
turn :: Double -> Angle
turn = Angle . fromDouble "turn"
{-# INLINE turn #-}


-- LENGTH UNITS


-- | Generates a CSS @ch@ \<length\> unit.
ch :: Double -> Length
ch = Length . fromDouble "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ \<length\> unit.
cm :: Double -> Length
cm = Length . fromDouble "cm"
{-# INLINE cm #-}


-- | Generates a CSS @cqb@ \<length\> unit.
cqb :: Double -> Length
cqb = Length . fromDouble "cqb"
{-# INLINE cqb #-}


-- | Generates a CSS @cqh@ \<length\> unit.
cqh :: Double -> Length
cqh = Length . fromDouble "cqh"
{-# INLINE cqh #-}


-- | Generates a CSS @cqi@ \<length\> unit.
cqi :: Double -> Length
cqi = Length . fromDouble "cqi"
{-# INLINE cqi #-}


-- | Generates a CSS @cqmax@ \<length\> unit.
cqmax :: Double -> Length
cqmax = Length . fromDouble "cqmax"
{-# INLINE cqmax #-}


-- | Generates a CSS @cqmin@ \<length\> unit.
cqmin :: Double -> Length
cqmin = Length . fromDouble "cqmin"
{-# INLINE cqmin #-}


-- | Generates a CSS @cqw@ \<length\> unit.
cqw :: Double -> Length
cqw = Length . fromDouble "cqw"
{-# INLINE cqw #-}


-- | Generates a CSS @dvb@ \<length\> unit.
dvb :: Double -> Length
dvb = Length . fromDouble "dvb"
{-# INLINE dvb #-}


-- | Generates a CSS @dvh@ \<length\> unit.
dvh :: Double -> Length
dvh = Length . fromDouble "dvh"
{-# INLINE dvh #-}


-- | Generates a CSS @dvi@ \<length\> unit.
dvi :: Double -> Length
dvi = Length . fromDouble "dvi"
{-# INLINE dvi #-}


-- | Generates a CSS @dvmax@ \<length\> unit.
dvmax :: Double -> Length
dvmax = Length . fromDouble "dvmax"
{-# INLINE dvmax #-}


-- | Generates a CSS @dvmin@ \<length\> unit.
dvmin :: Double -> Length
dvmin = Length . fromDouble "dvmin"
{-# INLINE dvmin #-}


-- | Generates a CSS @dvw@ \<length\> unit.
dvw :: Double -> Length
dvw = Length . fromDouble "dvw"
{-# INLINE dvw #-}


-- | Generates a CSS @em@ \<length\> unit.
em :: Double -> Length
em = Length . fromDouble "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ \<length\> unit.
ex :: Double -> Length
ex = Length . fromDouble "ex"
{-# INLINE ex #-}


-- | Generates a CSS @ic@ \<length\> unit.
ic :: Double -> Length
ic = Length . fromDouble "ic"
{-# INLINE ic #-}


-- | Generates a CSS @in@ \<length\> unit.
in_ :: Double -> Length
in_ = Length . fromDouble "in"
{-# INLINE in_ #-}


-- | Generates a CSS @lh@ \<length\> unit.
lh :: Double -> Length
lh = Length . fromDouble "lh"
{-# INLINE lh #-}


-- | Generates a CSS @lvb@ \<length\> unit.
lvb :: Double -> Length
lvb = Length . fromDouble "lvb"
{-# INLINE lvb #-}


-- | Generates a CSS @lvh@ \<length\> unit.
lvh :: Double -> Length
lvh = Length . fromDouble "lvh"
{-# INLINE lvh #-}


-- | Generates a CSS @lvi@ \<length\> unit.
lvi :: Double -> Length
lvi = Length . fromDouble "lvi"
{-# INLINE lvi #-}


-- | Generates a CSS @lvmax@ \<length\> unit.
lvmax :: Double -> Length
lvmax = Length . fromDouble "lvmax"
{-# INLINE lvmax #-}


-- | Generates a CSS @lvmin@ \<length\> unit.
lvmin :: Double -> Length
lvmin = Length . fromDouble "lvmin"
{-# INLINE lvmin #-}


-- | Generates a CSS @lvw@ \<length\> unit.
lvw :: Double -> Length
lvw = Length . fromDouble "lvw"
{-# INLINE lvw #-}


-- | Generates a CSS @mm@ \<length\> unit.
mm :: Double -> Length
mm = Length . fromDouble "mm"
{-# INLINE mm #-}


-- | Generates a CSS @pc@ \<length\> unit.
pc :: Double -> Length
pc = Length . fromDouble "pc"
{-# INLINE pc #-}


-- | Generates a CSS @pt@ \<length\> unit.
pt :: Double -> Length
pt = Length . fromDouble "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ \<length\> unit.
px :: Double -> Length
px = Length . fromDouble "px"
{-# INLINE px #-}


-- | Generates a CSS @Q@ \<length\> unit.
q :: Double -> Length
q = Length . fromDouble "Q"
{-# INLINE q #-}


-- | Generates a CSS @rem@ \<length\> unit.
rem :: Double -> Length
rem = Length . fromDouble "rem"
{-# INLINE rem #-}


-- | Generates a CSS @rlh@ \<length\> unit.
rlh :: Double -> Length
rlh = Length . fromDouble "rlh"
{-# INLINE rlh #-}


-- | Generates a CSS @svb@ \<length\> unit.
svb :: Double -> Length
svb = Length . fromDouble "svb"
{-# INLINE svb #-}


-- | Generates a CSS @svh@ \<length\> unit.
svh :: Double -> Length
svh = Length . fromDouble "svh"
{-# INLINE svh #-}


-- | Generates a CSS @svi@ \<length\> unit.
svi :: Double -> Length
svi = Length . fromDouble "svi"
{-# INLINE svi #-}


-- | Generates a CSS @svmax@ \<length\> unit.
svmax :: Double -> Length
svmax = Length . fromDouble "svmax"
{-# INLINE svmax #-}


-- | Generates a CSS @svmin@ \<length\> unit.
svmin :: Double -> Length
svmin = Length . fromDouble "svmin"
{-# INLINE svmin #-}


-- | Generates a CSS @svw@ \<length\> unit.
svw :: Double -> Length
svw = Length . fromDouble "svw"
{-# INLINE svw #-}


-- | Generates a CSS @vb@ \<length\> unit.
vb :: Double -> Length
vb = Length . fromDouble "vb"
{-# INLINE vb #-}


-- | Generates a CSS @vh@ \<length\> unit.
vh :: Double -> Length
vh = Length . fromDouble "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vi@ \<length\> unit.
vi :: Double -> Length
vi = Length . fromDouble "vi"
{-# INLINE vi #-}


-- | Generates a CSS @vmax@ \<length\> unit.
vmax :: Double -> Length
vmax = Length . fromDouble "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ \<length\> unit.
vmin :: Double -> Length
vmin = Length . fromDouble "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ \<length\> unit.
vw :: Double -> Length
vw = Length . fromDouble "vw"
{-# INLINE vw #-}


-- PERCENTAGE UNITS


-- | Generates a CSS @\%@ \<percentage\> unit.
pct :: Double -> Percentage
pct = Percentage . fromDouble "%"


-- RESOLUTION UNITS


-- | Generates a CSS @dpcm@ \<resolution\> unit.
dpcm :: Double -> Resolution
dpcm = Resolution . fromDouble "dpcm"
{-# INLINE dpcm #-}


-- | Generates a CSS @dpi@ \<resolution\> unit.
dpi :: Double -> Resolution
dpi = Resolution . fromDouble "dpi"
{-# INLINE dpi #-}


-- | Generates a CSS @dppx@ \<resolution\> unit.
dppx :: Double -> Resolution
dppx = Resolution . fromDouble "dppx"
{-# INLINE dppx #-}


-- | Functions as an alias for 'Css.Types.dppx'.
x :: Double -> Resolution
x = dppx
{-# INLINE x #-}


-- TIME UNITS


-- | Generates a CSS @ms@ \<time\> unit.
ms :: Double -> Time
ms = Time . fromDouble "ms"
{-# INLINE ms #-}


-- | Generates a CSS @s@ \<time\> unit.
s :: Double -> Time
s = Time . fromDouble "s"
{-# INLINE s #-}
