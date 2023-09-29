{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Numeric
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Numeric" module provides a set of types and functions for generating numeric data types in CSS.
module Css.DataTypes.Numeric
    ( -- * Data Types
      -- ** \<angle\>
      Angle
      -- ** \<angle-percentage\>
    , AnglePercentage
      -- ** \<dimension\>
    , Dimension
      -- ** \<frequency\>
    , Frequency
      -- ** \<frequency-percentage\>
    , FrequencyPercentage
      -- ** \<length\>
    , Length
      -- ** \<length-percentage\>
    , LengthPercentage
      -- ** \<number\>
    , Number
      -- ** \<percentage\>
    , Percentage
      -- ** \<resolution\>
    , Resolution
      -- ** \<time\>
    , Time
      -- ** \<time-percentage\>
    , TimePercentage

      -- * Percentages
      -- ** pct
    , pct

      -- * Distance Units

      -- ** Relative Lengths

      -- *** Font-relative Lengths
      -- **** cap
    , cap
      -- **** ch
    , ch
      -- **** em
    , em
      -- **** ex
    , ex
      -- **** ic
    , ic
      -- **** lh
    , lh
      -- **** rcap
    , rcap
      -- **** rch
    , rch
      -- **** rem
    , rem
      -- **** rex
    , rex
      -- **** ric
    , ric
      -- **** rlh
    , rlh

      -- *** Viewport-percentage Lengths
      -- **** dvb
    , dvb
      -- **** dvh
    , dvh
      -- **** dvi
    , dvi
      -- **** dvmax
    , dvmax
      -- **** dvmin
    , dvmin
      -- **** dvw
    , dvw
      -- **** lvb
    , lvb
      -- **** lvh
    , lvh
      -- **** lvi
    , lvi
      -- **** lvmax
    , lvmax
      -- **** lvmin
    , lvmin
      -- **** lvw
    , lvw
      -- **** svb
    , svb
      -- **** svh
    , svh
      -- **** svi
    , svi
      -- **** svmax
    , svmax
      -- **** svmin
    , svmin
      -- **** svw
    , svw
      -- **** vb
    , vb
      -- **** vh
    , vh
      -- **** vi
    , vi
      -- **** vmax
    , vmax
      -- **** vmin
    , vmin
      -- **** vw
    , vw

      -- ** Absolute Lengths
      -- *** cm
    , cm
      -- *** in
    , in_
      -- *** mm
    , mm
      -- *** pc
    , pc
      -- *** pt
    , pt
      -- *** px
    , px
      -- *** q
    , q

      -- * Other Units

      -- ** Angle Units
      -- *** deg
    , deg
      -- *** grad
    , grad
      -- *** rad
    , rad
      -- *** turn
    , turn

      -- ** Duration Units
      -- *** ms
    , ms
      -- *** s
    , s

      -- ** Frequency Units
      -- *** hz
    , hz
      -- *** khz
    , khz

      -- ** Resolution Units
      -- *** dpcm
    , dpcm
      -- *** dpi
    , dpi
      -- *** dppx
    , dppx
      -- *** x
    , x
    ) where


import Prelude hiding (rem)

import Css.Internal           (fromDouble)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))

import qualified Prelude


-- DATA TYPES


-- | Represents a CSS @\<angle\>@ data type.
newtype Angle = Angle Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<angle-percentage\>@ data type.
class Buildable a => AnglePercentage a


instance AnglePercentage Angle
instance AnglePercentage Percentage


-- | Represents a CSS @\<dimension\>@ data type.
class Buildable a => Dimension a


instance Dimension Angle
instance Dimension Frequency
instance Dimension Length
instance Dimension Resolution
instance Dimension Time


-- | Represents a CSS @\<frequency\>@ data type.
newtype Frequency = Frequency Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<frequency-percentage\>@ data type.
class Buildable a => FrequencyPercentage a


instance FrequencyPercentage Frequency
instance FrequencyPercentage Percentage


-- | Represents a CSS @\<length\>@ data type.
newtype Length = Length Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<length-percentage\>@ data type.
class Buildable a => LengthPercentage a


instance LengthPercentage Angle
instance LengthPercentage Percentage


-- | Represents a CSS @\<number\>@ data type.
type Number = Double


-- | Represents a CSS @\<percentage\>@ data type.
newtype Percentage = Percentage Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<resolution\>@ data type.
newtype Resolution = Resolution Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<time\>@ data type.
newtype Time = Time Builder
    deriving (Buildable, Show)


-- | Represents a CSS @\<time-percentage\>@ data type.
class Buildable a => TimePercentage a


instance TimePercentage Angle
instance TimePercentage Percentage

    {-
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
class Buildable a => AnglePercentageNone a where
    unAnglePercentageNone :: a -> Builder
    unAnglePercentageNone = build

instance {-# OVERLAPPING #-}                 AnglePercentageNone None
instance (Buildable a, AnglePercentage a) => AnglePercentageNone a

tmp :: AnglePercentage a => a -> Builder
tmp = build
-}


-- * PERCENTAGES


-- | Generates a CSS @\<percentage\>@ value.
pct :: Number -> Percentage
pct = Percentage . fromDouble "%"


-- * DISTANCE UNITS


-- ** RELATIVE LENGTHS


-- *** FONT-RELATIVE LENGTHS


-- | Generates a CSS @cap@ @\<length\>@ value.
cap :: Double -> Length
cap = Length . fromDouble "cap"
{-# INLINE cap #-}


-- | Generates a CSS @ch@ @\<length\>@ value.
ch :: Double -> Length
ch = Length . fromDouble "ch"
{-# INLINE ch #-}


-- | Generates a CSS @em@ @\<length\>@ value.
em :: Double -> Length
em = Length . fromDouble "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ @\<length\>@ value.
ex :: Double -> Length
ex = Length . fromDouble "ex"
{-# INLINE ex #-}


-- | Generates a CSS @ic@ @\<length\>@ value.
ic :: Double -> Length
ic = Length . fromDouble "ic"
{-# INLINE ic #-}


-- | Generates a CSS @lh@ @\<length\>@ value.
lh :: Double -> Length
lh = Length . fromDouble "lh"
{-# INLINE lh #-}


-- | Generates a CSS @rcap@ @\<length\>@ value.
rcap :: Double -> Length
rcap = Length . fromDouble "rcap"
{-# INLINE rcap #-}


-- | Generates a CSS @rch@ @\<length\>@ value.
rch :: Double -> Length
rch = Length . fromDouble "rch"
{-# INLINE rch #-}


-- | Generates a CSS @rem@ @\<length\>@ value.
rem :: Double -> Length
rem = Length . fromDouble "rem"
{-# INLINE rem #-}


-- | Generates a CSS @rex@ @\<length\>@ value.
rex :: Double -> Length
rex = Length . fromDouble "rex"
{-# INLINE rex #-}


-- | Generates a CSS @ric@ @\<length\>@ value.
ric :: Double -> Length
ric = Length . fromDouble "ric"
{-# INLINE ric #-}


-- | Generates a CSS @rlh@ @\<length\>@ value.
rlh :: Double -> Length
rlh = Length . fromDouble "rlh"
{-# INLINE rlh #-}


-- *** VIEWPORT-PERCENTAGE LENGTHS


-- | Generates a CSS @dvb@ @\<length\>@ value.
dvb :: Double -> Length
dvb = Length . fromDouble "dvb"
{-# INLINE dvb #-}


-- | Generates a CSS @dvh@ @\<length\>@ value.
dvh :: Double -> Length
dvh = Length . fromDouble "dvh"
{-# INLINE dvh #-}


-- | Generates a CSS @dvi@ @\<length\>@ value.
dvi :: Double -> Length
dvi = Length . fromDouble "dvi"
{-# INLINE dvi #-}


-- | Generates a CSS @dvmax@ @\<length\>@ value.
dvmax :: Double -> Length
dvmax = Length . fromDouble "dvmax"
{-# INLINE dvmax #-}


-- | Generates a CSS @dvmin@ @\<length\>@ value.
dvmin :: Double -> Length
dvmin = Length . fromDouble "dvmin"
{-# INLINE dvmin #-}


-- | Generates a CSS @dvw@ @\<length\>@ value.
dvw :: Double -> Length
dvw = Length . fromDouble "dvw"
{-# INLINE dvw #-}


-- | Generates a CSS @lvb@ @\<length\>@ value.
lvb :: Double -> Length
lvb = Length . fromDouble "lvb"
{-# INLINE lvb #-}


-- | Generates a CSS @lvh@ @\<length\>@ value.
lvh :: Double -> Length
lvh = Length . fromDouble "lvh"
{-# INLINE lvh #-}


-- | Generates a CSS @lvi@ @\<length\>@ value.
lvi :: Double -> Length
lvi = Length . fromDouble "lvi"
{-# INLINE lvi #-}


-- | Generates a CSS @lvmax@ @\<length\>@ value.
lvmax :: Double -> Length
lvmax = Length . fromDouble "lvmax"
{-# INLINE lvmax #-}


-- | Generates a CSS @lvmin@ @\<length\>@ value.
lvmin :: Double -> Length
lvmin = Length . fromDouble "lvmin"
{-# INLINE lvmin #-}


-- | Generates a CSS @lvw@ @\<length\>@ value.
lvw :: Double -> Length
lvw = Length . fromDouble "lvw"
{-# INLINE lvw #-}


-- | Generates a CSS @svb@ @\<length\>@ value.
svb :: Double -> Length
svb = Length . fromDouble "svb"
{-# INLINE svb #-}


-- | Generates a CSS @svh@ @\<length\>@ value.
svh :: Double -> Length
svh = Length . fromDouble "svh"
{-# INLINE svh #-}


-- | Generates a CSS @svi@ @\<length\>@ value.
svi :: Double -> Length
svi = Length . fromDouble "svi"
{-# INLINE svi #-}


-- | Generates a CSS @svmax@ @\<length\>@ value.
svmax :: Double -> Length
svmax = Length . fromDouble "svmax"
{-# INLINE svmax #-}


-- | Generates a CSS @svmin@ @\<length\>@ value.
svmin :: Double -> Length
svmin = Length . fromDouble "svmin"
{-# INLINE svmin #-}


-- | Generates a CSS @svw@ @\<length\>@ value.
svw :: Double -> Length
svw = Length . fromDouble "svw"
{-# INLINE svw #-}


-- | Generates a CSS @vb@ @\<length\>@ value.
vb :: Double -> Length
vb = Length . fromDouble "vb"
{-# INLINE vb #-}


-- | Generates a CSS @vh@ @\<length\>@ value.
vh :: Double -> Length
vh = Length . fromDouble "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vi@ @\<length\>@ value.
vi :: Double -> Length
vi = Length . fromDouble "vi"
{-# INLINE vi #-}


-- | Generates a CSS @vmax@ @\<length\>@ value.
vmax :: Double -> Length
vmax = Length . fromDouble "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ @\<length\>@ value.
vmin :: Double -> Length
vmin = Length . fromDouble "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ @\<length\>@ value.
vw :: Double -> Length
vw = Length . fromDouble "vw"
{-# INLINE vw #-}


-- ** ABSOLUTE LENGTHS


-- | Generates a CSS @cm@ @\<length\>@ value.
cm :: Double -> Length
cm = Length . fromDouble "cm"
{-# INLINE cm #-}


-- | Generates a CSS @in@ @\<length\>@ value.
in_ :: Double -> Length
in_ = Length . fromDouble "in"
{-# INLINE in_ #-}


-- | Generates a CSS @mm@ @\<length\>@ value.
mm :: Double -> Length
mm = Length . fromDouble "mm"
{-# INLINE mm #-}


-- | Generates a CSS @pc@ @\<length\>@ value.
pc :: Double -> Length
pc = Length . fromDouble "pc"
{-# INLINE pc #-}


-- | Generates a CSS @pt@ @\<length\>@ value.
pt :: Double -> Length
pt = Length . fromDouble "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ @\<length\>@ value.
px :: Double -> Length
px = Length . fromDouble "px"
{-# INLINE px #-}


-- | Generates a CSS @Q@ @\<length\>@ value.
q :: Double -> Length
q = Length . fromDouble "Q"
{-# INLINE q #-}


-- * OTHER UNITS


-- ** ANGLE UNITS


-- | Generates a CSS @deg@ @\<angle\>@ value.
deg :: Double -> Angle
deg = Angle . fromDouble "deg"
{-# INLINE deg #-}


-- | Generates a CSS @grad@ @\<angle\>@ value.
grad :: Double -> Angle
grad = Angle . fromDouble "grad"
{-# INLINE grad #-}


-- | Generates a CSS @rad@ @\<angle\>@ value.
rad :: Double -> Angle
rad = Angle . fromDouble "rad"
{-# INLINE rad #-}


-- | Generates a CSS @turn@ @\<angle\>@ value.
turn :: Double -> Angle
turn = Angle . fromDouble "turn"
{-# INLINE turn #-}


-- ** DURATION UNITS


-- | Generates a CSS @ms@ @\<time\>@ value.
ms :: Double -> Time
ms = Time . fromDouble "ms"
{-# INLINE ms #-}


-- | Generates a CSS @s@ @\<time\>@ value.
s :: Double -> Time
s = Time . fromDouble "s"
{-# INLINE s #-}


-- ** FREQUENCY UNITS


-- | Generates a CSS @Hz@ @\<frequency\>@ value.
hz :: Double -> Frequency
hz = Frequency . fromDouble "Hz"
{-# INLINE hz #-}


-- | Generates a CSS @kHz@ @\<frequency\>@ value.
khz :: Double -> Frequency
khz = Frequency . fromDouble "kHz"
{-# INLINE khz #-}


-- ** RESOLUTION UNITS


-- | Generates a CSS @dpcm@ @\<resolution\>@ value.
dpcm :: Double -> Resolution
dpcm = Resolution . fromDouble "dpcm"
{-# INLINE dpcm #-}


-- | Generates a CSS @dpi@ @\<resolution\>@ value.
dpi :: Double -> Resolution
dpi = Resolution . fromDouble "dpi"
{-# INLINE dpi #-}


-- | Generates a CSS @dppx@ @\<resolution\>@ value.
dppx :: Double -> Resolution
dppx = Resolution . fromDouble "dppx"
{-# INLINE dppx #-}


-- | Functions as an alias for 'Css.Types.dppx'.
x :: Double -> Resolution
x = dppx
{-# INLINE x #-}
