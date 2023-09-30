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

      -- * Absolute Lengths
      -- ** \'cm\'
    , cm
      -- ** \'in\'
    , in_
      -- ** \'mm\'
    , mm
      -- ** \'pc\'
    , pc
      -- ** \'pt\'
    , pt
      -- ** \'px\'
    , px
      -- ** \'Q\'
    , q

      -- * Angles
      -- ** \'deg\'
    , deg
      -- ** \'grad\'
    , grad
      -- ** \'rad\'
    , rad
      -- ** \'turn\'
    , turn

      -- * Durations
      -- ** \'ms\'
    , ms
      -- ** \'s\'
    , s

      -- * Font-relative Lengths
      -- ** \'cap\'
    , cap
      -- ** \'ch\'
    , ch
      -- ** \'em\'
    , em
      -- ** \'ex\'
    , ex
      -- ** \'ic\'
    , ic
      -- ** \'lh\'
    , lh
      -- ** \'rcap\'
    , rcap
      -- ** \'rch\'
    , rch
      -- ** \'rem\'
    , rem
      -- ** \'rex\'
    , rex
      -- ** \'ric\'
    , ric
      -- ** \'rlh\'
    , rlh

      -- * Frequencies
      -- ** \'Hz\'
    , hz
      -- ** \'kHz\'
    , khz

      -- * Percentages
      -- ** \'\%\'
    , pct

      -- * Resolutions
      -- ** \'dpcm\'
    , dpcm
      -- ** \'dpi\'
    , dpi
      -- ** \'dppx\'
    , dppx

      -- * Viewport-percentage Lengths
      -- ** \'dvb\'
    , dvb
      -- ** \'dvh\'
    , dvh
      -- ** \'dvi\'
    , dvi
      -- ** \'dvmax\'
    , dvmax
      -- ** \'dvmin\'
    , dvmin
      -- ** \'dvw\'
    , dvw
      -- ** \'lvb\'
    , lvb
      -- ** \'lvh\'
    , lvh
      -- ** \'lvi\'
    , lvi
      -- ** \'lvmax\'
    , lvmax
      -- ** \'lvmin\'
    , lvmin
      -- ** \'lvw\'
    , lvw
      -- ** \'svb\'
    , svb
      -- ** \'svh\'
    , svh
      -- ** \'svi\'
    , svi
      -- ** \'svmax\'
    , svmax
      -- ** \'svmin\'
    , svmin
      -- ** \'svw\'
    , svw
      -- ** \'vb\'
    , vb
      -- ** \'vh\'
    , vh
      -- ** \'vi\'
    , vi
      -- ** \'vmax\'
    , vmax
      -- ** \'vmin\'
    , vmin
      -- ** \'vw\'
    , vw
    ) where


import Prelude hiding (rem)

import Css.Internal
import Data.Text.Lazy.Builder           (Builder)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Html                             (Buildable(..))

import qualified Prelude


-- DATA TYPES


-- | Represents the CSS @\<angle\>@ data type.
newtype Angle = Angle Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<angle-percentage\>@ data type.
class Buildable a => AnglePercentage a


instance AnglePercentage Angle
instance AnglePercentage Percentage


-- | Represents the CSS @\<dimension\>@ data type.
class Buildable a => Dimension a


instance Dimension Angle
instance Dimension Frequency
instance Dimension Length
instance Dimension Resolution
instance Dimension Time


-- | Represents the CSS @\<frequency\>@ data type.
newtype Frequency = Frequency Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<frequency-percentage\>@ data type.
class Buildable a => FrequencyPercentage a


instance FrequencyPercentage Frequency
instance FrequencyPercentage Percentage


-- | Represents the CSS @\<length\>@ data type.
newtype Length = Length Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<length-percentage\>@ data type.
class Buildable a => LengthPercentage a


instance LengthPercentage Angle
instance LengthPercentage Percentage


-- | Represents the CSS @\<number\>@ data type.
type Number = Double


-- | Represents the CSS @\<percentage\>@ data type.
newtype Percentage = Percentage Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<resolution\>@ data type.
newtype Resolution = Resolution Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<time\>@ data type.
newtype Time = Time Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<time-percentage\>@ data type.
class Buildable a => TimePercentage a


instance TimePercentage Angle
instance TimePercentage Percentage


-- * ABSOLUTE LENGTHS


-- | Generates a CSS @cm@ @\<length\>@ value.
cm :: Number -> Length
cm = Length . fromNumber "cm"
{-# INLINE cm #-}


-- | Generates a CSS @in@ @\<length\>@ value.
in_ :: Number -> Length
in_ = Length . fromNumber "in"
{-# INLINE in_ #-}


-- | Generates a CSS @mm@ @\<length\>@ value.
mm :: Number -> Length
mm = Length . fromNumber "mm"
{-# INLINE mm #-}


-- | Generates a CSS @pc@ @\<length\>@ value.
pc :: Number -> Length
pc = Length . fromNumber "pc"
{-# INLINE pc #-}


-- | Generates a CSS @pt@ @\<length\>@ value.
pt :: Number -> Length
pt = Length . fromNumber "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ @\<length\>@ value.
px :: Number -> Length
px = Length . fromNumber "px"
{-# INLINE px #-}


-- | Generates a CSS @Q@ @\<length\>@ value.
q :: Number -> Length
q = Length . fromNumber "Q"
{-# INLINE q #-}


-- * ANGLE UNITS


-- | Generates a CSS @deg@ @\<angle\>@ value.
deg :: Number -> Angle
deg = Angle . fromNumber "deg"
{-# INLINE deg #-}


-- | Generates a CSS @grad@ @\<angle\>@ value.
grad :: Number -> Angle
grad = Angle . fromNumber "grad"
{-# INLINE grad #-}


-- | Generates a CSS @rad@ @\<angle\>@ value.
rad :: Number -> Angle
rad = Angle . fromNumber "rad"
{-# INLINE rad #-}


-- | Generates a CSS @turn@ @\<angle\>@ value.
turn :: Number -> Angle
turn = Angle . fromNumber "turn"
{-# INLINE turn #-}


-- * DURATION UNITS


-- | Generates a CSS @ms@ @\<time\>@ value.
ms :: Number -> Time
ms = Time . fromNumber "ms"
{-# INLINE ms #-}


-- | Generates a CSS @s@ @\<time\>@ value.
s :: Number -> Time
s = Time . fromNumber "s"
{-# INLINE s #-}


-- * FONT-RELATIVE LENGTHS


-- | Generates a CSS @cap@ @\<length\>@ value.
cap :: Number -> Length
cap = Length . fromNumber "cap"
{-# INLINE cap #-}


-- | Generates a CSS @ch@ @\<length\>@ value.
ch :: Number -> Length
ch = Length . fromNumber "ch"
{-# INLINE ch #-}


-- | Generates a CSS @em@ @\<length\>@ value.
em :: Number -> Length
em = Length . fromNumber "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ @\<length\>@ value.
ex :: Number -> Length
ex = Length . fromNumber "ex"
{-# INLINE ex #-}


-- | Generates a CSS @ic@ @\<length\>@ value.
ic :: Number -> Length
ic = Length . fromNumber "ic"
{-# INLINE ic #-}


-- | Generates a CSS @lh@ @\<length\>@ value.
lh :: Number -> Length
lh = Length . fromNumber "lh"
{-# INLINE lh #-}


-- | Generates a CSS @rcap@ @\<length\>@ value.
rcap :: Number -> Length
rcap = Length . fromNumber "rcap"
{-# INLINE rcap #-}


-- | Generates a CSS @rch@ @\<length\>@ value.
rch :: Number -> Length
rch = Length . fromNumber "rch"
{-# INLINE rch #-}


-- | Generates a CSS @rem@ @\<length\>@ value.
rem :: Number -> Length
rem = Length . fromNumber "rem"
{-# INLINE rem #-}


-- | Generates a CSS @rex@ @\<length\>@ value.
rex :: Number -> Length
rex = Length . fromNumber "rex"
{-# INLINE rex #-}


-- | Generates a CSS @ric@ @\<length\>@ value.
ric :: Number -> Length
ric = Length . fromNumber "ric"
{-# INLINE ric #-}


-- | Generates a CSS @rlh@ @\<length\>@ value.
rlh :: Number -> Length
rlh = Length . fromNumber "rlh"
{-# INLINE rlh #-}


-- * FREQUENCY UNITS


-- | Generates a CSS @Hz@ @\<frequency\>@ value.
hz :: Number -> Frequency
hz = Frequency . fromNumber "Hz"
{-# INLINE hz #-}


-- | Generates a CSS @kHz@ @\<frequency\>@ value.
khz :: Number -> Frequency
khz = Frequency . fromNumber "kHz"
{-# INLINE khz #-}


-- * PERCENTAGES


-- | Generates a CSS @\<percentage\>@ value.
pct :: Number -> Percentage
pct = Percentage . fromNumber "%"


-- * RESOLUTION UNITS


-- | Generates a CSS @dpcm@ @\<resolution\>@ value.
dpcm :: Number -> Resolution
dpcm = Resolution . fromNumber "dpcm"
{-# INLINE dpcm #-}


-- | Generates a CSS @dpi@ @\<resolution\>@ value.
dpi :: Number -> Resolution
dpi = Resolution . fromNumber "dpi"
{-# INLINE dpi #-}


-- | Generates a CSS @dppx@ @\<resolution\>@ value.
dppx :: Number -> Resolution
dppx = Resolution . fromNumber "dppx"
{-# INLINE dppx #-}


-- * VIEWPORT-PERCENTAGE LENGTHS


-- | Generates a CSS @dvb@ @\<length\>@ value.
dvb :: Number -> Length
dvb = Length . fromNumber "dvb"
{-# INLINE dvb #-}


-- | Generates a CSS @dvh@ @\<length\>@ value.
dvh :: Number -> Length
dvh = Length . fromNumber "dvh"
{-# INLINE dvh #-}


-- | Generates a CSS @dvi@ @\<length\>@ value.
dvi :: Number -> Length
dvi = Length . fromNumber "dvi"
{-# INLINE dvi #-}


-- | Generates a CSS @dvmax@ @\<length\>@ value.
dvmax :: Number -> Length
dvmax = Length . fromNumber "dvmax"
{-# INLINE dvmax #-}


-- | Generates a CSS @dvmin@ @\<length\>@ value.
dvmin :: Number -> Length
dvmin = Length . fromNumber "dvmin"
{-# INLINE dvmin #-}


-- | Generates a CSS @dvw@ @\<length\>@ value.
dvw :: Number -> Length
dvw = Length . fromNumber "dvw"
{-# INLINE dvw #-}


-- | Generates a CSS @lvb@ @\<length\>@ value.
lvb :: Number -> Length
lvb = Length . fromNumber "lvb"
{-# INLINE lvb #-}


-- | Generates a CSS @lvh@ @\<length\>@ value.
lvh :: Number -> Length
lvh = Length . fromNumber "lvh"
{-# INLINE lvh #-}


-- | Generates a CSS @lvi@ @\<length\>@ value.
lvi :: Number -> Length
lvi = Length . fromNumber "lvi"
{-# INLINE lvi #-}


-- | Generates a CSS @lvmax@ @\<length\>@ value.
lvmax :: Number -> Length
lvmax = Length . fromNumber "lvmax"
{-# INLINE lvmax #-}


-- | Generates a CSS @lvmin@ @\<length\>@ value.
lvmin :: Number -> Length
lvmin = Length . fromNumber "lvmin"
{-# INLINE lvmin #-}


-- | Generates a CSS @lvw@ @\<length\>@ value.
lvw :: Number -> Length
lvw = Length . fromNumber "lvw"
{-# INLINE lvw #-}


-- | Generates a CSS @svb@ @\<length\>@ value.
svb :: Number -> Length
svb = Length . fromNumber "svb"
{-# INLINE svb #-}


-- | Generates a CSS @svh@ @\<length\>@ value.
svh :: Number -> Length
svh = Length . fromNumber "svh"
{-# INLINE svh #-}


-- | Generates a CSS @svi@ @\<length\>@ value.
svi :: Number -> Length
svi = Length . fromNumber "svi"
{-# INLINE svi #-}


-- | Generates a CSS @svmax@ @\<length\>@ value.
svmax :: Number -> Length
svmax = Length . fromNumber "svmax"
{-# INLINE svmax #-}


-- | Generates a CSS @svmin@ @\<length\>@ value.
svmin :: Number -> Length
svmin = Length . fromNumber "svmin"
{-# INLINE svmin #-}


-- | Generates a CSS @svw@ @\<length\>@ value.
svw :: Number -> Length
svw = Length . fromNumber "svw"
{-# INLINE svw #-}


-- | Generates a CSS @vb@ @\<length\>@ value.
vb :: Number -> Length
vb = Length . fromNumber "vb"
{-# INLINE vb #-}


-- | Generates a CSS @vh@ @\<length\>@ value.
vh :: Number -> Length
vh = Length . fromNumber "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vi@ @\<length\>@ value.
vi :: Number -> Length
vi = Length . fromNumber "vi"
{-# INLINE vi #-}


-- | Generates a CSS @vmax@ @\<length\>@ value.
vmax :: Number -> Length
vmax = Length . fromNumber "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ @\<length\>@ value.
vmin :: Number -> Length
vmin = Length . fromNumber "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ @\<length\>@ value.
vw :: Number -> Length
vw = Length . fromNumber "vw"
{-# INLINE vw #-}


-- HELPER FUNCTIONS


fromNumber :: Builder -> Number -> Builder
fromNumber suffix value = realFloat value <> suffix
