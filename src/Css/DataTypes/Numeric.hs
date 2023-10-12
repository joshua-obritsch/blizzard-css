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
      -- ** \<frequency\>
    , Frequency
      -- ** \<length\>
    , Length
      -- ** \<number\>
    , Number
      -- ** \<number\>|\<percentage\>|none
    , NumberPercentageNone
      -- ** \<percentage\>
    , Percentage
      -- ** \<percentage\>|none
    , PercentageNone
      -- ** \<ratio\>
    , Ratio
    , RatioParams
      -- ** \<resolution\>
    , Resolution
      -- ** \<time\>
    , Time
      -- ** \<zero\>
    , Zero

      -- * \<angle\>
      -- ** deg
    , deg
      -- ** grad
    , grad
      -- ** rad
    , rad
      -- ** turn
    , turn

      -- * \<frequency\>
      -- ** Hz
    , hz
      -- ** kHz
    , khz

      -- * \<length\>
      -- ** cap
    , cap
      -- ** ch
    , ch
      -- ** cm
    , cm
      -- ** dvb
    , dvb
      -- ** dvh
    , dvh
      -- ** dvi
    , dvi
      -- ** dvmax
    , dvmax
      -- ** dvmin
    , dvmin
      -- ** dvw
    , dvw
      -- ** em
    , em
      -- ** ex
    , ex
      -- ** ic
    , ic
      -- ** in
    , in_
      -- ** lh
    , lh
      -- ** lvb
    , lvb
      -- ** lvh
    , lvh
      -- ** lvi
    , lvi
      -- ** lvmax
    , lvmax
      -- ** lvmin
    , lvmin
      -- ** lvw
    , lvw
      -- ** mm
    , mm
      -- ** pc
    , pc
      -- ** pt
    , pt
      -- ** px
    , px
      -- ** Q
    , q
      -- ** rcap
    , rcap
      -- ** rch
    , rch
      -- ** rem
    , rem
      -- ** rex
    , rex
      -- ** ric
    , ric
      -- ** rlh
    , rlh
      -- ** svb
    , svb
      -- ** svh
    , svh
      -- ** svi
    , svi
      -- ** svmax
    , svmax
      -- ** svmin
    , svmin
      -- ** svw
    , svw
      -- ** vb
    , vb
      -- ** vh
    , vh
      -- ** vi
    , vi
      -- ** vmax
    , vmax
      -- ** vmin
    , vmin
      -- ** vw
    , vw

      -- * \<percentage\>
    , pct

      -- * \<ratio\>
    , ratio

      -- * \<resolution\>
      -- ** dpcm
    , dpcm
      -- ** dpi
    , dpi
      -- ** dppx
    , dppx

      -- * \<time\>
      -- ** ms
    , ms
      -- ** s
    , s

      -- * \<zero\>
      -- ** zero
    , zero
    ) where


import Prelude hiding (rem)

import Css.Internal
import Css.Keywords           (None)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<angle\>@ data type.
newtype Angle = Angle Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<frequency\>@ data type.
newtype Frequency = Frequency Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<length\>@ data type.
newtype Length = Length Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<number\>@ data type.
class Buildable a => Number a


instance Number Double
instance Number Integer


-- | Represents the CSS @\<number\>|\<percentage\>|none@ data type.
class Buildable a => NumberPercentageNone a


instance NumberPercentageNone Double
instance NumberPercentageNone Integer
instance NumberPercentageNone None
instance NumberPercentageNone Percentage


-- | Represents the CSS @\<percentage\>@ data type.
newtype Percentage = Percentage Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<percentage\>|none@ data type.
class Buildable a => PercentageNone a


instance PercentageNone None
instance PercentageNone Percentage


-- | Represents the CSS @\<ratio\>@ data type.
newtype Ratio = Ratio Builder
    deriving (Buildable, Show)


-- | Represents the CSS params for the @\<ratio\>@ data type.
class Buildable a => RatioParams a where
    buildRatioParams :: a -> Builder


instance RatioParams Double where
    buildRatioParams = build


instance RatioParams Integer where
    buildRatioParams = build


instance (Buildable a, Num a, Buildable b, Num b) => RatioParams (a, b) where
    buildRatioParams (a, b) = build a <> " / " <> build b


-- | Represents the CSS @\<resolution\>@ data type.
newtype Resolution = Resolution Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<time\>@ data type.
newtype Time = Time Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<zero\>@ data type.
newtype Zero = Zero Builder
    deriving (Buildable, Show)


-- * ANGLE


-- | Generates a CSS @deg@ @\<angle\>@ value.
deg :: Number a => a -> Angle
deg = Angle . suffix "deg"
{-# INLINE deg #-}


-- | Generates a CSS @grad@ @\<angle\>@ value.
grad :: Number a => a -> Angle
grad = Angle . suffix "grad"
{-# INLINE grad #-}


-- | Generates a CSS @rad@ @\<angle\>@ value.
rad :: Number a => a -> Angle
rad = Angle . suffix "rad"
{-# INLINE rad #-}


-- | Generates a CSS @turn@ @\<angle\>@ value.
turn :: Number a => a -> Angle
turn = Angle . suffix "turn"
{-# INLINE turn #-}


-- * FREQUENCY


-- | Generates a CSS @Hz@ @\<frequency\>@ value.
hz :: Number a => a -> Frequency
hz = Frequency . suffix "Hz"
{-# INLINE hz #-}


-- | Generates a CSS @kHz@ @\<frequency\>@ value.
khz :: Number a => a -> Frequency
khz = Frequency . suffix "kHz"
{-# INLINE khz #-}


-- * LENGTH


-- | Generates a CSS @cap@ @\<length\>@ value.
cap :: Number a => a -> Length
cap = Length . suffix "cap"
{-# INLINE cap #-}


-- | Generates a CSS @ch@ @\<length\>@ value.
ch :: Number a => a -> Length
ch = Length . suffix "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ @\<length\>@ value.
cm :: Number a => a -> Length
cm = Length . suffix "cm"
{-# INLINE cm #-}


-- | Generates a CSS @dvb@ @\<length\>@ value.
dvb :: Number a => a -> Length
dvb = Length . suffix "dvb"
{-# INLINE dvb #-}


-- | Generates a CSS @dvh@ @\<length\>@ value.
dvh :: Number a => a -> Length
dvh = Length . suffix "dvh"
{-# INLINE dvh #-}


-- | Generates a CSS @dvi@ @\<length\>@ value.
dvi :: Number a => a -> Length
dvi = Length . suffix "dvi"
{-# INLINE dvi #-}


-- | Generates a CSS @dvmax@ @\<length\>@ value.
dvmax :: Number a => a -> Length
dvmax = Length . suffix "dvmax"
{-# INLINE dvmax #-}


-- | Generates a CSS @dvmin@ @\<length\>@ value.
dvmin :: Number a => a -> Length
dvmin = Length . suffix "dvmin"
{-# INLINE dvmin #-}


-- | Generates a CSS @dvw@ @\<length\>@ value.
dvw :: Number a => a -> Length
dvw = Length . suffix "dvw"
{-# INLINE dvw #-}


-- | Generates a CSS @em@ @\<length\>@ value.
em :: Number a => a -> Length
em = Length . suffix "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ @\<length\>@ value.
ex :: Number a => a -> Length
ex = Length . suffix "ex"
{-# INLINE ex #-}


-- | Generates a CSS @ic@ @\<length\>@ value.
ic :: Number a => a -> Length
ic = Length . suffix "ic"
{-# INLINE ic #-}


-- | Generates a CSS @in@ @\<length\>@ value.
in_ :: Number a => a -> Length
in_ = Length . suffix "in"
{-# INLINE in_ #-}


-- | Generates a CSS @lh@ @\<length\>@ value.
lh :: Number a => a -> Length
lh = Length . suffix "lh"
{-# INLINE lh #-}


-- | Generates a CSS @lvb@ @\<length\>@ value.
lvb :: Number a => a -> Length
lvb = Length . suffix "lvb"
{-# INLINE lvb #-}


-- | Generates a CSS @lvh@ @\<length\>@ value.
lvh :: Number a => a -> Length
lvh = Length . suffix "lvh"
{-# INLINE lvh #-}


-- | Generates a CSS @lvi@ @\<length\>@ value.
lvi :: Number a => a -> Length
lvi = Length . suffix "lvi"
{-# INLINE lvi #-}


-- | Generates a CSS @lvmax@ @\<length\>@ value.
lvmax :: Number a => a -> Length
lvmax = Length . suffix "lvmax"
{-# INLINE lvmax #-}


-- | Generates a CSS @lvmin@ @\<length\>@ value.
lvmin :: Number a => a -> Length
lvmin = Length . suffix "lvmin"
{-# INLINE lvmin #-}


-- | Generates a CSS @lvw@ @\<length\>@ value.
lvw :: Number a => a -> Length
lvw = Length . suffix "lvw"
{-# INLINE lvw #-}


-- | Generates a CSS @mm@ @\<length\>@ value.
mm :: Number a => a -> Length
mm = Length . suffix "mm"
{-# INLINE mm #-}


-- | Generates a CSS @pc@ @\<length\>@ value.
pc :: Number a => a -> Length
pc = Length . suffix "pc"
{-# INLINE pc #-}


-- | Generates a CSS @pt@ @\<length\>@ value.
pt :: Number a => a -> Length
pt = Length . suffix "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ @\<length\>@ value.
px :: Number a => a -> Length
px = Length . suffix "px"
{-# INLINE px #-}


-- | Generates a CSS @Q@ @\<length\>@ value.
q :: Number a => a -> Length
q = Length . suffix "Q"
{-# INLINE q #-}


-- | Generates a CSS @rcap@ @\<length\>@ value.
rcap :: Number a => a -> Length
rcap = Length . suffix "rcap"
{-# INLINE rcap #-}


-- | Generates a CSS @rch@ @\<length\>@ value.
rch :: Number a => a -> Length
rch = Length . suffix "rch"
{-# INLINE rch #-}


-- | Generates a CSS @rem@ @\<length\>@ value.
rem :: Number a => a -> Length
rem = Length . suffix "rem"
{-# INLINE rem #-}


-- | Generates a CSS @rex@ @\<length\>@ value.
rex :: Number a => a -> Length
rex = Length . suffix "rex"
{-# INLINE rex #-}


-- | Generates a CSS @ric@ @\<length\>@ value.
ric :: Number a => a -> Length
ric = Length . suffix "ric"
{-# INLINE ric #-}


-- | Generates a CSS @rlh@ @\<length\>@ value.
rlh :: Number a => a -> Length
rlh = Length . suffix "rlh"
{-# INLINE rlh #-}


-- | Generates a CSS @svb@ @\<length\>@ value.
svb :: Number a => a -> Length
svb = Length . suffix "svb"
{-# INLINE svb #-}


-- | Generates a CSS @svh@ @\<length\>@ value.
svh :: Number a => a -> Length
svh = Length . suffix "svh"
{-# INLINE svh #-}


-- | Generates a CSS @svi@ @\<length\>@ value.
svi :: Number a => a -> Length
svi = Length . suffix "svi"
{-# INLINE svi #-}


-- | Generates a CSS @svmax@ @\<length\>@ value.
svmax :: Number a => a -> Length
svmax = Length . suffix "svmax"
{-# INLINE svmax #-}


-- | Generates a CSS @svmin@ @\<length\>@ value.
svmin :: Number a => a -> Length
svmin = Length . suffix "svmin"
{-# INLINE svmin #-}


-- | Generates a CSS @svw@ @\<length\>@ value.
svw :: Number a => a -> Length
svw = Length . suffix "svw"
{-# INLINE svw #-}


-- | Generates a CSS @vb@ @\<length\>@ value.
vb :: Number a => a -> Length
vb = Length . suffix "vb"
{-# INLINE vb #-}


-- | Generates a CSS @vh@ @\<length\>@ value.
vh :: Number a => a -> Length
vh = Length . suffix "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vi@ @\<length\>@ value.
vi :: Number a => a -> Length
vi = Length . suffix "vi"
{-# INLINE vi #-}


-- | Generates a CSS @vmax@ @\<length\>@ value.
vmax :: Number a => a -> Length
vmax = Length . suffix "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ @\<length\>@ value.
vmin :: Number a => a -> Length
vmin = Length . suffix "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ @\<length\>@ value.
vw :: Number a => a -> Length
vw = Length . suffix "vw"
{-# INLINE vw #-}


-- * PERCENTAGE


-- | Generates a CSS @\<percentage\>@ value.
pct :: Number a => a -> Percentage
pct = Percentage . suffix "%"


-- * RATIO


-- | Generates a CSS @\<ratio\>@ value.
ratio :: RatioParams a => a -> Ratio
ratio = Ratio . buildRatioParams
{-# INLINE ratio #-}


-- * RESOLUTION


-- | Generates a CSS @dpcm@ @\<resolution\>@ value.
dpcm :: Number a => a -> Resolution
dpcm = Resolution . suffix "dpcm"
{-# INLINE dpcm #-}


-- | Generates a CSS @dpi@ @\<resolution\>@ value.
dpi :: Number a => a -> Resolution
dpi = Resolution . suffix "dpi"
{-# INLINE dpi #-}


-- | Generates a CSS @dppx@ @\<resolution\>@ value.
dppx :: Number a => a -> Resolution
dppx = Resolution . suffix "dppx"
{-# INLINE dppx #-}


-- * TIME


-- | Generates a CSS @ms@ @\<time\>@ value.
ms :: Number a => a -> Time
ms = Time . suffix "ms"
{-# INLINE ms #-}


-- | Generates a CSS @s@ @\<time\>@ value.
s :: Number a => a -> Time
s = Time . suffix "s"
{-# INLINE s #-}


-- * ZERO


-- | Generates the CSS @\<zero\>@ value.
zero :: Zero
zero = Zero "zero"
{-# INLINE zero #-}


-- HELPER FUNCTIONS


suffix :: Number a => Builder -> a -> Builder
suffix suffix' value = build value <> suffix'
