{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
deg = Angle . fromNumber "deg"
{-# INLINE deg #-}


-- | Generates a CSS @grad@ @\<angle\>@ value.
grad :: Number a => a -> Angle
grad = Angle . fromNumber "grad"
{-# INLINE grad #-}


-- | Generates a CSS @rad@ @\<angle\>@ value.
rad :: Number a => a -> Angle
rad = Angle . fromNumber "rad"
{-# INLINE rad #-}


-- | Generates a CSS @turn@ @\<angle\>@ value.
turn :: Number a => a -> Angle
turn = Angle . fromNumber "turn"
{-# INLINE turn #-}


-- * FREQUENCY


-- | Generates a CSS @Hz@ @\<frequency\>@ value.
hz :: Number a => a -> Frequency
hz = Frequency . fromNumber "Hz"
{-# INLINE hz #-}


-- | Generates a CSS @kHz@ @\<frequency\>@ value.
khz :: Number a => a -> Frequency
khz = Frequency . fromNumber "kHz"
{-# INLINE khz #-}


-- * LENGTH


-- | Generates a CSS @cap@ @\<length\>@ value.
cap :: Number a => a -> Length
cap = Length . fromNumber "cap"
{-# INLINE cap #-}


-- | Generates a CSS @ch@ @\<length\>@ value.
ch :: Number a => a -> Length
ch = Length . fromNumber "ch"
{-# INLINE ch #-}


-- | Generates a CSS @cm@ @\<length\>@ value.
cm :: Number a => a -> Length
cm = Length . fromNumber "cm"
{-# INLINE cm #-}


-- | Generates a CSS @dvb@ @\<length\>@ value.
dvb :: Number a => a -> Length
dvb = Length . fromNumber "dvb"
{-# INLINE dvb #-}


-- | Generates a CSS @dvh@ @\<length\>@ value.
dvh :: Number a => a -> Length
dvh = Length . fromNumber "dvh"
{-# INLINE dvh #-}


-- | Generates a CSS @dvi@ @\<length\>@ value.
dvi :: Number a => a -> Length
dvi = Length . fromNumber "dvi"
{-# INLINE dvi #-}


-- | Generates a CSS @dvmax@ @\<length\>@ value.
dvmax :: Number a => a -> Length
dvmax = Length . fromNumber "dvmax"
{-# INLINE dvmax #-}


-- | Generates a CSS @dvmin@ @\<length\>@ value.
dvmin :: Number a => a -> Length
dvmin = Length . fromNumber "dvmin"
{-# INLINE dvmin #-}


-- | Generates a CSS @dvw@ @\<length\>@ value.
dvw :: Number a => a -> Length
dvw = Length . fromNumber "dvw"
{-# INLINE dvw #-}


-- | Generates a CSS @em@ @\<length\>@ value.
em :: Number a => a -> Length
em = Length . fromNumber "em"
{-# INLINE em #-}


-- | Generates a CSS @ex@ @\<length\>@ value.
ex :: Number a => a -> Length
ex = Length . fromNumber "ex"
{-# INLINE ex #-}


-- | Generates a CSS @ic@ @\<length\>@ value.
ic :: Number a => a -> Length
ic = Length . fromNumber "ic"
{-# INLINE ic #-}


-- | Generates a CSS @in@ @\<length\>@ value.
in_ :: Number a => a -> Length
in_ = Length . fromNumber "in"
{-# INLINE in_ #-}


-- | Generates a CSS @lh@ @\<length\>@ value.
lh :: Number a => a -> Length
lh = Length . fromNumber "lh"
{-# INLINE lh #-}


-- | Generates a CSS @lvb@ @\<length\>@ value.
lvb :: Number a => a -> Length
lvb = Length . fromNumber "lvb"
{-# INLINE lvb #-}


-- | Generates a CSS @lvh@ @\<length\>@ value.
lvh :: Number a => a -> Length
lvh = Length . fromNumber "lvh"
{-# INLINE lvh #-}


-- | Generates a CSS @lvi@ @\<length\>@ value.
lvi :: Number a => a -> Length
lvi = Length . fromNumber "lvi"
{-# INLINE lvi #-}


-- | Generates a CSS @lvmax@ @\<length\>@ value.
lvmax :: Number a => a -> Length
lvmax = Length . fromNumber "lvmax"
{-# INLINE lvmax #-}


-- | Generates a CSS @lvmin@ @\<length\>@ value.
lvmin :: Number a => a -> Length
lvmin = Length . fromNumber "lvmin"
{-# INLINE lvmin #-}


-- | Generates a CSS @lvw@ @\<length\>@ value.
lvw :: Number a => a -> Length
lvw = Length . fromNumber "lvw"
{-# INLINE lvw #-}


-- | Generates a CSS @mm@ @\<length\>@ value.
mm :: Number a => a -> Length
mm = Length . fromNumber "mm"
{-# INLINE mm #-}


-- | Generates a CSS @pc@ @\<length\>@ value.
pc :: Number a => a -> Length
pc = Length . fromNumber "pc"
{-# INLINE pc #-}


-- | Generates a CSS @pt@ @\<length\>@ value.
pt :: Number a => a -> Length
pt = Length . fromNumber "pt"
{-# INLINE pt #-}


-- | Generates a CSS @px@ @\<length\>@ value.
px :: Number a => a -> Length
px = Length . fromNumber "px"
{-# INLINE px #-}


-- | Generates a CSS @Q@ @\<length\>@ value.
q :: Number a => a -> Length
q = Length . fromNumber "Q"
{-# INLINE q #-}


-- | Generates a CSS @rcap@ @\<length\>@ value.
rcap :: Number a => a -> Length
rcap = Length . fromNumber "rcap"
{-# INLINE rcap #-}


-- | Generates a CSS @rch@ @\<length\>@ value.
rch :: Number a => a -> Length
rch = Length . fromNumber "rch"
{-# INLINE rch #-}


-- | Generates a CSS @rem@ @\<length\>@ value.
rem :: Number a => a -> Length
rem = Length . fromNumber "rem"
{-# INLINE rem #-}


-- | Generates a CSS @rex@ @\<length\>@ value.
rex :: Number a => a -> Length
rex = Length . fromNumber "rex"
{-# INLINE rex #-}


-- | Generates a CSS @ric@ @\<length\>@ value.
ric :: Number a => a -> Length
ric = Length . fromNumber "ric"
{-# INLINE ric #-}


-- | Generates a CSS @rlh@ @\<length\>@ value.
rlh :: Number a => a -> Length
rlh = Length . fromNumber "rlh"
{-# INLINE rlh #-}


-- | Generates a CSS @svb@ @\<length\>@ value.
svb :: Number a => a -> Length
svb = Length . fromNumber "svb"
{-# INLINE svb #-}


-- | Generates a CSS @svh@ @\<length\>@ value.
svh :: Number a => a -> Length
svh = Length . fromNumber "svh"
{-# INLINE svh #-}


-- | Generates a CSS @svi@ @\<length\>@ value.
svi :: Number a => a -> Length
svi = Length . fromNumber "svi"
{-# INLINE svi #-}


-- | Generates a CSS @svmax@ @\<length\>@ value.
svmax :: Number a => a -> Length
svmax = Length . fromNumber "svmax"
{-# INLINE svmax #-}


-- | Generates a CSS @svmin@ @\<length\>@ value.
svmin :: Number a => a -> Length
svmin = Length . fromNumber "svmin"
{-# INLINE svmin #-}


-- | Generates a CSS @svw@ @\<length\>@ value.
svw :: Number a => a -> Length
svw = Length . fromNumber "svw"
{-# INLINE svw #-}


-- | Generates a CSS @vb@ @\<length\>@ value.
vb :: Number a => a -> Length
vb = Length . fromNumber "vb"
{-# INLINE vb #-}


-- | Generates a CSS @vh@ @\<length\>@ value.
vh :: Number a => a -> Length
vh = Length . fromNumber "vh"
{-# INLINE vh #-}


-- | Generates a CSS @vi@ @\<length\>@ value.
vi :: Number a => a -> Length
vi = Length . fromNumber "vi"
{-# INLINE vi #-}


-- | Generates a CSS @vmax@ @\<length\>@ value.
vmax :: Number a => a -> Length
vmax = Length . fromNumber "vmax"
{-# INLINE vmax #-}


-- | Generates a CSS @vmin@ @\<length\>@ value.
vmin :: Number a => a -> Length
vmin = Length . fromNumber "vmin"
{-# INLINE vmin #-}


-- | Generates a CSS @vw@ @\<length\>@ value.
vw :: Number a => a -> Length
vw = Length . fromNumber "vw"
{-# INLINE vw #-}


-- * PERCENTAGE


-- | Generates a CSS @\<percentage\>@ value.
pct :: Number a => a -> Percentage
pct = Percentage . fromNumber "%"


-- * RATIO


-- | Generates a CSS @\<ratio\>@ value.
ratio :: RatioParams a => a -> Ratio
ratio = Ratio . buildRatioParams
{-# INLINE ratio #-}


-- * RESOLUTION


-- | Generates a CSS @dpcm@ @\<resolution\>@ value.
dpcm :: Number a => a -> Resolution
dpcm = Resolution . fromNumber "dpcm"
{-# INLINE dpcm #-}


-- | Generates a CSS @dpi@ @\<resolution\>@ value.
dpi :: Number a => a -> Resolution
dpi = Resolution . fromNumber "dpi"
{-# INLINE dpi #-}


-- | Generates a CSS @dppx@ @\<resolution\>@ value.
dppx :: Number a => a -> Resolution
dppx = Resolution . fromNumber "dppx"
{-# INLINE dppx #-}


-- * TIME


-- | Generates a CSS @ms@ @\<time\>@ value.
ms :: Number a => a -> Time
ms = Time . fromNumber "ms"
{-# INLINE ms #-}


-- | Generates a CSS @s@ @\<time\>@ value.
s :: Number a => a -> Time
s = Time . fromNumber "s"
{-# INLINE s #-}


-- * ZERO


-- | Generates a CSS @\<zero\>@ value.
zero :: Zero
zero = Zero "zero"
{-# INLINE zero #-}


-- HELPER FUNCTIONS


fromNumber :: Number a => Builder -> a -> Builder
fromNumber suffix value = build value <> suffix
