{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Functions
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Functions" module provides a set of functions that correspond to CSS functions.
module Css.Functions
    ( -- * Functions
      -- ** attr
      attr
      -- ** calc
    , calc
      -- ** conic-gradient
    , conicGradient
      -- ** counter
    , counter
      -- ** cubic-bezier
    , cubicBezier
      -- ** hsl
    , hsl
      -- ** hsla
    , hsla
      -- ** linear-gradient
    , linearGradient
      -- ** max
    , max
      -- ** min
    , min
      -- ** radial-gradient
    , radialGradient
      -- ** repeating-conic-gradient
    , repeatingConicGradient
      -- ** repeating-linear-gradient
    , repeatingLinearGradient
      -- ** repeating-radial-gradient
    , repeatingRadialGradient
      -- ** rgb
    --, rgb
      -- ** rgba
    , rgba
      -- ** var
    , var
    ) where


import Prelude hiding (head, length, max, min, tail)

import Data.Foldable                    (fold)
import Data.List                        (intersperse)
import Data.Text.Lazy                   (Text, head, length, tail)
import Data.Text.Lazy.Builder           (Builder, fromLazyText, fromString, singleton, toLazyText)
import Data.Text.Lazy.Builder.Int       (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Html                             (Buildable(..))

import qualified Data.Text.Lazy.Read as Read


    {-
class IntegerOrPercentage a where
    integerOrPercentage :: a -> Builder


instance IntegerOrPercentage Integer where
    integerOrPercentage = decimal . fromInteger


instance IntegerOrPercentage Percentage where
    integerOrPercentage = unPercentage


newtype Color = Color { unColor :: Builder }
  deriving Show


rgb :: (IntegerOrPercentage a, IntegerOrPercentage b, IntegerOrPercentage c) => a -> b -> c -> Color
rgb red green blue
    =  Color
    $  "rgb("
    <> integerOrPercentage red
    <> singleton ','
    <> integerOrPercentage green
    <> singleton ','
    <> integerOrPercentage blue
    <> singleton ')'


--class Auto a where auto :: a
newtype Auto = Auto { unAuto :: Builder }


auto :: Auto
auto = Auto "auto"


instance AccentColor Auto where
    unAccentColor = unAuto


--instance AccentColor IntegerOrPercentage where
    --unAccentColor = integerOrPercentage


class AccentColor a where
    unAccentColor :: a -> Builder


--data Css


accentColor :: AccentColor a => a -> Builder
accentColor = unAccentColor
-}

instance Num Builder where
    (+) a b     = a <> singleton '+' <> b
    (-) a b     = a <> singleton '-' <> b
    (*) a b     = a <> singleton '*' <> b
    negate      = negate' . toLazyText
    abs         = abs'    . toLazyText
    signum      = signum' . toLazyText
    fromInteger = decimal . fromInteger


abs' :: Text -> Builder
abs' num
    | length num == 0   = ""
    | head   num == '-' = fromLazyText $ tail num
    | otherwise         = fromLazyText num


negate' :: Text -> Builder
negate' num
    | length num == 0   = ""
    | head   num == '-' = fromLazyText $ tail num
    | otherwise         = singleton '-' <> fromLazyText num


signum' :: Text -> Builder
signum' num
    | length num == 0                    = singleton '0'
    | length num == 1 && head num == '0' = singleton '0'
    | head   num == '-'                  = "-1"
    | otherwise                          = singleton '1'


instance Fractional Builder where
    (/) a b      = a <> singleton '/' <> b
    recip        = (<>) "1/"
    fromRational = realFloat . fromRational


attr :: Builder -> Builder
attr attributeName = "attr(" <> attributeName <> singleton ')'


calc :: Builder -> Builder
calc expression = "calc(" <> expression <> singleton ')'


conicGradient :: [Builder] -> [Builder] -> [[Builder]] -> Builder
conicGradient = conic "conic-gradient("


counter :: Builder -> [Builder] -> Builder
counter counterName counterStyle = "counter(" <> counterName <> optionalSpace <> listSpaces counterStyle <> singleton ')'
  where
    optionalSpace = if null counterStyle then mempty else singleton ' '


cubicBezier :: Builder -> Builder -> Builder -> Builder -> Builder
cubicBezier x1 y1 x2 y2 =  "cubic-bezier(" <> x1 <> singleton ',' <> y1 <> singleton ',' <> x2 <> singleton ',' <> y2 <> singleton ')'


hsl :: Builder -> Builder -> Builder -> Builder
hsl hue saturation lightness = "hsl(" <> hue <> singleton ',' <> saturation <> singleton ',' <> lightness <> singleton ')'


hsla :: Builder -> Builder -> Builder -> Builder -> Builder
hsla hue saturation lightness alpha
    =  "hsla("
    <> hue
    <> singleton ','
    <> saturation
    <> singleton ','
    <> lightness
    <> singleton ','
    <> alpha
    <> singleton ')'


linearGradient :: [Builder] -> [Builder] -> Builder
linearGradient = linear "linear-gradient("


max :: [Builder] -> Builder
max values = "max(" <> listCommas values <> singleton ')'


min :: [Builder] -> Builder
min values = "min(" <> listCommas values <> singleton ')'


radialGradient :: [Builder] -> [Builder] -> [Builder] -> [[Builder]] -> Builder
radialGradient = radial "radial-gradient("


repeatingConicGradient :: [Builder] -> [Builder] -> [[Builder]] -> Builder
repeatingConicGradient = conic "repeating-conic-gradient("


repeatingLinearGradient :: [Builder] -> [Builder] -> Builder
repeatingLinearGradient = linear "repeating-linear-gradient("


repeatingRadialGradient :: [Builder] -> [Builder] -> [Builder] -> [[Builder]] -> Builder
repeatingRadialGradient = radial "repeating-radial-gradient("


rgb :: Builder -> Builder -> Builder -> Builder
rgb red green blue = "rgb(" <> red <> singleton ',' <> green <> singleton ',' <> blue <> singleton ')'


rgba :: Builder -> Builder -> Builder -> Builder -> Builder
rgba red green blue alpha =  "rgba(" <> red <> singleton ',' <> green <> singleton ',' <> blue <> singleton ',' <> alpha <> singleton ')'


var :: Builder -> [Builder] -> Builder
var name value = "var(" <> name <> optionalComma <> listSpaces value <> singleton ')'
  where
    optionalComma = if null value then mempty else singleton ','


-- HELPER FUNCTIONS


conic :: Builder -> [Builder] -> [Builder] -> [[Builder]] -> Builder
conic name angle position colors
    =  name
    <> listSpaces angle
    <> optionalSpace
    <> listSpaces position
    <> optionalComma
    <> (listCommas . map listSpaces) colors
    <> singleton ')'
  where
    optionalComma = if null angle && null position || null colors then mempty else singleton ','
    optionalSpace = if not (null angle) && not (null position) then singleton ' ' else mempty


linear :: Builder -> [Builder] -> [Builder] -> Builder
linear name direction colorStops = name <> listSpaces direction <> optionalComma <> listCommas colorStops <> singleton ')'
  where
    optionalComma = if null direction || null colorStops then mempty else singleton ','


listCommas :: [Builder] -> Builder
listCommas = fold . intersperse (singleton ',')


listSpaces :: [Builder] -> Builder
listSpaces = fold . intersperse (singleton ' ')


radial :: Builder -> [Builder] -> [Builder] -> [Builder] -> [[Builder]] -> Builder
radial name shape size position colorStops
    =  name
    <> listSpaces shape
    <> optionalSpace1
    <> listSpaces size
    <> optionalSpace2
    <> listSpaces position
    <> optionalComma
    <> (listCommas . map listSpaces) colorStops
    <> singleton ')'
  where
    optionalComma  = if null shape && null size && null position || null colorStops then mempty else singleton ','
    optionalSpace1 = if not (null shape) && not (null size) then singleton ' ' else mempty
    optionalSpace2 = if not (null position) && (not (null shape) || not (null size)) then singleton ' ' else mempty
