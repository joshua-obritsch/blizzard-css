{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module    : Css.DataTypes.Color
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Color" module provides a set of types and functions for generating color data types in CSS.
module Css.DataTypes.Color
    ( -- * Data Types
      -- ** \<alpha-value\>
      AlphaValue
      -- ** \<color\>
    , Color
      -- ** \<color()\>
    , ColorFunction
      -- ** \<colorspace-params\>
    , ColorspaceParams
      -- ** \<hsl()\>
    , HslFunction
      -- ** \<hue\>|none
    , HueNone
      -- ** \<hwb()\>
    , HwbFunction
      -- ** \<lab()\>
    , LabFunction
      -- ** \<lch()\>
    , LchFunction
      -- ** \<oklab()\>
    , OklabFunction
      -- ** \<oklch()\>
    , OklchFunction
      -- ** \<rgb()\>
    , RgbFunction

      -- * \<absolute-color-base\>
      -- ** transparent
    , transparent

      -- * \<absolute-color-function\>
      -- ** \<color()\>
    , color
      -- ** \<hsl()\>
    , hsl
      -- ** \<hwb()\>
    , hwb
      -- ** \<lab()\>
    , lab
      -- ** \<lch()\>
    , lch
      -- ** \<oklab()\>
    , oklab
      -- ** \<oklch()\>
    , oklch
      -- ** \<rgb()\>
    , rgb

      -- * \<color\>
      -- ** currentcolor
    , currentcolor

      -- * \<hex-color\>
    , hex

      -- * \<named-color\>
      -- ** aliceblue
    , aliceblue
      -- ** antiquewhite
    , antiquewhite
      -- ** aqua
    , aqua
      -- ** aquamarine
    , aquamarine
      -- ** azure
    , azure
      -- ** beige
    , beige
      -- ** bisque
    , bisque
      -- ** black
    , black
      -- ** blanchedalmond
    , blanchedalmond
      -- ** blue
    , blue
      -- ** blueviolet
    , blueviolet
      -- ** brown
    , brown
      -- ** burlywood
    , burlywood
      -- ** cadetblue
    , cadetblue
      -- ** chartreuse
    , chartreuse
      -- ** chocolate
    , chocolate
      -- ** coral
    , coral
      -- ** cornflowerblue
    , cornflowerblue
      -- ** cornsilk
    , cornsilk
      -- ** crimson
    , crimson
      -- ** cyan
    , cyan
      -- ** darkblue
    , darkblue
      -- ** darkcyan
    , darkcyan
      -- ** darkgoldenrod
    , darkgoldenrod
      -- ** darkgray
    , darkgray
      -- ** darkgreen
    , darkgreen
      -- ** darkgrey
    , darkgrey
      -- ** darkkhaki
    , darkkhaki
      -- ** darkmagenta
    , darkmagenta
      -- ** darkolivegreen
    , darkolivegreen
      -- ** darkorange
    , darkorange
      -- ** darkorchid
    , darkorchid
      -- ** darkred
    , darkred
      -- ** darksalmon
    , darksalmon
      -- ** darkseagreen
    , darkseagreen
      -- ** darkslateblue
    , darkslateblue
      -- ** darkslategray
    , darkslategray
      -- ** darkslategrey
    , darkslategrey
      -- ** darkturquoise
    , darkturquoise
      -- ** darkviolet
    , darkviolet
      -- ** deeppink
    , deeppink
      -- ** deepskyblue
    , deepskyblue
      -- ** dimgray
    , dimgray
      -- ** dimgrey
    , dimgrey
      -- ** dodgerblue
    , dodgerblue
      -- ** firebrick
    , firebrick
      -- ** floralwhite
    , floralwhite
      -- ** forestgreen
    , forestgreen
      -- ** fuchsia
    , fuchsia
      -- ** gainsboro
    , gainsboro
      -- ** ghostwhite
    , ghostwhite
      -- ** gold
    , gold
      -- ** goldenrod
    , goldenrod
      -- ** gray
    , gray
      -- ** green
    , green
      -- ** greenyellow
    , greenyellow
      -- ** grey
    , grey
      -- ** honeydew
    , honeydew
      -- ** hotpink
    , hotpink
      -- ** indianred
    , indianred
      -- ** indigo
    , indigo
      -- ** ivory
    , ivory
      -- ** khaki
    , khaki
      -- ** lavender
    , lavender
      -- ** lavenderblush
    , lavenderblush
      -- ** lawngreen
    , lawngreen
      -- ** lemonchiffon
    , lemonchiffon
      -- ** lightblue
    , lightblue
      -- ** lightcoral
    , lightcoral
      -- ** lightcyan
    , lightcyan
      -- ** lightgoldenrodyellow
    , lightgoldenrodyellow
      -- ** lightgray
    , lightgray
      -- ** lightgreen
    , lightgreen
      -- ** lightgrey
    , lightgrey
      -- ** lightpink
    , lightpink
      -- ** lightsalmon
    , lightsalmon
      -- ** lightseagreen
    , lightseagreen
      -- ** lightskyblue
    , lightskyblue
      -- ** lightslategray
    , lightslategray
      -- ** lightslategrey
    , lightslategrey
      -- ** lightsteelblue
    , lightsteelblue
      -- ** lightyellow
    , lightyellow
      -- ** lime
    , lime
      -- ** limegreen
    , limegreen
      -- ** linen
    , linen
      -- ** magenta
    , magenta
      -- ** maroon
    , maroon
      -- ** mediumaquamarine
    , mediumaquamarine
      -- ** mediumblue
    , mediumblue
      -- ** mediumorchid
    , mediumorchid
      -- ** mediumpurple
    , mediumpurple
      -- ** mediumseagreen
    , mediumseagreen
      -- ** mediumslateblue
    , mediumslateblue
      -- ** mediumspringgreen
    , mediumspringgreen
      -- ** mediumturquoise
    , mediumturquoise
      -- ** mediumvioletred
    , mediumvioletred
      -- ** midnightblue
    , midnightblue
      -- ** mintcream
    , mintcream
      -- ** mistyrose
    , mistyrose
      -- ** moccasin
    , moccasin
      -- ** navajowhite
    , navajowhite
      -- ** navy
    , navy
      -- ** oldlace
    , oldlace
      -- ** olive
    , olive
      -- ** olivedrab
    , olivedrab
      -- ** orange
    , orange
      -- ** orangered
    , orangered
      -- ** orchid
    , orchid
      -- ** palegoldenrod
    , palegoldenrod
      -- ** palegreen
    , palegreen
      -- ** paleturquoise
    , paleturquoise
      -- ** palevioletred
    , palevioletred
      -- ** papayawhip
    , papayawhip
      -- ** peachpuff
    , peachpuff
      -- ** peru
    , peru
      -- ** pink
    , pink
      -- ** plum
    , plum
      -- ** powderblue
    , powderblue
      -- ** purple
    , purple
      -- ** rebeccapurple
    , rebeccapurple
      -- ** red
    , red
      -- ** rosybrown
    , rosybrown
      -- ** royalblue
    , royalblue
      -- ** saddlebrown
    , saddlebrown
      -- ** salmon
    , salmon
      -- ** sandybrown
    , sandybrown
      -- ** seagreen
    , seagreen
      -- ** seashell
    , seashell
      -- ** sienna
    , sienna
      -- ** silver
    , silver
      -- ** skyblue
    , skyblue
      -- ** slateblue
    , slateblue
      -- ** slategray
    , slategray
      -- ** slategrey
    , slategrey
      -- ** snow
    , snow
      -- ** springgreen
    , springgreen
      -- ** steelblue
    , steelblue
      -- ** tan
    , tan
      -- ** teal
    , teal
      -- ** thistle
    , thistle
      -- ** tomato
    , tomato
      -- ** turquoise
    , turquoise
      -- ** violet
    , violet
      -- ** wheat
    , wheat
      -- ** white
    , white
      -- ** whitesmoke
    , whitesmoke
      -- ** yellow
    , yellow
      -- ** yellowgreen
    , yellowgreen

      -- * \<predefined-rgb\>
      -- ** a98-rgb
    , a98Rgb
      -- ** display-p3
    , displayP3
      -- ** prophoto-rgb
    , prophotoRgb
      -- ** rec2020
    , rec2020
      -- ** srgb
    , srgb
      -- ** srgb-linear
    , srgbLinear

      -- * \<system-color\>
      -- ** AccentColor
    , accentColor
      -- ** AccentColorText
    , accentColorText
      -- ** ActiveText
    , activeText
      -- ** ButtonBorder
    , buttonBorder
      -- ** ButtonFace
    , buttonFace
      -- ** ButtonText
    , buttonText
      -- ** Canvas
    , canvas
      -- ** CanvasText
    , canvasText
      -- ** Field
    , field
      -- ** FieldText
    , fieldText
      -- ** GrayText
    , grayText
      -- ** Highlight
    , highlight
      -- ** HighlightText
    , highlightText
      -- ** LinkText
    , linkText
      -- ** Mark
    , mark
      -- ** MarkText
    , markText
      -- ** SelectedItem
    , selectedItem
      -- ** SelectedItemText
    , selectedItemText
      -- ** VisitedText
    , visitedText

      -- * \<xyz-space\>
      -- ** xyz
    , xyz
      -- ** xyz-d50
    , xyzD50
      -- ** xyz-d65
    , xyzD65
    ) where


import Prelude hiding (tan)

import Css.DataTypes.Numeric  (Angle, Number, NumberPercentageNone, PercentageNone)
import Css.Internal           (showHex)
import Css.Keywords           (None)
import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Word              (Word32)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<alpha-value\>@ data type.
type AlphaValue = NumberPercentageNone


-- | Represents the CSS @\<color\>@ data type.
newtype Color = Color Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<color()\>@ data type.
class ColorFunction a where
    buildColorFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => ColorFunction (ColorspaceParams, a, b, c) where
    buildColorFunction (a, b, c, d) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c <> singleton ' ' <> build d


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d)
    => ColorFunction (ColorspaceParams, a, b, c, d) where
    buildColorFunction (a, b, c, d, e)
        = build a <> singleton ' ' <> build b <> singleton ' ' <> build c <> singleton ' ' <> build d <> " / " <> build e


-- | Represents the CSS @\<colorspace-params\>@ data type.
newtype ColorspaceParams = ColorspaceParams Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<hsl()\>@ data type.
class HslFunction a where
    buildHslFunction :: a -> Builder


instance (HueNone a, PercentageNone b, PercentageNone c) => HslFunction (a, b, c) where
    buildHslFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (HueNone a, PercentageNone b, PercentageNone c, AlphaValue d) => HslFunction (a, b, c, d) where
    buildHslFunction (a, b, c, d) = buildHslFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<hue\>|none@ data type.
class Buildable a => HueNone a


instance HueNone Angle
instance HueNone Double
instance HueNone Integer
instance HueNone None


-- | Represents the CSS @\<hwb()\>@ data type.
class HwbFunction a where
    buildHwbFunction :: a -> Builder


instance (HueNone a, PercentageNone b, PercentageNone c) => HwbFunction (a, b, c) where
    buildHwbFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (HueNone a, PercentageNone b, PercentageNone c, AlphaValue d) => HwbFunction (a, b, c, d) where
    buildHwbFunction (a, b, c, d) = buildHwbFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<lab()\>@ data type.
class LabFunction a where
    buildLabFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => LabFunction (a, b, c) where
    buildLabFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => LabFunction (a, b, c, d) where
    buildLabFunction (a, b, c, d) = buildLabFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<lch()\>@ data type.
class LchFunction a where
    buildLchFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, HueNone c) => LchFunction (a, b, c) where
    buildLchFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (NumberPercentageNone a, NumberPercentageNone b, HueNone c, AlphaValue d) => LchFunction (a, b, c, d) where
    buildLchFunction (a, b, c, d) = buildLchFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<oklab()\>@ data type.
class OklabFunction a where
    buildOklabFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => OklabFunction (a, b, c) where
    buildOklabFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => OklabFunction (a, b, c, d) where
    buildOklabFunction (a, b, c, d) = buildOklabFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<oklch()\>@ data type.
class OklchFunction a where
    buildOklchFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, HueNone c) => OklchFunction (a, b, c) where
    buildOklchFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (NumberPercentageNone a, NumberPercentageNone b, HueNone c, AlphaValue d) => OklchFunction (a, b, c, d) where
    buildOklchFunction (a, b, c, d) = buildOklchFunction (a, b, c) <> " / " <> build d


-- | Represents the CSS @\<rgb()\>@ data type.
class RgbFunction a where
    buildRgbFunction :: a -> Builder


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => RgbFunction (a, b, c) where
    buildRgbFunction (a, b, c) = build a <> singleton ' ' <> build b <> singleton ' ' <> build c


instance (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => RgbFunction (a, b, c, d) where
    buildRgbFunction (a, b, c, d) = buildRgbFunction (a, b, c) <> " / " <> build d


-- * ABSOLUTE-COLOR-BASE


-- | Generates the CSS @transparent@ @\<absolute-color-base\>@ value.
transparent :: Color
transparent = Color "transparent"
{-# INLINE transparent #-}


-- * ABSOLUTE-COLOR-FUNCTION


-- | Generates a CSS @\<color()\>@ value.
color :: ColorFunction a => a -> Color
color value = Color $ "color(" <> buildColorFunction value <> singleton ')'


-- | Generates a CSS @\<hsl()\>@ value without @\<alpha-value\>@.
hsl :: HslFunction a => a -> Color
hsl value = Color $ "hsl(" <> buildHslFunction value <> singleton ')'


-- | Generates a CSS @\<hwb()\>@ value without @\<alpha-value\>@.
hwb :: HwbFunction a => a -> Color
hwb value = Color $ "hwb(" <> buildHwbFunction value <> singleton ')'


-- | Generates a CSS @\<lab()\>@ value without @\<alpha-value\>@.
lab :: LabFunction a => a -> Color
lab value = Color $ "lab(" <> buildLabFunction value <> singleton ')'


-- | Generates a CSS @\<lch()\>@ value without @\<alpha-value\>@.
lch :: LchFunction a => a -> Color
lch value = Color $ "lch(" <> buildLchFunction value <> singleton ')'


-- | Generates a CSS @\<oklab()\>@ value.
oklab :: OklabFunction a => a -> Color
oklab value = Color $ "oklab(" <> buildOklabFunction value <> singleton ')'


-- | Generates a CSS @\<oklch()\>@ value.
oklch :: OklchFunction a => a -> Color
oklch value = Color $ "oklch(" <> buildOklchFunction value <> singleton ')'


-- | Generates a CSS @\<rgb()\>@ value.
rgb :: RgbFunction a => a -> Color
rgb value = Color $ "rgb(" <> buildRgbFunction value <> singleton ')'


-- * COLOR


-- | Generates the CSS @currentcolor@ @\<color\>@ value.
currentcolor :: Color
currentcolor = Color "currentcolor"
{-# INLINE currentcolor #-}


-- * HEX-COLOR


-- | Generates a CSS @\<hex-color\>@ value.
hex :: Word32 -> Color
hex = Color . showHex
{-# INLINE hex #-}


-- * NAMED-COLOR


-- | Generates the CSS @aliceblue@ @\<named-color\>@ value.
aliceblue :: Color
aliceblue = Color "aliceblue"
{-# INLINE aliceblue #-}


-- | Generates the CSS @antiquewhite@ @\<named-color\>@ value.
antiquewhite :: Color
antiquewhite = Color "antiquewhite"
{-# INLINE antiquewhite #-}


-- | Generates the CSS @aqua@ @\<named-color\>@ value.
aqua :: Color
aqua = Color "aqua"
{-# INLINE aqua #-}


-- | Generates the CSS @aquamarine@ @\<named-color\>@ value.
aquamarine :: Color
aquamarine = Color "aquamarine"
{-# INLINE aquamarine #-}


-- | Generates the CSS @azure@ @\<named-color\>@ value.
azure :: Color
azure = Color "azure"
{-# INLINE azure #-}


-- | Generates the CSS @beige@ @\<named-color\>@ value.
beige :: Color
beige = Color "beige"
{-# INLINE beige #-}


-- | Generates the CSS @bisque@ @\<named-color\>@ value.
bisque :: Color
bisque = Color "bisque"
{-# INLINE bisque #-}


-- | Generates the CSS @black@ @\<named-color\>@ value.
black :: Color
black = Color "black"
{-# INLINE black #-}


-- | Generates the CSS @blanchedalmond@ @\<named-color\>@ value.
blanchedalmond :: Color
blanchedalmond = Color "blanchedalmond"
{-# INLINE blanchedalmond #-}


-- | Generates the CSS @blue@ @\<named-color\>@ value.
blue :: Color
blue = Color "blue"
{-# INLINE blue #-}


-- | Generates the CSS @blueviolet@ @\<named-color\>@ value.
blueviolet :: Color
blueviolet = Color "blueviolet"
{-# INLINE blueviolet #-}


-- | Generates the CSS @brown@ @\<named-color\>@ value.
brown :: Color
brown = Color "brown"
{-# INLINE brown #-}


-- | Generates the CSS @burlywood@ @\<named-color\>@ value.
burlywood :: Color
burlywood = Color "burlywood"
{-# INLINE burlywood #-}


-- | Generates the CSS @cadetblue@ @\<named-color\>@ value.
cadetblue :: Color
cadetblue = Color "cadetblue"
{-# INLINE cadetblue #-}


-- | Generates the CSS @chartreuse@ @\<named-color\>@ value.
chartreuse :: Color
chartreuse = Color "chartreuse"
{-# INLINE chartreuse #-}


-- | Generates the CSS @chocolate@ @\<named-color\>@ value.
chocolate :: Color
chocolate = Color "chocolate"
{-# INLINE chocolate #-}


-- | Generates the CSS @coral@ @\<named-color\>@ value.
coral :: Color
coral = Color "coral"
{-# INLINE coral #-}


-- | Generates the CSS @cornflowerblue@ @\<named-color\>@ value.
cornflowerblue :: Color
cornflowerblue = Color "cornflowerblue"
{-# INLINE cornflowerblue #-}


-- | Generates the CSS @cornsilk@ @\<named-color\>@ value.
cornsilk :: Color
cornsilk = Color "cornsilk"
{-# INLINE cornsilk #-}


-- | Generates the CSS @crimson@ @\<named-color\>@ value.
crimson :: Color
crimson = Color "crimson"
{-# INLINE crimson #-}


-- | Generates the CSS @cyan@ @\<named-color\>@ value.
cyan :: Color
cyan = Color "cyan"
{-# INLINE cyan #-}


-- | Generates the CSS @darkblue@ @\<named-color\>@ value.
darkblue :: Color
darkblue = Color "darkblue"
{-# INLINE darkblue #-}


-- | Generates the CSS @darkcyan@ @\<named-color\>@ value.
darkcyan :: Color
darkcyan = Color "darkcyan"
{-# INLINE darkcyan #-}


-- | Generates the CSS @darkgoldenrod@ @\<named-color\>@ value.
darkgoldenrod :: Color
darkgoldenrod = Color "darkgoldenrod"
{-# INLINE darkgoldenrod #-}


-- | Generates the CSS @darkgray@ @\<named-color\>@ value.
darkgray :: Color
darkgray = Color "darkgray"
{-# INLINE darkgray #-}


-- | Generates the CSS @darkgreen@ @\<named-color\>@ value.
darkgreen :: Color
darkgreen = Color "darkgreen"
{-# INLINE darkgreen #-}


-- | Generates the CSS @darkgrey@ @\<named-color\>@ value.
darkgrey :: Color
darkgrey = Color "darkgrey"
{-# INLINE darkgrey #-}


-- | Generates the CSS @darkkhaki@ @\<named-color\>@ value.
darkkhaki :: Color
darkkhaki = Color "darkkhaki"
{-# INLINE darkkhaki #-}


-- | Generates the CSS @darkmagenta@ @\<named-color\>@ value.
darkmagenta :: Color
darkmagenta = Color "darkmagenta"
{-# INLINE darkmagenta #-}


-- | Generates the CSS @darkolivegreen@ @\<named-color\>@ value.
darkolivegreen :: Color
darkolivegreen = Color "darkolivegreen"
{-# INLINE darkolivegreen #-}


-- | Generates the CSS @darkorange@ @\<named-color\>@ value.
darkorange :: Color
darkorange = Color "darkorange"
{-# INLINE darkorange #-}


-- | Generates the CSS @darkorchid@ @\<named-color\>@ value.
darkorchid :: Color
darkorchid = Color "darkorchid"
{-# INLINE darkorchid #-}


-- | Generates the CSS @darkred@ @\<named-color\>@ value.
darkred :: Color
darkred = Color "darkred"
{-# INLINE darkred #-}


-- | Generates the CSS @darksalmon@ @\<named-color\>@ value.
darksalmon :: Color
darksalmon = Color "darksalmon"
{-# INLINE darksalmon #-}


-- | Generates the CSS @darkseagreen@ @\<named-color\>@ value.
darkseagreen :: Color
darkseagreen = Color "darkseagreen"
{-# INLINE darkseagreen #-}


-- | Generates the CSS @darkslateblue@ @\<named-color\>@ value.
darkslateblue :: Color
darkslateblue = Color "darkslateblue"
{-# INLINE darkslateblue #-}


-- | Generates the CSS @darkslategray@ @\<named-color\>@ value.
darkslategray :: Color
darkslategray = Color "darkslategray"
{-# INLINE darkslategray #-}


-- | Generates the CSS @darkslategrey@ @\<named-color\>@ value.
darkslategrey :: Color
darkslategrey = Color "darkslategrey"
{-# INLINE darkslategrey #-}


-- | Generates the CSS @darkturquoise@ @\<named-color\>@ value.
darkturquoise :: Color
darkturquoise = Color "darkturquoise"
{-# INLINE darkturquoise #-}


-- | Generates the CSS @darkviolet@ @\<named-color\>@ value.
darkviolet :: Color
darkviolet = Color "darkviolet"
{-# INLINE darkviolet #-}


-- | Generates the CSS @deeppink@ @\<named-color\>@ value.
deeppink :: Color
deeppink = Color "deeppink"
{-# INLINE deeppink #-}


-- | Generates the CSS @deepskyblue@ @\<named-color\>@ value.
deepskyblue :: Color
deepskyblue = Color "deepskyblue"
{-# INLINE deepskyblue #-}


-- | Generates the CSS @dimgray@ @\<named-color\>@ value.
dimgray :: Color
dimgray = Color "dimgray"
{-# INLINE dimgray #-}


-- | Generates the CSS @dimgrey@ @\<named-color\>@ value.
dimgrey :: Color
dimgrey = Color "dimgrey"
{-# INLINE dimgrey #-}


-- | Generates the CSS @dodgerblue@ @\<named-color\>@ value.
dodgerblue :: Color
dodgerblue = Color "dodgerblue"
{-# INLINE dodgerblue #-}


-- | Generates the CSS @firebrick@ @\<named-color\>@ value.
firebrick :: Color
firebrick = Color "firebrick"
{-# INLINE firebrick #-}


-- | Generates the CSS @floralwhite@ @\<named-color\>@ value.
floralwhite :: Color
floralwhite = Color "floralwhite"
{-# INLINE floralwhite #-}


-- | Generates the CSS @forestgreen@ @\<named-color\>@ value.
forestgreen :: Color
forestgreen = Color "forestgreen"
{-# INLINE forestgreen #-}


-- | Generates the CSS @fuchsia@ @\<named-color\>@ value.
fuchsia :: Color
fuchsia = Color "fuchsia"
{-# INLINE fuchsia #-}


-- | Generates the CSS @gainsboro@ @\<named-color\>@ value.
gainsboro :: Color
gainsboro = Color "gainsboro"
{-# INLINE gainsboro #-}


-- | Generates the CSS @ghostwhite@ @\<named-color\>@ value.
ghostwhite :: Color
ghostwhite = Color "ghostwhite"
{-# INLINE ghostwhite #-}


-- | Generates the CSS @gold@ @\<named-color\>@ value.
gold :: Color
gold = Color "gold"
{-# INLINE gold #-}


-- | Generates the CSS @goldenrod@ @\<named-color\>@ value.
goldenrod :: Color
goldenrod = Color "goldenrod"
{-# INLINE goldenrod #-}


-- | Generates the CSS @gray@ @\<named-color\>@ value.
gray :: Color
gray = Color "gray"
{-# INLINE gray #-}


-- | Generates the CSS @green@ @\<named-color\>@ value.
green :: Color
green = Color "green"
{-# INLINE green #-}


-- | Generates the CSS @greenyellow@ @\<named-color\>@ value.
greenyellow :: Color
greenyellow = Color "greenyellow"
{-# INLINE greenyellow #-}


-- | Generates the CSS @grey@ @\<named-color\>@ value.
grey :: Color
grey = Color "grey"
{-# INLINE grey #-}


-- | Generates the CSS @honeydew@ @\<named-color\>@ value.
honeydew :: Color
honeydew = Color "honeydew"
{-# INLINE honeydew #-}


-- | Generates the CSS @hotpink@ @\<named-color\>@ value.
hotpink :: Color
hotpink = Color "hotpink"
{-# INLINE hotpink #-}


-- | Generates the CSS @indianred@ @\<named-color\>@ value.
indianred :: Color
indianred = Color "indianred"
{-# INLINE indianred #-}


-- | Generates the CSS @indigo@ @\<named-color\>@ value.
indigo :: Color
indigo = Color "indigo"
{-# INLINE indigo #-}


-- | Generates the CSS @ivory@ @\<named-color\>@ value.
ivory :: Color
ivory = Color "ivory"
{-# INLINE ivory #-}


-- | Generates the CSS @khaki@ @\<named-color\>@ value.
khaki :: Color
khaki = Color "khaki"
{-# INLINE khaki #-}


-- | Generates the CSS @lavender@ @\<named-color\>@ value.
lavender :: Color
lavender = Color "lavender"
{-# INLINE lavender #-}


-- | Generates the CSS @lavenderblush@ @\<named-color\>@ value.
lavenderblush :: Color
lavenderblush = Color "lavenderblush"
{-# INLINE lavenderblush #-}


-- | Generates the CSS @lawngreen@ @\<named-color\>@ value.
lawngreen :: Color
lawngreen = Color "lawngreen"
{-# INLINE lawngreen #-}


-- | Generates the CSS @lemonchiffon@ @\<named-color\>@ value.
lemonchiffon :: Color
lemonchiffon = Color "lemonchiffon"
{-# INLINE lemonchiffon #-}


-- | Generates the CSS @lightblue@ @\<named-color\>@ value.
lightblue :: Color
lightblue = Color "lightblue"
{-# INLINE lightblue #-}


-- | Generates the CSS @lightcoral@ @\<named-color\>@ value.
lightcoral :: Color
lightcoral = Color "lightcoral"
{-# INLINE lightcoral #-}


-- | Generates the CSS @lightcyan@ @\<named-color\>@ value.
lightcyan :: Color
lightcyan = Color "lightcyan"
{-# INLINE lightcyan #-}


-- | Generates the CSS @lightgoldenrodyellow@ @\<named-color\>@ value.
lightgoldenrodyellow :: Color
lightgoldenrodyellow = Color "lightgoldenrodyellow"
{-# INLINE lightgoldenrodyellow #-}


-- | Generates the CSS @lightgray@ @\<named-color\>@ value.
lightgray :: Color
lightgray = Color "lightgray"
{-# INLINE lightgray #-}


-- | Generates the CSS @lightgreen@ @\<named-color\>@ value.
lightgreen :: Color
lightgreen = Color "lightgreen"
{-# INLINE lightgreen #-}


-- | Generates the CSS @lightgrey@ @\<named-color\>@ value.
lightgrey :: Color
lightgrey = Color "lightgrey"
{-# INLINE lightgrey #-}


-- | Generates the CSS @lightpink@ @\<named-color\>@ value.
lightpink :: Color
lightpink = Color "lightpink"
{-# INLINE lightpink #-}


-- | Generates the CSS @lightsalmon@ @\<named-color\>@ value.
lightsalmon :: Color
lightsalmon = Color "lightsalmon"
{-# INLINE lightsalmon #-}


-- | Generates the CSS @lightseagreen@ @\<named-color\>@ value.
lightseagreen :: Color
lightseagreen = Color "lightseagreen"
{-# INLINE lightseagreen #-}


-- | Generates the CSS @lightskyblue@ @\<named-color\>@ value.
lightskyblue :: Color
lightskyblue = Color "lightskyblue"
{-# INLINE lightskyblue #-}


-- | Generates the CSS @lightslategray@ @\<named-color\>@ value.
lightslategray :: Color
lightslategray = Color "lightslategray"
{-# INLINE lightslategray #-}


-- | Generates the CSS @lightslategrey@ @\<named-color\>@ value.
lightslategrey :: Color
lightslategrey = Color "lightslategrey"
{-# INLINE lightslategrey #-}


-- | Generates the CSS @lightsteelblue@ @\<named-color\>@ value.
lightsteelblue :: Color
lightsteelblue = Color "lightsteelblue"
{-# INLINE lightsteelblue #-}


-- | Generates the CSS @lightyellow@ @\<named-color\>@ value.
lightyellow :: Color
lightyellow = Color "lightyellow"
{-# INLINE lightyellow #-}


-- | Generates the CSS @lime@ @\<named-color\>@ value.
lime :: Color
lime = Color "lime"
{-# INLINE lime #-}


-- | Generates the CSS @limegreen@ @\<named-color\>@ value.
limegreen :: Color
limegreen = Color "limegreen"
{-# INLINE limegreen #-}


-- | Generates the CSS @linen@ @\<named-color\>@ value.
linen :: Color
linen = Color "linen"
{-# INLINE linen #-}


-- | Generates the CSS @magenta@ @\<named-color\>@ value.
magenta :: Color
magenta = Color "magenta"
{-# INLINE magenta #-}


-- | Generates the CSS @maroon@ @\<named-color\>@ value.
maroon :: Color
maroon = Color "maroon"
{-# INLINE maroon #-}


-- | Generates the CSS @mediumaquamarine@ @\<named-color\>@ value.
mediumaquamarine :: Color
mediumaquamarine = Color "mediumaquamarine"
{-# INLINE mediumaquamarine #-}


-- | Generates the CSS @mediumblue@ @\<named-color\>@ value.
mediumblue :: Color
mediumblue = Color "mediumblue"
{-# INLINE mediumblue #-}


-- | Generates the CSS @mediumorchid@ @\<named-color\>@ value.
mediumorchid :: Color
mediumorchid = Color "mediumorchid"
{-# INLINE mediumorchid #-}


-- | Generates the CSS @mediumpurple@ @\<named-color\>@ value.
mediumpurple :: Color
mediumpurple = Color "mediumpurple"
{-# INLINE mediumpurple #-}


-- | Generates the CSS @mediumseagreen@ @\<named-color\>@ value.
mediumseagreen :: Color
mediumseagreen = Color "mediumseagreen"
{-# INLINE mediumseagreen #-}


-- | Generates the CSS @mediumslateblue@ @\<named-color\>@ value.
mediumslateblue :: Color
mediumslateblue = Color "mediumslateblue"
{-# INLINE mediumslateblue #-}


-- | Generates the CSS @mediumspringgreen@ @\<named-color\>@ value.
mediumspringgreen :: Color
mediumspringgreen = Color "mediumspringgreen"
{-# INLINE mediumspringgreen #-}


-- | Generates the CSS @mediumturquoise@ @\<named-color\>@ value.
mediumturquoise :: Color
mediumturquoise = Color "mediumturquoise"
{-# INLINE mediumturquoise #-}


-- | Generates the CSS @mediumvioletred@ @\<named-color\>@ value.
mediumvioletred :: Color
mediumvioletred = Color "mediumvioletred"
{-# INLINE mediumvioletred #-}


-- | Generates the CSS @midnightblue@ @\<named-color\>@ value.
midnightblue :: Color
midnightblue = Color "midnightblue"
{-# INLINE midnightblue #-}


-- | Generates the CSS @mintcream@ @\<named-color\>@ value.
mintcream :: Color
mintcream = Color "mintcream"
{-# INLINE mintcream #-}


-- | Generates the CSS @mistyrose@ @\<named-color\>@ value.
mistyrose :: Color
mistyrose = Color "mistyrose"
{-# INLINE mistyrose #-}


-- | Generates the CSS @moccasin@ @\<named-color\>@ value.
moccasin :: Color
moccasin = Color "moccasin"
{-# INLINE moccasin #-}


-- | Generates the CSS @navajowhite@ @\<named-color\>@ value.
navajowhite :: Color
navajowhite = Color "navajowhite"
{-# INLINE navajowhite #-}


-- | Generates the CSS @navy@ @\<named-color\>@ value.
navy :: Color
navy = Color "navy"
{-# INLINE navy #-}


-- | Generates the CSS @oldlace@ @\<named-color\>@ value.
oldlace :: Color
oldlace = Color "oldlace"
{-# INLINE oldlace #-}


-- | Generates the CSS @olive@ @\<named-color\>@ value.
olive :: Color
olive = Color "olive"
{-# INLINE olive #-}


-- | Generates the CSS @olivedrab@ @\<named-color\>@ value.
olivedrab :: Color
olivedrab = Color "olivedrab"
{-# INLINE olivedrab #-}


-- | Generates the CSS @orange@ @\<named-color\>@ value.
orange :: Color
orange = Color "orange"
{-# INLINE orange #-}


-- | Generates the CSS @orangered@ @\<named-color\>@ value.
orangered :: Color
orangered = Color "orangered"
{-# INLINE orangered #-}


-- | Generates the CSS @orchid@ @\<named-color\>@ value.
orchid :: Color
orchid = Color "orchid"
{-# INLINE orchid #-}


-- | Generates the CSS @palegoldenrod@ @\<named-color\>@ value.
palegoldenrod :: Color
palegoldenrod = Color "palegoldenrod"
{-# INLINE palegoldenrod #-}


-- | Generates the CSS @palegreen@ @\<named-color\>@ value.
palegreen :: Color
palegreen = Color "palegreen"
{-# INLINE palegreen #-}


-- | Generates the CSS @paleturquoise@ @\<named-color\>@ value.
paleturquoise :: Color
paleturquoise = Color "paleturquoise"
{-# INLINE paleturquoise #-}


-- | Generates the CSS @palevioletred@ @\<named-color\>@ value.
palevioletred :: Color
palevioletred = Color "palevioletred"
{-# INLINE palevioletred #-}


-- | Generates the CSS @papayawhip@ @\<named-color\>@ value.
papayawhip :: Color
papayawhip = Color "papayawhip"
{-# INLINE papayawhip #-}


-- | Generates the CSS @peachpuff@ @\<named-color\>@ value.
peachpuff :: Color
peachpuff = Color "peachpuff"
{-# INLINE peachpuff #-}


-- | Generates the CSS @peru@ @\<named-color\>@ value.
peru :: Color
peru = Color "peru"
{-# INLINE peru #-}


-- | Generates the CSS @pink@ @\<named-color\>@ value.
pink :: Color
pink = Color "pink"
{-# INLINE pink #-}


-- | Generates the CSS @plum@ @\<named-color\>@ value.
plum :: Color
plum = Color "plum"
{-# INLINE plum #-}


-- | Generates the CSS @powderblue@ @\<named-color\>@ value.
powderblue :: Color
powderblue = Color "powderblue"
{-# INLINE powderblue #-}


-- | Generates the CSS @purple@ @\<named-color\>@ value.
purple :: Color
purple = Color "purple"
{-# INLINE purple #-}


-- | Generates the CSS @rebeccapurple@ @\<named-color\>@ value.
rebeccapurple :: Color
rebeccapurple = Color "rebeccapurple"
{-# INLINE rebeccapurple #-}


-- | Generates the CSS @red@ @\<named-color\>@ value.
red :: Color
red = Color "red"
{-# INLINE red #-}


-- | Generates the CSS @rosybrown@ @\<named-color\>@ value.
rosybrown :: Color
rosybrown = Color "rosybrown"
{-# INLINE rosybrown #-}


-- | Generates the CSS @royalblue@ @\<named-color\>@ value.
royalblue :: Color
royalblue = Color "royalblue"
{-# INLINE royalblue #-}


-- | Generates the CSS @saddlebrown@ @\<named-color\>@ value.
saddlebrown :: Color
saddlebrown = Color "saddlebrown"
{-# INLINE saddlebrown #-}


-- | Generates the CSS @salmon@ @\<named-color\>@ value.
salmon :: Color
salmon = Color "salmon"
{-# INLINE salmon #-}


-- | Generates the CSS @sandybrown@ @\<named-color\>@ value.
sandybrown :: Color
sandybrown = Color "sandybrown"
{-# INLINE sandybrown #-}


-- | Generates the CSS @seagreen@ @\<named-color\>@ value.
seagreen :: Color
seagreen = Color "seagreen"
{-# INLINE seagreen #-}


-- | Generates the CSS @seashell@ @\<named-color\>@ value.
seashell :: Color
seashell = Color "seashell"
{-# INLINE seashell #-}


-- | Generates the CSS @sienna@ @\<named-color\>@ value.
sienna :: Color
sienna = Color "sienna"
{-# INLINE sienna #-}


-- | Generates the CSS @silver@ @\<named-color\>@ value.
silver :: Color
silver = Color "silver"
{-# INLINE silver #-}


-- | Generates the CSS @skyblue@ @\<named-color\>@ value.
skyblue :: Color
skyblue = Color "skyblue"
{-# INLINE skyblue #-}


-- | Generates the CSS @slateblue@ @\<named-color\>@ value.
slateblue :: Color
slateblue = Color "slateblue"
{-# INLINE slateblue #-}


-- | Generates the CSS @slategray@ @\<named-color\>@ value.
slategray :: Color
slategray = Color "slategray"
{-# INLINE slategray #-}


-- | Generates the CSS @slategrey@ @\<named-color\>@ value.
slategrey :: Color
slategrey = Color "slategrey"
{-# INLINE slategrey #-}


-- | Generates the CSS @snow@ @\<named-color\>@ value.
snow :: Color
snow = Color "snow"
{-# INLINE snow #-}


-- | Generates the CSS @springgreen@ @\<named-color\>@ value.
springgreen :: Color
springgreen = Color "springgreen"
{-# INLINE springgreen #-}


-- | Generates the CSS @steelblue@ @\<named-color\>@ value.
steelblue :: Color
steelblue = Color "steelblue"
{-# INLINE steelblue #-}


-- | Generates the CSS @tan@ @\<named-color\>@ value.
tan :: Color
tan = Color "tan"
{-# INLINE tan #-}


-- | Generates the CSS @teal@ @\<named-color\>@ value.
teal :: Color
teal = Color "teal"
{-# INLINE teal #-}


-- | Generates the CSS @thistle@ @\<named-color\>@ value.
thistle :: Color
thistle = Color "thistle"
{-# INLINE thistle #-}


-- | Generates the CSS @tomato@ @\<named-color\>@ value.
tomato :: Color
tomato = Color "tomato"
{-# INLINE tomato #-}


-- | Generates the CSS @turquoise@ @\<named-color\>@ value.
turquoise :: Color
turquoise = Color "turquoise"
{-# INLINE turquoise #-}


-- | Generates the CSS @violet@ @\<named-color\>@ value.
violet :: Color
violet = Color "violet"
{-# INLINE violet #-}


-- | Generates the CSS @wheat@ @\<named-color\>@ value.
wheat :: Color
wheat = Color "wheat"
{-# INLINE wheat #-}


-- | Generates the CSS @white@ @\<named-color\>@ value.
white :: Color
white = Color "white"
{-# INLINE white #-}


-- | Generates the CSS @whitesmoke@ @\<named-color\>@ value.
whitesmoke :: Color
whitesmoke = Color "whitesmoke"
{-# INLINE whitesmoke #-}


-- | Generates the CSS @yellow@ @\<named-color\>@ value.
yellow :: Color
yellow = Color "yellow"
{-# INLINE yellow #-}


-- | Generates the CSS @yellowgreen@ @\<named-color\>@ value.
yellowgreen :: Color
yellowgreen = Color "yellowgreen"
{-# INLINE yellowgreen #-}


-- * PREDEFINED-RGB


-- | Generates the CSS @a98-rgb@ @\<predefined-rgb\>@ value.
a98Rgb :: ColorspaceParams
a98Rgb = ColorspaceParams "a98-rgb"
{-# INLINE a98Rgb #-}


-- | Generates the CSS @display-p3@ @\<predefined-rgb\>@ value.
displayP3 :: ColorspaceParams
displayP3 = ColorspaceParams "display-p3"
{-# INLINE displayP3 #-}


-- | Generates the CSS @prophoto-rgb@ @\<predefined-rgb\>@ value.
prophotoRgb :: ColorspaceParams
prophotoRgb = ColorspaceParams "prophoto-rgb"
{-# INLINE prophotoRgb #-}


-- | Generates the CSS @rec2020@ @\<predefined-rgb\>@ value.
rec2020 :: ColorspaceParams
rec2020 = ColorspaceParams "rec2020"
{-# INLINE rec2020 #-}


-- | Generates the CSS @srgb@ @\<predefined-rgb\>@ value.
srgb :: ColorspaceParams
srgb = ColorspaceParams "srgb"
{-# INLINE srgb #-}


-- | Generates the CSS @srgb-linear@ @\<predefined-rgb\>@ value.
srgbLinear :: ColorspaceParams
srgbLinear = ColorspaceParams "srgb-linear"
{-# INLINE srgbLinear #-}


-- * SYSTEM-COLOR


-- | Generates the CSS @AccentColor@ @\<system-color\>@ value.
accentColor :: Color
accentColor = Color "AccentColor"
{-# INLINE accentColor #-}


-- | Generates the CSS @AccentColorText@ @\<system-color\>@ value.
accentColorText :: Color
accentColorText = Color "AccentColorText"
{-# INLINE accentColorText #-}


-- | Generates the CSS @ActiveText@ @\<system-color\>@ value.
activeText :: Color
activeText = Color "ActiveText"
{-# INLINE activeText #-}


-- | Generates the CSS @ButtonBorder@ @\<system-color\>@ value.
buttonBorder :: Color
buttonBorder = Color "ButtonBorder"
{-# INLINE buttonBorder #-}


-- | Generates the CSS @ButtonFace@ @\<system-color\>@ value.
buttonFace :: Color
buttonFace = Color "ButtonFace"
{-# INLINE buttonFace #-}


-- | Generates the CSS @ButtonText@ @\<system-color\>@ value.
buttonText :: Color
buttonText = Color "ButtonText"
{-# INLINE buttonText #-}


-- | Generates the CSS @Canvas@ @\<system-color\>@ value.
canvas :: Color
canvas = Color "Canvas"
{-# INLINE canvas #-}


-- | Generates the CSS @CanvasText@ @\<system-color\>@ value.
canvasText :: Color
canvasText = Color "CanvasText"
{-# INLINE canvasText #-}


-- | Generates the CSS @Field@ @\<system-color\>@ value.
field :: Color
field = Color "Field"
{-# INLINE field #-}


-- | Generates the CSS @FieldText@ @\<system-color\>@ value.
fieldText :: Color
fieldText = Color "FieldText"
{-# INLINE fieldText #-}


-- | Generates the CSS @GrayText@ @\<system-color\>@ value.
grayText :: Color
grayText = Color "GrayText"
{-# INLINE grayText #-}


-- | Generates the CSS @Highlight@ @\<system-color\>@ value.
highlight :: Color
highlight = Color "Highlight"
{-# INLINE highlight #-}


-- | Generates the CSS @HighlightText@ @\<system-color\>@ value.
highlightText :: Color
highlightText = Color "HighlightText"
{-# INLINE highlightText #-}


-- | Generates the CSS @LinkText@ @\<system-color\>@ value.
linkText :: Color
linkText = Color "LinkText"
{-# INLINE linkText #-}


-- | Generates the CSS @Mark@ @\<system-color\>@ value.
mark :: Color
mark = Color "Mark"
{-# INLINE mark #-}


-- | Generates the CSS @MarkText@ @\<system-color\>@ value.
markText :: Color
markText = Color "MarkText"
{-# INLINE markText #-}


-- | Generates the CSS @SelectedItem@ @\<system-color\>@ value.
selectedItem :: Color
selectedItem = Color "SelectedItem"
{-# INLINE selectedItem #-}


-- | Generates the CSS @SelectedItemText@ @\<system-color\>@ value.
selectedItemText :: Color
selectedItemText = Color "SelectedItemText"
{-# INLINE selectedItemText #-}


-- | Generates the CSS @VisitedText@ @\<system-color\>@ value.
visitedText :: Color
visitedText = Color "VisitedText"
{-# INLINE visitedText #-}


-- * XYZ-SPACE


-- | Generates the CSS @xyz@ @\<xyz-space\>@ value.
xyz :: ColorspaceParams
xyz = ColorspaceParams "xyz"
{-# INLINE xyz #-}


-- | Generates the CSS @xyz-d50@ @\<xyz-space\>@ value.
xyzD50 :: ColorspaceParams
xyzD50 = ColorspaceParams "xyz-d50"
{-# INLINE xyzD50 #-}


-- | Generates the CSS @xyz-d65@ @\<xyz-space\> value.
xyzD65 :: ColorspaceParams
xyzD65 = ColorspaceParams "xyz-d65"
{-# INLINE xyzD65 #-}
