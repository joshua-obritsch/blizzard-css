{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module    : Css.DataTypes.Colors
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Colors" module provides a set of types and functions for generating color data types in CSS.
module Css.DataTypes.Colors
    ( -- * Colors
      -- ** \<color\>
      Color
      -- ** \<absolute-color-base\>
    , AbsoluteColorBase
      -- ** \<absolute-color-function\>
    , AbsoluteColorFunction
      -- ** \<alpha-value\>
    , AlphaValue
      -- ** \<colorspace\>
    , Colorspace
      -- ** \<hue\>
    , Hue
      -- ** \<hue\>|none
    , HueNone
      -- ** \<number\>\<percentage\>|none
    , NumberPercentageNone
      -- ** \<percentage\>|none
    , PercentageNone
      -- ** \'currentcolor\'
    , Currentcolor
    , currentcolor
      -- ** \'transparent\'
    , Transparent
    , transparent

      -- * Functions
      -- ** \<color()\>
    , ColorFunction
    , color
    , colora
      -- ** \<hsl()\>
    , HslFunction
    , hsl
    , hsla
      -- ** \<hwb()\>
    , HwbFunction
    , hwb
    , hwba
      -- ** \<lab()\>
    , LabFunction
    , lab
    , laba
      -- ** \<lch()\>
    , LchFunction
    , lch
    , lcha
      -- ** \<oklab()\>
    , OklabFunction
    , oklab
    , oklaba
      -- ** \<oklch()\>
    , OklchFunction
    , oklch
    , oklcha
      -- ** \<rgb()\>
    , RgbFunction
    , rgb
    , rgba

      -- * Hexadecimal
      -- ** \<hex-color\>
    , HexColor
    , hex

      -- * Named Colors
      -- ** \<named-color\>
    , NamedColor
      -- ** \'aliceblue\'
    , aliceblue
      -- ** \'antiquewhite\'
    , antiquewhite
      -- ** \'aqua\'
    , aqua
      -- ** \'aquamarine\'
    , aquamarine
      -- ** \'azure\'
    , azure
      -- ** \'beige\'
    , beige
      -- ** \'bisque\'
    , bisque
      -- ** \'black\'
    , black
      -- ** \'blanchedalmond\'
    , blanchedalmond
      -- ** \'blue\'
    , blue
      -- ** \'blueviolet\'
    , blueviolet
      -- ** \'brown\'
    , brown
      -- ** \'burlywood\'
    , burlywood
      -- ** \'cadetblue\'
    , cadetblue
      -- ** \'chartreuse\'
    , chartreuse
      -- ** \'chocolate\'
    , chocolate
      -- ** \'coral\'
    , coral
      -- ** \'cornflowerblue\'
    , cornflowerblue
      -- ** \'cornsilk\'
    , cornsilk
      -- ** \'crimson\'
    , crimson
      -- ** \'cyan\'
    , cyan
      -- ** \'darkblue\'
    , darkblue
      -- ** \'darkcyan\'
    , darkcyan
      -- ** \'darkgoldenrod\'
    , darkgoldenrod
      -- ** \'darkgray\'
    , darkgray
      -- ** \'darkgreen\'
    , darkgreen
      -- ** \'darkgrey\'
    , darkgrey
      -- ** \'darkkhaki\'
    , darkkhaki
      -- ** \'darkmagenta\'
    , darkmagenta
      -- ** \'darkolivegreen\'
    , darkolivegreen
      -- ** \'darkorange\'
    , darkorange
      -- ** \'darkorchid\'
    , darkorchid
      -- ** \'darkred\'
    , darkred
      -- ** \'darksalmon\'
    , darksalmon
      -- ** \'darkseagreen\'
    , darkseagreen
      -- ** \'darkslateblue\'
    , darkslateblue
      -- ** \'darkslategray\'
    , darkslategray
      -- ** \'darkslategrey\'
    , darkslategrey
      -- ** \'darkturquoise\'
    , darkturquoise
      -- ** \'darkviolet\'
    , darkviolet
      -- ** \'deeppink\'
    , deeppink
      -- ** \'deepskyblue\'
    , deepskyblue
      -- ** \'dimgray\'
    , dimgray
      -- ** \'dimgrey\'
    , dimgrey
      -- ** \'dodgerblue\'
    , dodgerblue
      -- ** \'firebrick\'
    , firebrick
      -- ** \'floralwhite\'
    , floralwhite
      -- ** \'forestgreen\'
    , forestgreen
      -- ** \'fuchsia\'
    , fuchsia
      -- ** \'gainsboro\'
    , gainsboro
      -- ** \'ghostwhite\'
    , ghostwhite
      -- ** \'gold\'
    , gold
      -- ** \'goldenrod\'
    , goldenrod
      -- ** \'gray\'
    , gray
      -- ** \'green\'
    , green
      -- ** \'greenyellow\'
    , greenyellow
      -- ** \'grey\'
    , grey
      -- ** \'honeydew\'
    , honeydew
      -- ** \'hotpink\'
    , hotpink
      -- ** \'indianred\'
    , indianred
      -- ** \'indigo\'
    , indigo
      -- ** \'ivory\'
    , ivory
      -- ** \'khaki\'
    , khaki
      -- ** \'lavender\'
    , lavender
      -- ** \'lavenderblush\'
    , lavenderblush
      -- ** \'lawngreen\'
    , lawngreen
      -- ** \'lemonchiffon\'
    , lemonchiffon
      -- ** \'lightblue\'
    , lightblue
      -- ** \'lightcoral\'
    , lightcoral
      -- ** \'lightcyan\'
    , lightcyan
      -- ** \'lightgoldenrodyellow\'
    , lightgoldenrodyellow
      -- ** \'lightgray\'
    , lightgray
      -- ** \'lightgreen\'
    , lightgreen
      -- ** \'lightgrey\'
    , lightgrey
      -- ** \'lightpink\'
    , lightpink
      -- ** \'lightsalmon\'
    , lightsalmon
      -- ** \'lightseagreen\'
    , lightseagreen
      -- ** \'lightskyblue\'
    , lightskyblue
      -- ** \'lightslategray\'
    , lightslategray
      -- ** \'lightslategrey\'
    , lightslategrey
      -- ** \'lightsteelblue\'
    , lightsteelblue
      -- ** \'lightyellow\'
    , lightyellow
      -- ** \'lime\'
    , lime
      -- ** \'limegreen\'
    , limegreen
      -- ** \'linen\'
    , linen
      -- ** \'magenta\'
    , magenta
      -- ** \'maroon\'
    , maroon
      -- ** \'mediumaquamarine\'
    , mediumaquamarine
      -- ** \'mediumblue\'
    , mediumblue
      -- ** \'mediumorchid\'
    , mediumorchid
      -- ** \'mediumpurple\'
    , mediumpurple
      -- ** \'mediumseagreen\'
    , mediumseagreen
      -- ** \'mediumslateblue\'
    , mediumslateblue
      -- ** \'mediumspringgreen\'
    , mediumspringgreen
      -- ** \'mediumturquoise\'
    , mediumturquoise
      -- ** \'mediumvioletred\'
    , mediumvioletred
      -- ** \'midnightblue\'
    , midnightblue
      -- ** \'mintcream\'
    , mintcream
      -- ** \'mistyrose\'
    , mistyrose
      -- ** \'moccasin\'
    , moccasin
      -- ** \'navajowhite\'
    , navajowhite
      -- ** \'navy\'
    , navy
      -- ** \'oldlace\'
    , oldlace
      -- ** \'olive\'
    , olive
      -- ** \'olivedrab\'
    , olivedrab
      -- ** \'orange\'
    , orange
      -- ** \'orangered\'
    , orangered
      -- ** \'orchid\'
    , orchid
      -- ** \'palegoldenrod\'
    , palegoldenrod
      -- ** \'palegreen\'
    , palegreen
      -- ** \'paleturquoise\'
    , paleturquoise
      -- ** \'palevioletred\'
    , palevioletred
      -- ** \'papayawhip\'
    , papayawhip
      -- ** \'peachpuff\'
    , peachpuff
      -- ** \'peru\'
    , peru
      -- ** \'pink\'
    , pink
      -- ** \'plum\'
    , plum
      -- ** \'powderblue\'
    , powderblue
      -- ** \'purple\'
    , purple
      -- ** \'rebeccapurple\'
    , rebeccapurple
      -- ** \'red\'
    , red
      -- ** \'rosybrown\'
    , rosybrown
      -- ** \'royalblue\'
    , royalblue
      -- ** \'saddlebrown\'
    , saddlebrown
      -- ** \'salmon\'
    , salmon
      -- ** \'sandybrown\'
    , sandybrown
      -- ** \'seagreen\'
    , seagreen
      -- ** \'seashell\'
    , seashell
      -- ** \'sienna\'
    , sienna
      -- ** \'silver\'
    , silver
      -- ** \'skyblue\'
    , skyblue
      -- ** \'slateblue\'
    , slateblue
      -- ** \'slategray\'
    , slategray
      -- ** \'slategrey\'
    , slategrey
      -- ** \'snow\'
    , snow
      -- ** \'springgreen\'
    , springgreen
      -- ** \'steelblue\'
    , steelblue
      -- ** \'tan\'
    , tan
      -- ** \'teal\'
    , teal
      -- ** \'thistle\'
    , thistle
      -- ** \'tomato\'
    , tomato
      -- ** \'turquoise\'
    , turquoise
      -- ** \'violet\'
    , violet
      -- ** \'wheat\'
    , wheat
      -- ** \'white\'
    , white
      -- ** \'whitesmoke\'
    , whitesmoke
      -- ** \'yellow\'
    , yellow
      -- ** \'yellowgreen\'
    , yellowgreen

      -- * Predefined Rgb
      -- ** \<predefined-rgb\>
    , PredefinedRgb
      -- ** \'a98-rgb\'
    , a98Rgb
      -- ** \'display-p3\'
    , displayP3
      -- ** \'prophoto-rgb\'
    , prophotoRgb
      -- ** \'rec2020\'
    , rec2020
      -- ** \'srgb\'
    , srgb
      -- ** \'srgb-linear\'
    , srgbLinear

      -- * System Colors
      -- ** \<system-color\>
    , SystemColor
      -- ** \'AccentColor\'
    , accentColor
      -- ** \'AccentColorText\'
    , accentColorText
      -- ** \'ActiveText\'
    , activeText
      -- ** \'ButtonBorder\'
    , buttonBorder
      -- ** \'ButtonFace\'
    , buttonFace
      -- ** \'ButtonText\'
    , buttonText
      -- ** \'Canvas\'
    , canvas
      -- ** \'CanvasText\'
    , canvasText
      -- ** \'Field\'
    , field
      -- ** \'FieldText\'
    , fieldText
      -- ** \'GrayText\'
    , grayText
      -- ** \'Highlight\'
    , highlight
      -- ** \'HighlightText\'
    , highlightText
      -- ** \'LinkText\'
    , linkText
      -- ** \'Mark\'
    , mark
      -- ** \'MarkText\'
    , markText
      -- ** \'SelectedItem\'
    , selectedItem
      -- ** \'SelectedItemText\'
    , selectedItemText
      -- ** \'VisitedText\'
    , visitedText

      -- * Xyz Space
      -- ** \<xyz-space\>
    , XyzSpace
      -- ** \'xyz\'
    , xyz
      -- ** \'xyz-d50\'
    , xyzD50
      -- ** \'xyz-d65\'
    , xyzD65
    ) where


import Prelude hiding (tan)

import Css.DataTypes.Numeric  (Angle, Number, Percentage)
import Css.Internal           (showHex)
import Css.Keywords           (None)
import Data.Text.Lazy.Builder (Builder, singleton)
import Data.Word              (Word32)
import Html                   (Buildable(..))


-- * COLORS


-- | Represents the CSS @\<color\>@ data type.
class Buildable a => Color a


instance {-# OVERLAPPING #-}                   Color Currentcolor
instance {-# OVERLAPPING #-}                   Color SystemColor
instance (Buildable a, AbsoluteColorBase a) => Color a


-- | Represents the CSS @\<absolute-color-base\>@ data type.
class Buildable a => AbsoluteColorBase a


instance {-# OVERLAPPING #-}                       AbsoluteColorBase HexColor
instance {-# OVERLAPPING #-}                       AbsoluteColorBase NamedColor
instance {-# OVERLAPPING #-}                       AbsoluteColorBase Transparent
instance (Buildable a, AbsoluteColorFunction a) => AbsoluteColorBase a


-- | Represents the CSS @\<absolute-color-function\>@ data type.
class Buildable a => AbsoluteColorFunction a


instance AbsoluteColorFunction RgbFunction
instance AbsoluteColorFunction HslFunction
instance AbsoluteColorFunction HwbFunction
instance AbsoluteColorFunction LabFunction
instance AbsoluteColorFunction LchFunction
instance AbsoluteColorFunction OklabFunction
instance AbsoluteColorFunction OklchFunction
instance AbsoluteColorFunction ColorFunction


-- | Represents the CSS @\<alpha-value\>@ data type.
type AlphaValue = NumberPercentageNone


-- | Represents the colorspace in the CSS @\<colorspace-params\>@ data type.
class Buildable a => Colorspace a


instance Colorspace PredefinedRgb
instance Colorspace XyzSpace


-- | Represents the CSS @\<hue\>@ data type.
class Buildable a => Hue a


instance Hue Angle
instance Hue Number


-- | Represents the CSS @\<hue\>|none@ data type.
class Buildable a => HueNone a


instance {-# OVERLAPPING #-}     HueNone None
instance (Buildable a, Hue a) => HueNone a


-- | Represents the CSS @\<number\>|\<percentage\>|none@ data type.
class Buildable a => NumberPercentageNone a


instance NumberPercentageNone None
instance NumberPercentageNone Number
instance NumberPercentageNone Percentage


-- | Represents the CSS @\<percentage\>|none@ data type.
class Buildable a => PercentageNone a


instance PercentageNone None
instance PercentageNone Percentage


-- | Represents the CSS @currentcolor@ keyword.
newtype Currentcolor = Currentcolor Builder
    deriving (Buildable, Show)


-- | Generates the CSS @currentcolor@ keyword.
currentcolor :: Currentcolor
currentcolor = Currentcolor "currentcolor"
{-# INLINE currentcolor #-}


-- | Represents the CSS @transparent@ keyword.
newtype Transparent = Transparent Builder
    deriving (Buildable, Show)


-- | Generates the CSS @transparent@ keyword.
transparent :: Transparent
transparent = Transparent "transparent"
{-# INLINE transparent #-}


-- * FUNCTIONS


-- | Represents the CSS @\<color()\>@ data type.
newtype ColorFunction = ColorFunction Builder
    deriving (Buildable, Show)


color :: (Colorspace a, NumberPercentageNone b, NumberPercentageNone c, NumberPercentageNone d) => a -> b -> c -> d -> ColorFunction
color colorspace c1 c2 c3
    =  ColorFunction
    $  "color("
    <> build colorspace
    <> singleton ' '
    <> build c1
    <> singleton ' '
    <> build c2
    <> singleton ' '
    <> build c3
    <> singleton ')'


colora :: (Colorspace a, NumberPercentageNone b, NumberPercentageNone c, NumberPercentageNone d, AlphaValue e)
       => a -> b -> c -> d -> e -> ColorFunction
colora colorspace c1 c2 c3 alpha
    =  ColorFunction
    $  "color("
    <> build colorspace
    <> singleton ' '
    <> build c1
    <> singleton ' '
    <> build c2
    <> singleton ' '
    <> build c3
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<hsl()\>@ data type.
newtype HslFunction = HslFunction Builder
    deriving (Buildable, Show)


hsl :: (HueNone a, PercentageNone b, PercentageNone c) => a -> b -> c -> HslFunction
hsl hue saturation lightness
    =  HslFunction
    $  "hsl("
    <> build hue
    <> singleton ' '
    <> build saturation
    <> singleton ' '
    <> build lightness
    <> singleton ')'


hsla :: (HueNone a, PercentageNone b, PercentageNone c, AlphaValue d) => a -> b -> c -> d -> HslFunction
hsla hue saturation lightness alpha
    =  HslFunction
    $  "hsl("
    <> build hue
    <> singleton ' '
    <> build saturation
    <> singleton ' '
    <> build lightness
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<hwb()\>@ data type.
newtype HwbFunction = HwbFunction Builder
    deriving (Buildable, Show)


hwb :: (HueNone a, PercentageNone b, PercentageNone c) => a -> b -> c -> HwbFunction
hwb hue whiteness blackness
    =  HwbFunction
    $  "hwb("
    <> build hue
    <> singleton ' '
    <> build whiteness
    <> singleton ' '
    <> build blackness
    <> singleton ')'


hwba :: (HueNone a, PercentageNone b, PercentageNone c, AlphaValue d) => a -> b -> c -> d -> HwbFunction
hwba hue whiteness blackness alpha
    =  HwbFunction
    $  "hwb("
    <> build hue
    <> singleton ' '
    <> build whiteness
    <> singleton ' '
    <> build blackness
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<lab()\>@ data type.
newtype LabFunction = LabFunction Builder
    deriving (Buildable, Show)


lab :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => a -> b -> c -> LabFunction
lab lightness a b
    =  LabFunction
    $  "lab("
    <> build lightness
    <> singleton ' '
    <> build a
    <> singleton ' '
    <> build b
    <> singleton ')'


laba :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => a -> b -> c -> d -> LabFunction
laba lightness a b alpha
    =  LabFunction
    $  "lab("
    <> build lightness
    <> singleton ' '
    <> build a
    <> singleton ' '
    <> build b
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<lch()\>@ data type.
newtype LchFunction = LchFunction Builder
    deriving (Buildable, Show)


lch :: (NumberPercentageNone a, NumberPercentageNone b, HueNone c) => a -> b -> c -> LchFunction
lch lightness chroma hue
    =  LchFunction
    $  "lch("
    <> build lightness
    <> singleton ' '
    <> build chroma
    <> singleton ' '
    <> build hue
    <> singleton ')'


lcha :: (NumberPercentageNone a, NumberPercentageNone b, HueNone c, AlphaValue d) => a -> b -> c -> d -> LchFunction
lcha lightness chroma hue alpha
    =  LchFunction
    $  "lch("
    <> build lightness
    <> singleton ' '
    <> build chroma
    <> singleton ' '
    <> build hue
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<oklab()\>@ data type.
newtype OklabFunction = OklabFunction Builder
    deriving (Buildable, Show)


oklab :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => a -> b -> c -> OklabFunction
oklab lightness a b
    =  OklabFunction
    $  "oklab("
    <> build lightness
    <> singleton ' '
    <> build a
    <> singleton ' '
    <> build b
    <> singleton ')'


oklaba :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => a -> b -> c -> d -> OklabFunction
oklaba lightness a b alpha
    =  OklabFunction
    $  "oklab("
    <> build lightness
    <> singleton ' '
    <> build a
    <> singleton ' '
    <> build b
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<oklch()\>@ data type.
newtype OklchFunction = OklchFunction Builder
    deriving (Buildable, Show)


oklch :: (NumberPercentageNone a, NumberPercentageNone b, HueNone c) => a -> b -> c -> OklchFunction
oklch lightness chroma hue
    =  OklchFunction
    $  "oklch("
    <> build lightness
    <> singleton ' '
    <> build chroma
    <> singleton ' '
    <> build hue
    <> singleton ')'


oklcha :: (NumberPercentageNone a, NumberPercentageNone b, HueNone c, AlphaValue d) => a -> b -> c -> d -> OklchFunction
oklcha lightness chroma hue alpha
    =  OklchFunction
    $  "oklch("
    <> build lightness
    <> singleton ' '
    <> build chroma
    <> singleton ' '
    <> build hue
    <> " / "
    <> build alpha
    <> singleton ')'


-- | Represents the CSS @\<rgb()\>@ data type.
newtype RgbFunction = RgbFunction Builder
    deriving (Buildable, Show)


rgb :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c) => a -> b -> c -> RgbFunction
rgb red green blue
    =  RgbFunction
    $  "rgb("
    <> build red
    <> singleton ' '
    <> build green
    <> singleton ' '
    <> build blue
    <> singleton ')'


rgba :: (NumberPercentageNone a, NumberPercentageNone b, NumberPercentageNone c, AlphaValue d) => a -> b -> c -> d -> RgbFunction
rgba red green blue alpha
    =  RgbFunction
    $  "rgb("
    <> build red
    <> singleton ' '
    <> build green
    <> singleton ' '
    <> build blue
    <> " / "
    <> build alpha
    <> singleton ')'


-- HEXADECIMAL


-- | Represents the CSS @\<hex-color\>@ data type.
newtype HexColor = HexColor Builder
    deriving (Buildable, Show)


-- | Generates a CSS \<hex-color\> value.
hex :: Word32 -> HexColor
hex = HexColor . showHex
{-# INLINE hex #-}


-- * NAMED COLORS


-- | Represents the CSS @\<named-color\>@ data type.
newtype NamedColor = NamedColor Builder
    deriving (Buildable, Show)


-- | Generates the CSS @aliceblue@ \<named-color\> value.
aliceblue :: NamedColor
aliceblue = NamedColor "aliceblue"
{-# INLINE aliceblue #-}


-- | Generates the CSS @antiquewhite@ \<named-color\> value.
antiquewhite :: NamedColor
antiquewhite = NamedColor "antiquewhite"
{-# INLINE antiquewhite #-}


-- | Generates the CSS @aqua@ \<named-color\> value.
aqua :: NamedColor
aqua = NamedColor "aqua"
{-# INLINE aqua #-}


-- | Generates the CSS @aquamarine@ \<named-color\> value.
aquamarine :: NamedColor
aquamarine = NamedColor "aquamarine"
{-# INLINE aquamarine #-}


-- | Generates the CSS @azure@ \<named-color\> value.
azure :: NamedColor
azure = NamedColor "azure"
{-# INLINE azure #-}


-- | Generates the CSS @beige@ \<named-color\> value.
beige :: NamedColor
beige = NamedColor "beige"
{-# INLINE beige #-}


-- | Generates the CSS @bisque@ \<named-color\> value.
bisque :: NamedColor
bisque = NamedColor "bisque"
{-# INLINE bisque #-}


-- | Generates the CSS @black@ \<named-color\> value.
black :: NamedColor
black = NamedColor "black"
{-# INLINE black #-}


-- | Generates the CSS @blanchedalmond@ \<named-color\> value.
blanchedalmond :: NamedColor
blanchedalmond = NamedColor "blanchedalmond"
{-# INLINE blanchedalmond #-}


-- | Generates the CSS @blue@ \<named-color\> value.
blue :: NamedColor
blue = NamedColor "blue"
{-# INLINE blue #-}


-- | Generates the CSS @blueviolet@ @\<named-color\>@ value.
blueviolet :: NamedColor
blueviolet = NamedColor "blueviolet"
{-# INLINE blueviolet #-}


-- | Generates the CSS @brown@ @\<named-color\>@ value.
brown :: NamedColor
brown = NamedColor "brown"
{-# INLINE brown #-}


-- | Generates the CSS @burlywood@ @\<named-color\>@ value.
burlywood :: NamedColor
burlywood = NamedColor "burlywood"
{-# INLINE burlywood #-}


-- | Generates the CSS @cadetblue@ @\<named-color\>@ value.
cadetblue :: NamedColor
cadetblue = NamedColor "cadetblue"
{-# INLINE cadetblue #-}


-- | Generates the CSS @chartreuse@ @\<named-color\>@ value.
chartreuse :: NamedColor
chartreuse = NamedColor "chartreuse"
{-# INLINE chartreuse #-}


-- | Generates the CSS @chocolate@ @\<named-color\>@ value.
chocolate :: NamedColor
chocolate = NamedColor "chocolate"
{-# INLINE chocolate #-}


-- | Generates the CSS @coral@ @\<named-color\>@ value.
coral :: NamedColor
coral = NamedColor "coral"
{-# INLINE coral #-}


-- | Generates the CSS @cornflowerblue@ @\<named-color\>@ value.
cornflowerblue :: NamedColor
cornflowerblue = NamedColor "cornflowerblue"
{-# INLINE cornflowerblue #-}


-- | Generates the CSS @cornsilk@ @\<named-color\>@ value.
cornsilk :: NamedColor
cornsilk = NamedColor "cornsilk"
{-# INLINE cornsilk #-}


-- | Generates the CSS @crimson@ @\<named-color\>@ value.
crimson :: NamedColor
crimson = NamedColor "crimson"
{-# INLINE crimson #-}


-- | Generates the CSS @cyan@ @\<named-color\>@ value.
cyan :: NamedColor
cyan = NamedColor "cyan"
{-# INLINE cyan #-}


-- | Generates the CSS @darkblue@ @\<named-color\>@ value.
darkblue :: NamedColor
darkblue = NamedColor "darkblue"
{-# INLINE darkblue #-}


-- | Generates the CSS @darkcyan@ @\<named-color\>@ value.
darkcyan :: NamedColor
darkcyan = NamedColor "darkcyan"
{-# INLINE darkcyan #-}


-- | Generates the CSS @darkgoldenrod@ @\<named-color\>@ value.
darkgoldenrod :: NamedColor
darkgoldenrod = NamedColor "darkgoldenrod"
{-# INLINE darkgoldenrod #-}


-- | Generates the CSS @darkgray@ @\<named-color\>@ value.
darkgray :: NamedColor
darkgray = NamedColor "darkgray"
{-# INLINE darkgray #-}


-- | Generates the CSS @darkgreen@ @\<named-color\>@ value.
darkgreen :: NamedColor
darkgreen = NamedColor "darkgreen"
{-# INLINE darkgreen #-}


-- | Generates the CSS @darkgrey@ @\<named-color\>@ value.
darkgrey :: NamedColor
darkgrey = NamedColor "darkgrey"
{-# INLINE darkgrey #-}


-- | Generates the CSS @darkkhaki@ @\<named-color\>@ value.
darkkhaki :: NamedColor
darkkhaki = NamedColor "darkkhaki"
{-# INLINE darkkhaki #-}


-- | Generates the CSS @darkmagenta@ @\<named-color\>@ value.
darkmagenta :: NamedColor
darkmagenta = NamedColor "darkmagenta"
{-# INLINE darkmagenta #-}


-- | Generates the CSS @darkolivegreen@ @\<named-color\>@ value.
darkolivegreen :: NamedColor
darkolivegreen = NamedColor "darkolivegreen"
{-# INLINE darkolivegreen #-}


-- | Generates the CSS @darkorange@ @\<named-color\>@ value.
darkorange :: NamedColor
darkorange = NamedColor "darkorange"
{-# INLINE darkorange #-}


-- | Generates the CSS @darkorchid@ @\<named-color\>@ value.
darkorchid :: NamedColor
darkorchid = NamedColor "darkorchid"
{-# INLINE darkorchid #-}


-- | Generates the CSS @darkred@ @\<named-color\>@ value.
darkred :: NamedColor
darkred = NamedColor "darkred"
{-# INLINE darkred #-}


-- | Generates the CSS @darksalmon@ @\<named-color\>@ value.
darksalmon :: NamedColor
darksalmon = NamedColor "darksalmon"
{-# INLINE darksalmon #-}


-- | Generates the CSS @darkseagreen@ @\<named-color\>@ value.
darkseagreen :: NamedColor
darkseagreen = NamedColor "darkseagreen"
{-# INLINE darkseagreen #-}


-- | Generates the CSS @darkslateblue@ @\<named-color\>@ value.
darkslateblue :: NamedColor
darkslateblue = NamedColor "darkslateblue"
{-# INLINE darkslateblue #-}


-- | Generates the CSS @darkslategray@ @\<named-color\>@ value.
darkslategray :: NamedColor
darkslategray = NamedColor "darkslategray"
{-# INLINE darkslategray #-}


-- | Generates the CSS @darkslategrey@ @\<named-color\>@ value.
darkslategrey :: NamedColor
darkslategrey = NamedColor "darkslategrey"
{-# INLINE darkslategrey #-}


-- | Generates the CSS @darkturquoise@ @\<named-color\>@ value.
darkturquoise :: NamedColor
darkturquoise = NamedColor "darkturquoise"
{-# INLINE darkturquoise #-}


-- | Generates the CSS @darkviolet@ @\<named-color\>@ value.
darkviolet :: NamedColor
darkviolet = NamedColor "darkviolet"
{-# INLINE darkviolet #-}


-- | Generates the CSS @deeppink@ @\<named-color\>@ value.
deeppink :: NamedColor
deeppink = NamedColor "deeppink"
{-# INLINE deeppink #-}


-- | Generates the CSS @deepskyblue@ @\<named-color\>@ value.
deepskyblue :: NamedColor
deepskyblue = NamedColor "deepskyblue"
{-# INLINE deepskyblue #-}


-- | Generates the CSS @dimgray@ @\<named-color\>@ value.
dimgray :: NamedColor
dimgray = NamedColor "dimgray"
{-# INLINE dimgray #-}


-- | Generates the CSS @dimgrey@ @\<named-color\>@ value.
dimgrey :: NamedColor
dimgrey = NamedColor "dimgrey"
{-# INLINE dimgrey #-}


-- | Generates the CSS @dodgerblue@ @\<named-color\>@ value.
dodgerblue :: NamedColor
dodgerblue = NamedColor "dodgerblue"
{-# INLINE dodgerblue #-}


-- | Generates the CSS @firebrick@ @\<named-color\>@ value.
firebrick :: NamedColor
firebrick = NamedColor "firebrick"
{-# INLINE firebrick #-}


-- | Generates the CSS @floralwhite@ @\<named-color\>@ value.
floralwhite :: NamedColor
floralwhite = NamedColor "floralwhite"
{-# INLINE floralwhite #-}


-- | Generates the CSS @forestgreen@ @\<named-color\>@ value.
forestgreen :: NamedColor
forestgreen = NamedColor "forestgreen"
{-# INLINE forestgreen #-}


-- | Generates the CSS @fuchsia@ @\<named-color\>@ value.
fuchsia :: NamedColor
fuchsia = NamedColor "fuchsia"
{-# INLINE fuchsia #-}


-- | Generates the CSS @gainsboro@ @\<named-color\>@ value.
gainsboro :: NamedColor
gainsboro = NamedColor "gainsboro"
{-# INLINE gainsboro #-}


-- | Generates the CSS @ghostwhite@ @\<named-color\>@ value.
ghostwhite :: NamedColor
ghostwhite = NamedColor "ghostwhite"
{-# INLINE ghostwhite #-}


-- | Generates the CSS @gold@ @\<named-color\>@ value.
gold :: NamedColor
gold = NamedColor "gold"
{-# INLINE gold #-}


-- | Generates the CSS @goldenrod@ @\<named-color\>@ value.
goldenrod :: NamedColor
goldenrod = NamedColor "goldenrod"
{-# INLINE goldenrod #-}


-- | Generates the CSS @gray@ @\<named-color\>@ value.
gray :: NamedColor
gray = NamedColor "gray"
{-# INLINE gray #-}


-- | Generates the CSS @green@ @\<named-color\>@ value.
green :: NamedColor
green = NamedColor "green"
{-# INLINE green #-}


-- | Generates the CSS @greenyellow@ @\<named-color\>@ value.
greenyellow :: NamedColor
greenyellow = NamedColor "greenyellow"
{-# INLINE greenyellow #-}


-- | Generates the CSS @grey@ @\<named-color\>@ value.
grey :: NamedColor
grey = NamedColor "grey"
{-# INLINE grey #-}


-- | Generates the CSS @honeydew@ @\<named-color\>@ value.
honeydew :: NamedColor
honeydew = NamedColor "honeydew"
{-# INLINE honeydew #-}


-- | Generates the CSS @hotpink@ @\<named-color\>@ value.
hotpink :: NamedColor
hotpink = NamedColor "hotpink"
{-# INLINE hotpink #-}


-- | Generates the CSS @indianred@ @\<named-color\>@ value.
indianred :: NamedColor
indianred = NamedColor "indianred"
{-# INLINE indianred #-}


-- | Generates the CSS @indigo@ @\<named-color\>@ value.
indigo :: NamedColor
indigo = NamedColor "indigo"
{-# INLINE indigo #-}


-- | Generates the CSS @ivory@ @\<named-color\>@ value.
ivory :: NamedColor
ivory = NamedColor "ivory"
{-# INLINE ivory #-}


-- | Generates the CSS @khaki@ @\<named-color\>@ value.
khaki :: NamedColor
khaki = NamedColor "khaki"
{-# INLINE khaki #-}


-- | Generates the CSS @lavender@ @\<named-color\>@ value.
lavender :: NamedColor
lavender = NamedColor "lavender"
{-# INLINE lavender #-}


-- | Generates the CSS @lavenderblush@ @\<named-color\>@ value.
lavenderblush :: NamedColor
lavenderblush = NamedColor "lavenderblush"
{-# INLINE lavenderblush #-}


-- | Generates the CSS @lawngreen@ @\<named-color\>@ value.
lawngreen :: NamedColor
lawngreen = NamedColor "lawngreen"
{-# INLINE lawngreen #-}


-- | Generates the CSS @lemonchiffon@ @\<named-color\>@ value.
lemonchiffon :: NamedColor
lemonchiffon = NamedColor "lemonchiffon"
{-# INLINE lemonchiffon #-}


-- | Generates the CSS @lightblue@ @\<named-color\>@ value.
lightblue :: NamedColor
lightblue = NamedColor "lightblue"
{-# INLINE lightblue #-}


-- | Generates the CSS @lightcoral@ @\<named-color\>@ value.
lightcoral :: NamedColor
lightcoral = NamedColor "lightcoral"
{-# INLINE lightcoral #-}


-- | Generates the CSS @lightcyan@ @\<named-color\>@ value.
lightcyan :: NamedColor
lightcyan = NamedColor "lightcyan"
{-# INLINE lightcyan #-}


-- | Generates the CSS @lightgoldenrodyellow@ @\<named-color\>@ value.
lightgoldenrodyellow :: NamedColor
lightgoldenrodyellow = NamedColor "lightgoldenrodyellow"
{-# INLINE lightgoldenrodyellow #-}


-- | Generates the CSS @lightgray@ @\<named-color\>@ value.
lightgray :: NamedColor
lightgray = NamedColor "lightgray"
{-# INLINE lightgray #-}


-- | Generates the CSS @lightgreen@ @\<named-color\>@ value.
lightgreen :: NamedColor
lightgreen = NamedColor "lightgreen"
{-# INLINE lightgreen #-}


-- | Generates the CSS @lightgrey@ @\<named-color\>@ value.
lightgrey :: NamedColor
lightgrey = NamedColor "lightgrey"
{-# INLINE lightgrey #-}


-- | Generates the CSS @lightpink@ @\<named-color\>@ value.
lightpink :: NamedColor
lightpink = NamedColor "lightpink"
{-# INLINE lightpink #-}


-- | Generates the CSS @lightsalmon@ @\<named-color\>@ value.
lightsalmon :: NamedColor
lightsalmon = NamedColor "lightsalmon"
{-# INLINE lightsalmon #-}


-- | Generates the CSS @lightseagreen@ @\<named-color\>@ value.
lightseagreen :: NamedColor
lightseagreen = NamedColor "lightseagreen"
{-# INLINE lightseagreen #-}


-- | Generates the CSS @lightskyblue@ @\<named-color\>@ value.
lightskyblue :: NamedColor
lightskyblue = NamedColor "lightskyblue"
{-# INLINE lightskyblue #-}


-- | Generates the CSS @lightslategray@ @\<named-color\>@ value.
lightslategray :: NamedColor
lightslategray = NamedColor "lightslategray"
{-# INLINE lightslategray #-}


-- | Generates the CSS @lightslategrey@ @\<named-color\>@ value.
lightslategrey :: NamedColor
lightslategrey = NamedColor "lightslategrey"
{-# INLINE lightslategrey #-}


-- | Generates the CSS @lightsteelblue@ @\<named-color\>@ value.
lightsteelblue :: NamedColor
lightsteelblue = NamedColor "lightsteelblue"
{-# INLINE lightsteelblue #-}


-- | Generates the CSS @lightyellow@ @\<named-color\>@ value.
lightyellow :: NamedColor
lightyellow = NamedColor "lightyellow"
{-# INLINE lightyellow #-}


-- | Generates the CSS @lime@ @\<named-color\>@ value.
lime :: NamedColor
lime = NamedColor "lime"
{-# INLINE lime #-}


-- | Generates the CSS @limegreen@ @\<named-color\>@ value.
limegreen :: NamedColor
limegreen = NamedColor "limegreen"
{-# INLINE limegreen #-}


-- | Generates the CSS @linen@ @\<named-color\>@ value.
linen :: NamedColor
linen = NamedColor "linen"
{-# INLINE linen #-}


-- | Generates the CSS @magenta@ @\<named-color\>@ value.
magenta :: NamedColor
magenta = NamedColor "magenta"
{-# INLINE magenta #-}


-- | Generates the CSS @maroon@ @\<named-color\>@ value.
maroon :: NamedColor
maroon = NamedColor "maroon"
{-# INLINE maroon #-}


-- | Generates the CSS @mediumaquamarine@ @\<named-color\>@ value.
mediumaquamarine :: NamedColor
mediumaquamarine = NamedColor "mediumaquamarine"
{-# INLINE mediumaquamarine #-}


-- | Generates the CSS @mediumblue@ @\<named-color\>@ value.
mediumblue :: NamedColor
mediumblue = NamedColor "mediumblue"
{-# INLINE mediumblue #-}


-- | Generates the CSS @mediumorchid@ @\<named-color\>@ value.
mediumorchid :: NamedColor
mediumorchid = NamedColor "mediumorchid"
{-# INLINE mediumorchid #-}


-- | Generates the CSS @mediumpurple@ @\<named-color\>@ value.
mediumpurple :: NamedColor
mediumpurple = NamedColor "mediumpurple"
{-# INLINE mediumpurple #-}


-- | Generates the CSS @mediumseagreen@ @\<named-color\>@ value.
mediumseagreen :: NamedColor
mediumseagreen = NamedColor "mediumseagreen"
{-# INLINE mediumseagreen #-}


-- | Generates the CSS @mediumslateblue@ @\<named-color\>@ value.
mediumslateblue :: NamedColor
mediumslateblue = NamedColor "mediumslateblue"
{-# INLINE mediumslateblue #-}


-- | Generates the CSS @mediumspringgreen@ @\<named-color\>@ value.
mediumspringgreen :: NamedColor
mediumspringgreen = NamedColor "mediumspringgreen"
{-# INLINE mediumspringgreen #-}


-- | Generates the CSS @mediumturquoise@ @\<named-color\>@ value.
mediumturquoise :: NamedColor
mediumturquoise = NamedColor "mediumturquoise"
{-# INLINE mediumturquoise #-}


-- | Generates the CSS @mediumvioletred@ @\<named-color\>@ value.
mediumvioletred :: NamedColor
mediumvioletred = NamedColor "mediumvioletred"
{-# INLINE mediumvioletred #-}


-- | Generates the CSS @midnightblue@ @\<named-color\>@ value.
midnightblue :: NamedColor
midnightblue = NamedColor "midnightblue"
{-# INLINE midnightblue #-}


-- | Generates the CSS @mintcream@ @\<named-color\>@ value.
mintcream :: NamedColor
mintcream = NamedColor "mintcream"
{-# INLINE mintcream #-}


-- | Generates the CSS @mistyrose@ @\<named-color\>@ value.
mistyrose :: NamedColor
mistyrose = NamedColor "mistyrose"
{-# INLINE mistyrose #-}


-- | Generates the CSS @moccasin@ @\<named-color\>@ value.
moccasin :: NamedColor
moccasin = NamedColor "moccasin"
{-# INLINE moccasin #-}


-- | Generates the CSS @navajowhite@ @\<named-color\>@ value.
navajowhite :: NamedColor
navajowhite = NamedColor "navajowhite"
{-# INLINE navajowhite #-}


-- | Generates the CSS @navy@ @\<named-color\>@ value.
navy :: NamedColor
navy = NamedColor "navy"
{-# INLINE navy #-}


-- | Generates the CSS @oldlace@ @\<named-color\>@ value.
oldlace :: NamedColor
oldlace = NamedColor "oldlace"
{-# INLINE oldlace #-}


-- | Generates the CSS @olive@ @\<named-color\>@ value.
olive :: NamedColor
olive = NamedColor "olive"
{-# INLINE olive #-}


-- | Generates the CSS @olivedrab@ @\<named-color\>@ value.
olivedrab :: NamedColor
olivedrab = NamedColor "olivedrab"
{-# INLINE olivedrab #-}


-- | Generates the CSS @orange@ @\<named-color\>@ value.
orange :: NamedColor
orange = NamedColor "orange"
{-# INLINE orange #-}


-- | Generates the CSS @orangered@ @\<named-color\>@ value.
orangered :: NamedColor
orangered = NamedColor "orangered"
{-# INLINE orangered #-}


-- | Generates the CSS @orchid@ @\<named-color\>@ value.
orchid :: NamedColor
orchid = NamedColor "orchid"
{-# INLINE orchid #-}


-- | Generates the CSS @palegoldenrod@ @\<named-color\>@ value.
palegoldenrod :: NamedColor
palegoldenrod = NamedColor "palegoldenrod"
{-# INLINE palegoldenrod #-}


-- | Generates the CSS @palegreen@ @\<named-color\>@ value.
palegreen :: NamedColor
palegreen = NamedColor "palegreen"
{-# INLINE palegreen #-}


-- | Generates the CSS @paleturquoise@ @\<named-color\>@ value.
paleturquoise :: NamedColor
paleturquoise = NamedColor "paleturquoise"
{-# INLINE paleturquoise #-}


-- | Generates the CSS @palevioletred@ @\<named-color\>@ value.
palevioletred :: NamedColor
palevioletred = NamedColor "palevioletred"
{-# INLINE palevioletred #-}


-- | Generates the CSS @papayawhip@ @\<named-color\>@ value.
papayawhip :: NamedColor
papayawhip = NamedColor "papayawhip"
{-# INLINE papayawhip #-}


-- | Generates the CSS @peachpuff@ @\<named-color\>@ value.
peachpuff :: NamedColor
peachpuff = NamedColor "peachpuff"
{-# INLINE peachpuff #-}


-- | Generates the CSS @peru@ @\<named-color\>@ value.
peru :: NamedColor
peru = NamedColor "peru"
{-# INLINE peru #-}


-- | Generates the CSS @pink@ @\<named-color\>@ value.
pink :: NamedColor
pink = NamedColor "pink"
{-# INLINE pink #-}


-- | Generates the CSS @plum@ @\<named-color\>@ value.
plum :: NamedColor
plum = NamedColor "plum"
{-# INLINE plum #-}


-- | Generates the CSS @powderblue@ @\<named-color\>@ value.
powderblue :: NamedColor
powderblue = NamedColor "powderblue"
{-# INLINE powderblue #-}


-- | Generates the CSS @purple@ @\<named-color\>@ value.
purple :: NamedColor
purple = NamedColor "purple"
{-# INLINE purple #-}


-- | Generates the CSS @rebeccapurple@ @\<named-color\>@ value.
rebeccapurple :: NamedColor
rebeccapurple = NamedColor "rebeccapurple"
{-# INLINE rebeccapurple #-}


-- | Generates the CSS @red@ @\<named-color\>@ value.
red :: NamedColor
red = NamedColor "red"
{-# INLINE red #-}


-- | Generates the CSS @rosybrown@ @\<named-color\>@ value.
rosybrown :: NamedColor
rosybrown = NamedColor "rosybrown"
{-# INLINE rosybrown #-}


-- | Generates the CSS @royalblue@ @\<named-color\>@ value.
royalblue :: NamedColor
royalblue = NamedColor "royalblue"
{-# INLINE royalblue #-}


-- | Generates the CSS @saddlebrown@ @\<named-color\>@ value.
saddlebrown :: NamedColor
saddlebrown = NamedColor "saddlebrown"
{-# INLINE saddlebrown #-}


-- | Generates the CSS @salmon@ @\<named-color\>@ value.
salmon :: NamedColor
salmon = NamedColor "salmon"
{-# INLINE salmon #-}


-- | Generates the CSS @sandybrown@ @\<named-color\>@ value.
sandybrown :: NamedColor
sandybrown = NamedColor "sandybrown"
{-# INLINE sandybrown #-}


-- | Generates the CSS @seagreen@ @\<named-color\>@ value.
seagreen :: NamedColor
seagreen = NamedColor "seagreen"
{-# INLINE seagreen #-}


-- | Generates the CSS @seashell@ @\<named-color\>@ value.
seashell :: NamedColor
seashell = NamedColor "seashell"
{-# INLINE seashell #-}


-- | Generates the CSS @sienna@ @\<named-color\>@ value.
sienna :: NamedColor
sienna = NamedColor "sienna"
{-# INLINE sienna #-}


-- | Generates the CSS @silver@ @\<named-color\>@ value.
silver :: NamedColor
silver = NamedColor "silver"
{-# INLINE silver #-}


-- | Generates the CSS @skyblue@ @\<named-color\>@ value.
skyblue :: NamedColor
skyblue = NamedColor "skyblue"
{-# INLINE skyblue #-}


-- | Generates the CSS @slateblue@ @\<named-color\>@ value.
slateblue :: NamedColor
slateblue = NamedColor "slateblue"
{-# INLINE slateblue #-}


-- | Generates the CSS @slategray@ @\<named-color\>@ value.
slategray :: NamedColor
slategray = NamedColor "slategray"
{-# INLINE slategray #-}


-- | Generates the CSS @slategrey@ @\<named-color\>@ value.
slategrey :: NamedColor
slategrey = NamedColor "slategrey"
{-# INLINE slategrey #-}


-- | Generates the CSS @snow@ @\<named-color\>@ value.
snow :: NamedColor
snow = NamedColor "snow"
{-# INLINE snow #-}


-- | Generates the CSS @springgreen@ @\<named-color\>@ value.
springgreen :: NamedColor
springgreen = NamedColor "springgreen"
{-# INLINE springgreen #-}


-- | Generates the CSS @steelblue@ @\<named-color\>@ value.
steelblue :: NamedColor
steelblue = NamedColor "steelblue"
{-# INLINE steelblue #-}


-- | Generates the CSS @tan@ @\<named-color\>@ value.
tan :: NamedColor
tan = NamedColor "tan"
{-# INLINE tan #-}


-- | Generates the CSS @teal@ @\<named-color\>@ value.
teal :: NamedColor
teal = NamedColor "teal"
{-# INLINE teal #-}


-- | Generates the CSS @thistle@ @\<named-color\>@ value.
thistle :: NamedColor
thistle = NamedColor "thistle"
{-# INLINE thistle #-}


-- | Generates the CSS @tomato@ @\<named-color\>@ value.
tomato :: NamedColor
tomato = NamedColor "tomato"
{-# INLINE tomato #-}


-- | Generates the CSS @turquoise@ @\<named-color\>@ value.
turquoise :: NamedColor
turquoise = NamedColor "turquoise"
{-# INLINE turquoise #-}


-- | Generates the CSS @violet@ @\<named-color\>@ value.
violet :: NamedColor
violet = NamedColor "violet"
{-# INLINE violet #-}


-- | Generates the CSS @wheat@ @\<named-color\>@ value.
wheat :: NamedColor
wheat = NamedColor "wheat"
{-# INLINE wheat #-}


-- | Generates the CSS @white@ @\<named-color\>@ value.
white :: NamedColor
white = NamedColor "white"
{-# INLINE white #-}


-- | Generates the CSS @whitesmoke@ @\<named-color\>@ value.
whitesmoke :: NamedColor
whitesmoke = NamedColor "whitesmoke"
{-# INLINE whitesmoke #-}


-- | Generates the CSS @yellow@ @\<named-color\>@ value.
yellow :: NamedColor
yellow = NamedColor "yellow"
{-# INLINE yellow #-}


-- | Generates the CSS @yellowgreen@ @\<named-color\>@ value.
yellowgreen :: NamedColor
yellowgreen = NamedColor "yellowgreen"
{-# INLINE yellowgreen #-}


-- * PREDEFINED RGB


-- | Represents the CSS @\<predefined-rgb\>@ data type.
newtype PredefinedRgb = PredefinedRgb Builder
    deriving (Buildable, Show)


-- | Generates the CSS @a98-rgb@ @\<predefined-rgb\>@ value.
a98Rgb :: PredefinedRgb
a98Rgb = PredefinedRgb "a98-rgb"
{-# INLINE a98Rgb #-}


-- | Generates the CSS @display-p3@ @\<predefined-rgb\>@ value.
displayP3 :: PredefinedRgb
displayP3 = PredefinedRgb "display-p3"
{-# INLINE displayP3 #-}


-- | Generates the CSS @prophoto-rgb@ @\<predefined-rgb\>@ value.
prophotoRgb :: PredefinedRgb
prophotoRgb = PredefinedRgb "prophoto-rgb"
{-# INLINE prophotoRgb #-}


-- | Generates the CSS @rec2020@ @\<predefined-rgb\>@ value.
rec2020 :: PredefinedRgb
rec2020 = PredefinedRgb "rec2020"
{-# INLINE rec2020 #-}


-- | Generates the CSS @srgb@ @\<predefined-rgb\>@ value.
srgb :: PredefinedRgb
srgb = PredefinedRgb "srgb"
{-# INLINE srgb #-}


-- | Generates the CSS @srgb-linear@ @\<predefined-rgb\>@ value.
srgbLinear :: PredefinedRgb
srgbLinear = PredefinedRgb "srgb-linear"
{-# INLINE srgbLinear #-}


-- * SYSTEM COLORS


-- | Represents the CSS @\<system-color\>@ data type.
newtype SystemColor = SystemColor Builder
    deriving (Buildable, Show)


-- | Generates the CSS @AccentColor@ @\<system-color\>@ value.
accentColor :: SystemColor
accentColor = SystemColor "AccentColor"
{-# INLINE accentColor #-}


-- | Generates the CSS @AccentColorText@ @\<system-color\>@ value.
accentColorText :: SystemColor
accentColorText = SystemColor "AccentColorText"
{-# INLINE accentColorText #-}


-- | Generates the CSS @ActiveText@ @\<system-color\>@ value.
activeText :: SystemColor
activeText = SystemColor "ActiveText"
{-# INLINE activeText #-}


-- | Generates the CSS @ButtonBorder@ @\<system-color\>@ value.
buttonBorder :: SystemColor
buttonBorder = SystemColor "ButtonBorder"
{-# INLINE buttonBorder #-}


-- | Generates the CSS @ButtonFace@ @\<system-color\>@ value.
buttonFace :: SystemColor
buttonFace = SystemColor "ButtonFace"
{-# INLINE buttonFace #-}


-- | Generates the CSS @ButtonText@ @\<system-color\>@ value.
buttonText :: SystemColor
buttonText = SystemColor "ButtonText"
{-# INLINE buttonText #-}


-- | Generates the CSS @Canvas@ @\<system-color\>@ value.
canvas :: SystemColor
canvas = SystemColor "Canvas"
{-# INLINE canvas #-}


-- | Generates the CSS @CanvasText@ @\<system-color\>@ value.
canvasText :: SystemColor
canvasText = SystemColor "CanvasText"
{-# INLINE canvasText #-}


-- | Generates the CSS @Field@ @\<system-color\>@ value.
field :: SystemColor
field = SystemColor "Field"
{-# INLINE field #-}


-- | Generates the CSS @FieldText@ @\<system-color\>@ value.
fieldText :: SystemColor
fieldText = SystemColor "FieldText"
{-# INLINE fieldText #-}


-- | Generates the CSS @GrayText@ @\<system-color\>@ value.
grayText :: SystemColor
grayText = SystemColor "GrayText"
{-# INLINE grayText #-}


-- | Generates the CSS @Highlight@ @\<system-color\>@ value.
highlight :: SystemColor
highlight = SystemColor "Highlight"
{-# INLINE highlight #-}


-- | Generates the CSS @HighlightText@ @\<system-color\>@ value.
highlightText :: SystemColor
highlightText = SystemColor "HighlightText"
{-# INLINE highlightText #-}


-- | Generates the CSS @LinkText@ @\<system-color\>@ value.
linkText :: SystemColor
linkText = SystemColor "LinkText"
{-# INLINE linkText #-}


-- | Generates the CSS @Mark@ @\<system-color\>@ value.
mark :: SystemColor
mark = SystemColor "Mark"
{-# INLINE mark #-}


-- | Generates the CSS @MarkText@ @\<system-color\>@ value.
markText :: SystemColor
markText = SystemColor "MarkText"
{-# INLINE markText #-}


-- | Generates the CSS @SelectedItem@ @\<system-color\>@ value.
selectedItem :: SystemColor
selectedItem = SystemColor "SelectedItem"
{-# INLINE selectedItem #-}


-- | Generates the CSS @SelectedItemText@ @\<system-color\>@ value.
selectedItemText :: SystemColor
selectedItemText = SystemColor "SelectedItemText"
{-# INLINE selectedItemText #-}


-- | Generates the CSS @VisitedText@ @\<system-color\>@ value.
visitedText :: SystemColor
visitedText = SystemColor "VisitedText"
{-# INLINE visitedText #-}


-- * XYZ SPACE


-- | Represents the CSS @\<xyz-space\>@ data type.
newtype XyzSpace = XyzSpace Builder
    deriving (Buildable, Show)


-- | Generates the CSS @xyz@ @\<xyz-space\>@ value.
xyz :: XyzSpace
xyz = XyzSpace "xyz"
{-# INLINE xyz #-}


-- | Generates the CSS @xyz-d50@ @\<xyz-space\>@ value.
xyzD50 :: XyzSpace
xyzD50 = XyzSpace "xyz-d50"
{-# INLINE xyzD50 #-}


-- | Generates the CSS @xyz-d65@ @\<xyz-space\> value.
xyzD65 :: XyzSpace
xyzD65 = XyzSpace "xyz-d65"
{-# INLINE xyzD65 #-}
