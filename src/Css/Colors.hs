{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Colors
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Colors" module provides a set of functions for CSS colors.
module Css.Colors
    ( -- * Colors
      -- ** aliceblue
      aliceblue
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
      -- ** transparent
    , transparent
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
    ) where


import Prelude hiding (tan)

import Data.Text.Lazy.Builder (Builder)


-- | Generates the CSS color @aliceblue@.
aliceblue :: Builder
aliceblue = "aliceblue"
{-# INLINE aliceblue #-}


-- | Generates the CSS color @antiquewhite@.
antiquewhite :: Builder
antiquewhite = "antiquewhite"
{-# INLINE antiquewhite #-}


-- | Generates the CSS color @aqua@.
aqua :: Builder
aqua = "aqua"
{-# INLINE aqua #-}


-- | Generates the CSS color @aquamarine@.
aquamarine :: Builder
aquamarine = "aquamarine"
{-# INLINE aquamarine #-}


-- | Generates the CSS color @azure@.
azure :: Builder
azure = "azure"
{-# INLINE azure #-}


-- | Generates the CSS color @beige@.
beige :: Builder
beige = "beige"
{-# INLINE beige #-}


-- | Generates the CSS color @bisque@.
bisque :: Builder
bisque = "bisque"
{-# INLINE bisque #-}


-- | Generates the CSS color @black@.
black :: Builder
black = "black"
{-# INLINE black #-}


-- | Generates the CSS color @blanchedalmond@.
blanchedalmond :: Builder
blanchedalmond = "blanchedalmond"
{-# INLINE blanchedalmond #-}


-- | Generates the CSS color @blue@.
blue :: Builder
blue = "blue"
{-# INLINE blue #-}


-- | Generates the CSS color @blueviolet@.
blueviolet :: Builder
blueviolet = "blueviolet"
{-# INLINE blueviolet #-}


-- | Generates the CSS color @brown@.
brown :: Builder
brown = "brown"
{-# INLINE brown #-}


-- | Generates the CSS color @burlywood@.
burlywood :: Builder
burlywood = "burlywood"
{-# INLINE burlywood #-}


-- | Generates the CSS color @cadetblue@.
cadetblue :: Builder
cadetblue = "cadetblue"
{-# INLINE cadetblue #-}


-- | Generates the CSS color @chartreuse@.
chartreuse :: Builder
chartreuse = "chartreuse"
{-# INLINE chartreuse #-}


-- | Generates the CSS color @chocolate@.
chocolate :: Builder
chocolate = "chocolate"
{-# INLINE chocolate #-}


-- | Generates the CSS color @coral@.
coral :: Builder
coral = "coral"
{-# INLINE coral #-}


-- | Generates the CSS color @cornflowerblue@.
cornflowerblue :: Builder
cornflowerblue = "cornflowerblue"
{-# INLINE cornflowerblue #-}


-- | Generates the CSS color @cornsilk@.
cornsilk :: Builder
cornsilk = "cornsilk"
{-# INLINE cornsilk #-}


-- | Generates the CSS color @crimson@.
crimson :: Builder
crimson = "crimson"
{-# INLINE crimson #-}


-- | Generates the CSS color @cyan@.
cyan :: Builder
cyan = "cyan"
{-# INLINE cyan #-}


-- | Generates the CSS color @darkblue@.
darkblue :: Builder
darkblue = "darkblue"
{-# INLINE darkblue #-}


-- | Generates the CSS color @darkcyan@.
darkcyan :: Builder
darkcyan = "darkcyan"
{-# INLINE darkcyan #-}


-- | Generates the CSS color @darkgoldenrod@.
darkgoldenrod :: Builder
darkgoldenrod = "darkgoldenrod"
{-# INLINE darkgoldenrod #-}


-- | Generates the CSS color @darkgray@.
darkgray :: Builder
darkgray = "darkgray"
{-# INLINE darkgray #-}


-- | Generates the CSS color @darkgreen@.
darkgreen :: Builder
darkgreen = "darkgreen"
{-# INLINE darkgreen #-}


-- | Generates the CSS color @darkgrey@.
darkgrey :: Builder
darkgrey = "darkgrey"
{-# INLINE darkgrey #-}


-- | Generates the CSS color @darkkhaki@.
darkkhaki :: Builder
darkkhaki = "darkkhaki"
{-# INLINE darkkhaki #-}


-- | Generates the CSS color @darkmagenta@.
darkmagenta :: Builder
darkmagenta = "darkmagenta"
{-# INLINE darkmagenta #-}


-- | Generates the CSS color @darkolivegreen@.
darkolivegreen :: Builder
darkolivegreen = "darkolivegreen"
{-# INLINE darkolivegreen #-}


-- | Generates the CSS color @darkorange@.
darkorange :: Builder
darkorange = "darkorange"
{-# INLINE darkorange #-}


-- | Generates the CSS color @darkorchid@.
darkorchid :: Builder
darkorchid = "darkorchid"
{-# INLINE darkorchid #-}


-- | Generates the CSS color @darkred@.
darkred :: Builder
darkred = "darkred"
{-# INLINE darkred #-}


-- | Generates the CSS color @darksalmon@.
darksalmon :: Builder
darksalmon = "darksalmon"
{-# INLINE darksalmon #-}


-- | Generates the CSS color @darkseagreen@.
darkseagreen :: Builder
darkseagreen = "darkseagreen"
{-# INLINE darkseagreen #-}


-- | Generates the CSS color @darkslateblue@.
darkslateblue :: Builder
darkslateblue = "darkslateblue"
{-# INLINE darkslateblue #-}


-- | Generates the CSS color @darkslategray@.
darkslategray :: Builder
darkslategray = "darkslategray"
{-# INLINE darkslategray #-}


-- | Generates the CSS color @darkslategrey@.
darkslategrey :: Builder
darkslategrey = "darkslategrey"
{-# INLINE darkslategrey #-}


-- | Generates the CSS color @darkturquoise@.
darkturquoise :: Builder
darkturquoise = "darkturquoise"
{-# INLINE darkturquoise #-}


-- | Generates the CSS color @darkviolet@.
darkviolet :: Builder
darkviolet = "darkviolet"
{-# INLINE darkviolet #-}


-- | Generates the CSS color @deeppink@.
deeppink :: Builder
deeppink = "deeppink"
{-# INLINE deeppink #-}


-- | Generates the CSS color @deepskyblue@.
deepskyblue :: Builder
deepskyblue = "deepskyblue"
{-# INLINE deepskyblue #-}


-- | Generates the CSS color @dimgray@.
dimgray :: Builder
dimgray = "dimgray"
{-# INLINE dimgray #-}


-- | Generates the CSS color @dimgrey@.
dimgrey :: Builder
dimgrey = "dimgrey"
{-# INLINE dimgrey #-}


-- | Generates the CSS color @dodgerblue@.
dodgerblue :: Builder
dodgerblue = "dodgerblue"
{-# INLINE dodgerblue #-}


-- | Generates the CSS color @firebrick@.
firebrick :: Builder
firebrick = "firebrick"
{-# INLINE firebrick #-}


-- | Generates the CSS color @floralwhite@.
floralwhite :: Builder
floralwhite = "floralwhite"
{-# INLINE floralwhite #-}


-- | Generates the CSS color @forestgreen@.
forestgreen :: Builder
forestgreen = "forestgreen"
{-# INLINE forestgreen #-}


-- | Generates the CSS color @fuchsia@.
fuchsia :: Builder
fuchsia = "fuchsia"
{-# INLINE fuchsia #-}


-- | Generates the CSS color @gainsboro@.
gainsboro :: Builder
gainsboro = "gainsboro"
{-# INLINE gainsboro #-}


-- | Generates the CSS color @ghostwhite@.
ghostwhite :: Builder
ghostwhite = "ghostwhite"
{-# INLINE ghostwhite #-}


-- | Generates the CSS color @gold@.
gold :: Builder
gold = "gold"
{-# INLINE gold #-}


-- | Generates the CSS color @goldenrod@.
goldenrod :: Builder
goldenrod = "goldenrod"
{-# INLINE goldenrod #-}


-- | Generates the CSS color @gray@.
gray :: Builder
gray = "gray"
{-# INLINE gray #-}


-- | Generates the CSS color @green@.
green :: Builder
green = "green"
{-# INLINE green #-}


-- | Generates the CSS color @greenyellow@.
greenyellow :: Builder
greenyellow = "greenyellow"
{-# INLINE greenyellow #-}


-- | Generates the CSS color @grey@.
grey :: Builder
grey = "grey"
{-# INLINE grey #-}


-- | Generates the CSS color @honeydew@.
honeydew :: Builder
honeydew = "honeydew"
{-# INLINE honeydew #-}


-- | Generates the CSS color @hotpink@.
hotpink :: Builder
hotpink = "hotpink"
{-# INLINE hotpink #-}


-- | Generates the CSS color @indianred@.
indianred :: Builder
indianred = "indianred"
{-# INLINE indianred #-}


-- | Generates the CSS color @indigo@.
indigo :: Builder
indigo = "indigo"
{-# INLINE indigo #-}


-- | Generates the CSS color @ivory@.
ivory :: Builder
ivory = "ivory"
{-# INLINE ivory #-}


-- | Generates the CSS color @khaki@.
khaki :: Builder
khaki = "khaki"
{-# INLINE khaki #-}


-- | Generates the CSS color @lavender@.
lavender :: Builder
lavender = "lavender"
{-# INLINE lavender #-}


-- | Generates the CSS color @lavenderblush@.
lavenderblush :: Builder
lavenderblush = "lavenderblush"
{-# INLINE lavenderblush #-}


-- | Generates the CSS color @lawngreen@.
lawngreen :: Builder
lawngreen = "lawngreen"
{-# INLINE lawngreen #-}


-- | Generates the CSS color @lemonchiffon@.
lemonchiffon :: Builder
lemonchiffon = "lemonchiffon"
{-# INLINE lemonchiffon #-}


-- | Generates the CSS color @lightblue@.
lightblue :: Builder
lightblue = "lightblue"
{-# INLINE lightblue #-}


-- | Generates the CSS color @lightcoral@.
lightcoral :: Builder
lightcoral = "lightcoral"
{-# INLINE lightcoral #-}


-- | Generates the CSS color @lightcyan@.
lightcyan :: Builder
lightcyan = "lightcyan"
{-# INLINE lightcyan #-}


-- | Generates the CSS color @lightgoldenrodyellow@.
lightgoldenrodyellow :: Builder
lightgoldenrodyellow = "lightgoldenrodyellow"
{-# INLINE lightgoldenrodyellow #-}


-- | Generates the CSS color @lightgray@.
lightgray :: Builder
lightgray = "lightgray"
{-# INLINE lightgray #-}


-- | Generates the CSS color @lightgreen@.
lightgreen :: Builder
lightgreen = "lightgreen"
{-# INLINE lightgreen #-}


-- | Generates the CSS color @lightgrey@.
lightgrey :: Builder
lightgrey = "lightgrey"
{-# INLINE lightgrey #-}


-- | Generates the CSS color @lightpink@.
lightpink :: Builder
lightpink = "lightpink"
{-# INLINE lightpink #-}


-- | Generates the CSS color @lightsalmon@.
lightsalmon :: Builder
lightsalmon = "lightsalmon"
{-# INLINE lightsalmon #-}


-- | Generates the CSS color @lightseagreen@.
lightseagreen :: Builder
lightseagreen = "lightseagreen"
{-# INLINE lightseagreen #-}


-- | Generates the CSS color @lightskyblue@.
lightskyblue :: Builder
lightskyblue = "lightskyblue"
{-# INLINE lightskyblue #-}


-- | Generates the CSS color @lightslategray@.
lightslategray :: Builder
lightslategray = "lightslategray"
{-# INLINE lightslategray #-}


-- | Generates the CSS color @lightslategrey@.
lightslategrey :: Builder
lightslategrey = "lightslategrey"
{-# INLINE lightslategrey #-}


-- | Generates the CSS color @lightsteelblue@.
lightsteelblue :: Builder
lightsteelblue = "lightsteelblue"
{-# INLINE lightsteelblue #-}


-- | Generates the CSS color @lightyellow@.
lightyellow :: Builder
lightyellow = "lightyellow"
{-# INLINE lightyellow #-}


-- | Generates the CSS color @lime@.
lime :: Builder
lime = "lime"
{-# INLINE lime #-}


-- | Generates the CSS color @limegreen@.
limegreen :: Builder
limegreen = "limegreen"
{-# INLINE limegreen #-}


-- | Generates the CSS color @linen@.
linen :: Builder
linen = "linen"
{-# INLINE linen #-}


-- | Generates the CSS color @magenta@.
magenta :: Builder
magenta = "magenta"
{-# INLINE magenta #-}


-- | Generates the CSS color @maroon@.
maroon :: Builder
maroon = "maroon"
{-# INLINE maroon #-}


-- | Generates the CSS color @mediumaquamarine@.
mediumaquamarine :: Builder
mediumaquamarine = "mediumaquamarine"
{-# INLINE mediumaquamarine #-}


-- | Generates the CSS color @mediumblue@.
mediumblue :: Builder
mediumblue = "mediumblue"
{-# INLINE mediumblue #-}


-- | Generates the CSS color @mediumorchid@.
mediumorchid :: Builder
mediumorchid = "mediumorchid"
{-# INLINE mediumorchid #-}


-- | Generates the CSS color @mediumpurple@.
mediumpurple :: Builder
mediumpurple = "mediumpurple"
{-# INLINE mediumpurple #-}


-- | Generates the CSS color @mediumseagreen@.
mediumseagreen :: Builder
mediumseagreen = "mediumseagreen"
{-# INLINE mediumseagreen #-}


-- | Generates the CSS color @mediumslateblue@.
mediumslateblue :: Builder
mediumslateblue = "mediumslateblue"
{-# INLINE mediumslateblue #-}


-- | Generates the CSS color @mediumspringgreen@.
mediumspringgreen :: Builder
mediumspringgreen = "mediumspringgreen"
{-# INLINE mediumspringgreen #-}


-- | Generates the CSS color @mediumturquoise@.
mediumturquoise :: Builder
mediumturquoise = "mediumturquoise"
{-# INLINE mediumturquoise #-}


-- | Generates the CSS color @mediumvioletred@.
mediumvioletred :: Builder
mediumvioletred = "mediumvioletred"
{-# INLINE mediumvioletred #-}


-- | Generates the CSS color @midnightblue@.
midnightblue :: Builder
midnightblue = "midnightblue"
{-# INLINE midnightblue #-}


-- | Generates the CSS color @mintcream@.
mintcream :: Builder
mintcream = "mintcream"
{-# INLINE mintcream #-}


-- | Generates the CSS color @mistyrose@.
mistyrose :: Builder
mistyrose = "mistyrose"
{-# INLINE mistyrose #-}


-- | Generates the CSS color @moccasin@.
moccasin :: Builder
moccasin = "moccasin"
{-# INLINE moccasin #-}


-- | Generates the CSS color @navajowhite@.
navajowhite :: Builder
navajowhite = "navajowhite"
{-# INLINE navajowhite #-}


-- | Generates the CSS color @navy@.
navy :: Builder
navy = "navy"
{-# INLINE navy #-}


-- | Generates the CSS color @oldlace@.
oldlace :: Builder
oldlace = "oldlace"
{-# INLINE oldlace #-}


-- | Generates the CSS color @olive@.
olive :: Builder
olive = "olive"
{-# INLINE olive #-}


-- | Generates the CSS color @olivedrab@.
olivedrab :: Builder
olivedrab = "olivedrab"
{-# INLINE olivedrab #-}


-- | Generates the CSS color @orange@.
orange :: Builder
orange = "orange"
{-# INLINE orange #-}


-- | Generates the CSS color @orangered@.
orangered :: Builder
orangered = "orangered"
{-# INLINE orangered #-}


-- | Generates the CSS color @orchid@.
orchid :: Builder
orchid = "orchid"
{-# INLINE orchid #-}


-- | Generates the CSS color @palegoldenrod@.
palegoldenrod :: Builder
palegoldenrod = "palegoldenrod"
{-# INLINE palegoldenrod #-}


-- | Generates the CSS color @palegreen@.
palegreen :: Builder
palegreen = "palegreen"
{-# INLINE palegreen #-}


-- | Generates the CSS color @paleturquoise@.
paleturquoise :: Builder
paleturquoise = "paleturquoise"
{-# INLINE paleturquoise #-}


-- | Generates the CSS color @palevioletred@.
palevioletred :: Builder
palevioletred = "palevioletred"
{-# INLINE palevioletred #-}


-- | Generates the CSS color @papayawhip@.
papayawhip :: Builder
papayawhip = "papayawhip"
{-# INLINE papayawhip #-}


-- | Generates the CSS color @peachpuff@.
peachpuff :: Builder
peachpuff = "peachpuff"
{-# INLINE peachpuff #-}


-- | Generates the CSS color @peru@.
peru :: Builder
peru = "peru"
{-# INLINE peru #-}


-- | Generates the CSS color @pink@.
pink :: Builder
pink = "pink"
{-# INLINE pink #-}


-- | Generates the CSS color @plum@.
plum :: Builder
plum = "plum"
{-# INLINE plum #-}


-- | Generates the CSS color @powderblue@.
powderblue :: Builder
powderblue = "powderblue"
{-# INLINE powderblue #-}


-- | Generates the CSS color @purple@.
purple :: Builder
purple = "purple"
{-# INLINE purple #-}


-- | Generates the CSS color @rebeccapurple@.
rebeccapurple :: Builder
rebeccapurple = "rebeccapurple"
{-# INLINE rebeccapurple #-}


-- | Generates the CSS color @red@.
red :: Builder
red = "red"
{-# INLINE red #-}


-- | Generates the CSS color @rosybrown@.
rosybrown :: Builder
rosybrown = "rosybrown"
{-# INLINE rosybrown #-}


-- | Generates the CSS color @royalblue@.
royalblue :: Builder
royalblue = "royalblue"
{-# INLINE royalblue #-}


-- | Generates the CSS color @saddlebrown@.
saddlebrown :: Builder
saddlebrown = "saddlebrown"
{-# INLINE saddlebrown #-}


-- | Generates the CSS color @salmon@.
salmon :: Builder
salmon = "salmon"
{-# INLINE salmon #-}


-- | Generates the CSS color @sandybrown@.
sandybrown :: Builder
sandybrown = "sandybrown"
{-# INLINE sandybrown #-}


-- | Generates the CSS color @seagreen@.
seagreen :: Builder
seagreen = "seagreen"
{-# INLINE seagreen #-}


-- | Generates the CSS color @seashell@.
seashell :: Builder
seashell = "seashell"
{-# INLINE seashell #-}


-- | Generates the CSS color @sienna@.
sienna :: Builder
sienna = "sienna"
{-# INLINE sienna #-}


-- | Generates the CSS color @silver@.
silver :: Builder
silver = "silver"
{-# INLINE silver #-}


-- | Generates the CSS color @skyblue@.
skyblue :: Builder
skyblue = "skyblue"
{-# INLINE skyblue #-}


-- | Generates the CSS color @slateblue@.
slateblue :: Builder
slateblue = "slateblue"
{-# INLINE slateblue #-}


-- | Generates the CSS color @slategray@.
slategray :: Builder
slategray = "slategray"
{-# INLINE slategray #-}


-- | Generates the CSS color @slategrey@.
slategrey :: Builder
slategrey = "slategrey"
{-# INLINE slategrey #-}


-- | Generates the CSS color @snow@.
snow :: Builder
snow = "snow"
{-# INLINE snow #-}


-- | Generates the CSS color @springgreen@.
springgreen :: Builder
springgreen = "springgreen"
{-# INLINE springgreen #-}


-- | Generates the CSS color @steelblue@.
steelblue :: Builder
steelblue = "steelblue"
{-# INLINE steelblue #-}


-- | Generates the CSS color @tan@.
tan :: Builder
tan = "tan"
{-# INLINE tan #-}


-- | Generates the CSS color @teal@.
teal :: Builder
teal = "teal"
{-# INLINE teal #-}


-- | Generates the CSS color @thistle@.
thistle :: Builder
thistle = "thistle"
{-# INLINE thistle #-}


-- | Generates the CSS color @tomato@.
tomato :: Builder
tomato = "tomato"
{-# INLINE tomato #-}


-- | Generates the CSS color @transparent@.
transparent :: Builder
transparent = "transparent"
{-# INLINE transparent #-}


-- | Generates the CSS color @turquoise@.
turquoise :: Builder
turquoise = "turquoise"
{-# INLINE turquoise #-}


-- | Generates the CSS color @violet@.
violet :: Builder
violet = "violet"
{-# INLINE violet #-}


-- | Generates the CSS color @wheat@.
wheat :: Builder
wheat = "wheat"
{-# INLINE wheat #-}


-- | Generates the CSS color @white@.
white :: Builder
white = "white"
{-# INLINE white #-}


-- | Generates the CSS color @whitesmoke@.
whitesmoke :: Builder
whitesmoke = "whitesmoke"
{-# INLINE whitesmoke #-}


-- | Generates the CSS color @yellow@.
yellow :: Builder
yellow = "yellow"
{-# INLINE yellow #-}

-- | Generates the CSS color @yellowgreen@.
yellowgreen :: Builder
yellowgreen = "yellowgreen"
{-# INLINE yellowgreen #-}
