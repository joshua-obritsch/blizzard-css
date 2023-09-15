{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Css
    ( auto
    , inherit
    , initial
    , accentColor
    , alignContent
    ) where


import Prelude

import Css.Internal (buildWithMap, extract)
import Data.Foldable (fold)
import Data.Text.Lazy.Builder (Builder, singleton)
import Html (Attribute(..), Html(..), Translatable(..))


css :: [Builder] -> Attribute
css = TextAttribute " css=\"" . fold


build :: Html lng -> Builder
build html = buildWithMap map html'
  where
    (map, html') = extract html


-- Values


alternate :: Builder
alternate = "alternate"


alternateReverse :: Builder
alternateReverse = "alternate-reverse"


auto :: Builder
auto = "auto"


backwards :: Builder
backwards = "backwards"


baseline :: Builder
baseline = "baseline"


both :: Builder
both = "both"


center :: Builder
center = "center"


end :: Builder
end = "end"


flexEnd :: Builder
flexEnd = "flex-end"


flexStart :: Builder
flexStart = "flex-start"


forwards :: Builder
forwards = "forwards"


infinite :: Builder
infinite = "infinite"


inherit :: Builder
inherit = "inherit"


initial :: Builder
initial = "initial"


none :: Builder
none = "none"


normal :: Builder
normal = "normal"


paused :: Builder
paused = "paused"


reverse :: Builder
reverse = "reverse"


running :: Builder
running = "running"


spaceAround :: Builder
spaceAround = "space-around"


spaceBetween :: Builder
spaceBetween = "space-between"


spaceEvenly :: Builder
spaceEvenly = "space-evenly"


start :: Builder
start = "start"


unset :: Builder
unset = "unset"


-- Properties


accentColor :: Builder -> Builder
accentColor = prop "accent-color:"


alignContent :: Builder -> Builder
alignContent = prop "align-content:"


alignItems :: Builder -> Builder
alignItems = prop "align-items:"


alignSelf :: Builder -> Builder
alignSelf = prop "align-self:"


all :: Builder -> Builder
all = prop "all:"


animation :: Builder -> Builder
animation = prop "animation:"


animationDelay :: Builder -> Builder
animationDelay = prop "animation-delay:"


animationDirection :: Builder -> Builder
animationDirection = prop "animation-direction:"


animationDuration :: Builder -> Builder
animationDuration = prop "animation-duration:"


animationFillMode :: Builder -> Builder
animationFillMode = prop "animation-fill-mode:"


animationIterationCount :: Builder -> Builder
animationIterationCount = prop "animation-iteration-count:"


animationName :: Builder -> Builder
animationName = prop "animation-name:"


animationPlayState :: Builder -> Builder
animationPlayState = prop "animation-play-state:"


animationTimingFunction :: Builder -> Builder
animationTimingFunction = prop "animation-timing-function:"


aspectRatio :: Builder -> Builder
aspectRatio = prop "aspect-ratio:"


backdropFilter :: Builder -> Builder
backdropFilter = prop "backdrop-filter:"


prop :: Builder -> Builder -> Builder
prop key value = key <> value <> singleton ';'
