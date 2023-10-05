{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module    : Css.Properties
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Properties" module provides a set of functions for generating CSS properties.
module Css.Properties
    ( -- * Properties
      -- ** accent-color
      accentColor
      -- ** align-content
    , alignContent
      -- ** align-items
    , alignItems
      -- ** align-self
    , alignSelf
      -- ** all
    , all
      -- ** animation
    , animation
      -- ** animation-composition
    , animationComposition
      -- ** animation-delay
    , animationDelay
      -- ** animation-direction
    , animationDirection
      -- ** animation-duration
    , animationDuration
      -- ** animation-fill-mode
    , animationFillMode
      -- ** animation-iteration-count
    , animationIterationCount
      -- ** animation-name
    , animationName
      -- ** animation-play-state
    , animationPlayState
      -- ** animation-timing-function
    , animationTimingFunction
      -- ** aspect-ratio
    , aspectRatio
      -- ** backdrop-filter
    , backdropFilter
      -- ** backface-visibility
    , backfaceVisibility
      -- ** background
    , background
      -- ** background-attachment
    , backgroundAttachment
      -- ** background-blend-mode
    , backgroundBlendMode
      -- ** background-clip
    , backgroundClip
      -- ** background-color
    , backgroundColor
      -- ** background-image
    , backgroundImage
      -- ** background-origin
    , backgroundOrigin
      -- ** background-position
    , backgroundPosition
      -- ** background-position-x
    , backgroundPositionX
      -- ** background-position-y
    , backgroundPositionY
      -- ** background-repeat
    , backgroundRepeat
      -- ** background-size
    , backgroundSize
      -- ** block-size
    , blockSize
      -- ** border
    , border
      -- ** border-block
    , borderBlock
      -- ** border-block-color
    , borderBlockColor
      -- ** border-block-end-color
    , borderBlockEndColor
      -- ** border-block-end-style
    , borderBlockEndStyle
      -- ** border-block-end-width
    , borderBlockEndWidth
      -- ** border-block-start-color
    , borderBlockStartColor
      -- ** border-block-start-style
    , borderBlockStartStyle
      -- ** border-block-start-width
    , borderBlockStartWidth
      -- ** border-block-style
    , borderBlockStyle
      -- ** border-block-width
    , borderBlockWidth
      -- ** border-bottom
    , borderBottom
      -- ** border-bottom-color
    , borderBottomColor
      -- ** border-bottom-left-radius
    , borderBottomLeftRadius
      -- ** border-bottom-right-radius
    , borderBottomRightRadius
      -- ** border-bottom-style
    , borderBottomStyle
      -- ** border-bottom-width
    , borderBottomWidth
      -- ** border-collapse
    , borderCollapse
      -- ** border-color
    , borderColor
      -- ** border-end-end-radius
    , borderEndEndRadius
      -- ** border-end-start-radius
    , borderEndStartRadius
      -- ** border-image
    , borderImage
      -- ** border-image-outset
    , borderImageOutset
      -- ** border-image-repeat
    , borderImageRepeat
      -- ** border-image-slice
    , borderImageSlice
      -- ** border-image-source
    , borderImageSource
      -- ** border-image-width
    , borderImageWidth
      -- ** border-inline
    , borderInline
      -- ** border-inline-color
    , borderInlineColor
      -- ** border-inline-end-color
    , borderInlineEndColor
      -- ** border-inline-end-style
    , borderInlineEndStyle
      -- ** border-inline-end-width
    , borderInlineEndWidth
      -- ** border-inline-start-color
    , borderInlineStartColor
      -- ** border-inline-start-style
    , borderInlineStartStyle
      -- ** border-inline-start-width
    , borderInlineStartWidth
      -- ** border-inline-style
    , borderInlineStyle
      -- ** border-inline-width
    , borderInlineWidth
      -- ** border-left
    , borderLeft
      -- ** border-left-color
    , borderLeftColor
      -- ** border-left-style
    , borderLeftStyle
      -- ** border-left-width
    , borderLeftWidth
      -- ** border-radius
    , borderRadius
      -- ** border-right
    , borderRight
      -- ** border-right-color
    , borderRightColor
      -- ** border-right-style
    , borderRightStyle
      -- ** border-right-width
    , borderRightWidth
      -- ** border-spacing
    , borderSpacing
      -- ** border-start-end-radius
    , borderStartEndRadius
      -- ** border-start-start-radius
    , borderStartStartRadius
      -- ** border-style
    , borderStyle
      -- ** border-top
    , borderTop
      -- ** border-top-color
    , borderTopColor
      -- ** border-top-left-radius
    , borderTopLeftRadius
      -- ** border-top-right-radius
    , borderTopRightRadius
      -- ** border-top-style
    , borderTopStyle
      -- ** border-top-width
    , borderTopWidth
      -- ** border-width
    , borderWidth
      -- ** bottom
    , bottom
      -- ** box-decoration-break
    , boxDecorationBreak
      -- ** box-reflect
    , boxReflect
      -- ** box-shadow
    , boxShadow
      -- ** box-sizing
    , boxSizing
      -- ** break-after
    , breakAfter
      -- ** break-before
    , breakBefore
      -- ** break-inside
    , breakInside
      -- ** caption-side
    , captionSide
      -- ** caret-color
    , caretColor
      -- ** clear
    , clear
      -- ** clip
    , clip
      -- ** clip-path
    , clipPath
      -- ** color
    , color
      -- ** column-count
    , columnCount
      -- ** column-fill
    , columnFill
      -- ** column-gap
    , columnGap
      -- ** column-rule
    , columnRule
      -- ** column-rule-color
    , columnRuleColor
      -- ** column-rule-style
    , columnRuleStyle
      -- ** column-rule-width
    , columnRuleWidth
      -- ** column-span
    , columnSpan
      -- ** column-width
    , columnWidth
      -- ** columns
    , columns
      -- ** content
    , content
      -- ** counter-increment
    , counterIncrement
      -- ** counter-reset
    , counterReset
      -- ** cursor
    , cursor
      -- ** direction
    , direction
      -- ** display
    , display
      -- ** empty-cells
    , emptyCells
      -- ** filter
    , filter
      -- ** flex
    , flex
      -- ** flex-basis
    , flexBasis
      -- ** flex-direction
    , flexDirection
      -- ** flex-flow
    , flexFlow
      -- ** flex-grow
    , flexGrow
      -- ** flex-shrink
    , flexShrink
      -- ** flex-wrap
    , flexWrap
      -- ** float
    , float
      -- ** font
    , font
      -- ** font-family
    , fontFamily
      -- ** font-feature-settings
    , fontFeatureSettings
      -- ** font-kerning
    , fontKerning
      -- ** font-size
    , fontSize
      -- ** font-size-adjust
    , fontSizeAdjust
      -- ** font-stretch
    , fontStretch
      -- ** font-style
    , fontStyle
      -- ** font-variant
    , fontVariant
      -- ** font-variant-caps
    , fontVariantCaps
      -- ** font-weight
    , fontWeight
      -- ** gap
    , gap
      -- ** grid
    , grid
      -- ** grid-area
    , gridArea
      -- ** grid-auto-columns
    , gridAutoColumns
      -- ** grid-auto-flow
    , gridAutoFlow
      -- ** grid-auto-rows
    , gridAutoRows
      -- ** grid-column
    , gridColumn
      -- ** grid-column-end
    , gridColumnEnd
      -- ** grid-column-gap
    , gridColumnGap
      -- ** grid-column-start
    , gridColumnStart
      -- ** grid-gap
    , gridGap
      -- ** grid-row
    , gridRow
      -- ** grid-row-end
    , gridRowEnd
      -- ** grid-row-gap
    , gridRowGap
      -- ** grid-row-start
    , gridRowStart
      -- ** grid-template
    , gridTemplate
      -- ** grid-template-areas
    , gridTemplateAreas
      -- ** grid-template-columns
    , gridTemplateColumns
      -- ** grid-template-rows
    , gridTemplateRows
      -- ** hanging-punctuation
    , hangingPunctuation
      -- ** height
    , height
      -- ** hyphens
    , hyphens
      -- ** image-rendering
    , imageRendering
      -- ** inline-size
    , inlineSize
      -- ** inset
    , inset
      -- ** inset-block
    , insetBlock
      -- ** inset-block-end
    , insetBlockEnd
      -- ** inset-block-start
    , insetBlockStart
      -- ** inset-inline
    , insetInline
      -- ** inset-inline-end
    , insetInlineEnd
      -- ** inset-inline-start
    , insetInlineStart
      -- ** isolation
    , isolation
      -- ** justify-content
    , justifyContent
      -- ** justify-items
    , justifyItems
      -- ** justify-self
    , justifySelf
      -- ** left
    , left
      -- ** letter-spacing
    , letterSpacing
      -- ** line-height
    , lineHeight
      -- ** list-style
    , listStyle
      -- ** list-style-image
    , listStyleImage
      -- ** list-style-position
    , listStylePosition
      -- ** list-style-type
    , listStyleType
      -- ** margin
    , margin
      -- ** margin-block
    , marginBlock
      -- ** margin-block-end
    , marginBlockEnd
      -- ** margin-block-start
    , marginBlockStart
      -- ** margin-bottom
    , marginBottom
      -- ** margin-inline
    , marginInline
      -- ** margin-inline-end
    , marginInlineEnd
      -- ** margin-inline-start
    , marginInlineStart
      -- ** margin-left
    , marginLeft
      -- ** margin-right
    , marginRight
      -- ** margin-top
    , marginTop
      -- ** mask-image
    , maskImage
      -- ** mask-mode
    , maskMode
      -- ** mask-origin
    , maskOrigin
      -- ** mask-position
    , maskPosition
      -- ** mask-repeat
    , maskRepeat
      -- ** mask-size
    , maskSize
      -- ** max-block-size
    , maxBlockSize
      -- ** max-height
    , maxHeight
      -- ** max-inline-size
    , maxInlineSize
      -- ** max-width
    , maxWidth
      -- ** min-block-size
    , minBlockSize
      -- ** min-inline-size
    , minInlineSize
      -- ** min-height
    , minHeight
      -- ** min-width
    , minWidth
      -- ** mix-blend-mode
    , mixBlendMode
      -- ** object-fit
    , objectFit
      -- ** object-position
    , objectPosition
      -- ** offset
    , offset
      -- ** offset-anchor
    , offsetAnchor
      -- ** offset-distance
    , offsetDistance
      -- ** offset-path
    , offsetPath
      -- ** offset-rotate
    , offsetRotate
      -- ** opacity
    , opacity
      -- ** order
    , order
      -- ** orphans
    , orphans
      -- ** outline
    , outline
      -- ** outline-color
    , outlineColor
      -- ** outline-offset
    , outlineOffset
      -- ** outline-style
    , outlineStyle
      -- ** outline-width
    , outlineWidth
      -- ** overflow
    , overflow
      -- ** overflow-anchor
    , overflowAnchor
      -- ** overflow-wrap
    , overflowWrap
      -- ** overflow-x
    , overflowX
      -- ** overflow-y
    , overflowY
      -- ** overscroll-behavior
    , overscrollBehavior
      -- ** overscroll-behavior-block
    , overscrollBehaviorBlock
      -- ** overscroll-behavior-inline
    , overscrollBehaviorInline
      -- ** overscroll-behavior-x
    , overscrollBehaviorX
      -- ** overscroll-behavior-y
    , overscrollBehaviorY
      -- ** padding
    , padding
      -- ** padding-block
    , paddingBlock
      -- ** padding-block-end
    , paddingBlockEnd
      -- ** padding-block-start
    , paddingBlockStart
      -- ** padding-bottom
    , paddingBottom
      -- ** padding-inline
    , paddingInline
      -- ** padding-inline-end
    , paddingInlineEnd
      -- ** padding-inline-start
    , paddingInlineStart
      -- ** padding-left
    , paddingLeft
      -- ** padding-right
    , paddingRight
      -- ** padding-top
    , paddingTop
      -- ** page-break-after
    , pageBreakAfter
      -- ** page-break-before
    , pageBreakBefore
      -- ** page-break-inside
    , pageBreakInside
      -- ** paint-order
    , paintOrder
      -- ** perspective
    , perspective
      -- ** perspective-origin
    , perspectiveOrigin
      -- ** place-content
    , placeContent
      -- ** place-items
    , placeItems
      -- ** place-self
    , placeSelf
      -- ** pointer-events
    , pointerEvents
      -- ** position
    , position
      -- ** quotes
    , quotes
      -- ** resize
    , resize
      -- ** right
    , right
      -- ** rotate
    , rotate
      -- ** row-gap
    , rowGap
      -- ** scale
    , scale
      -- ** scroll-behavior
    , scrollBehavior
      -- ** scroll-margin
    , scrollMargin
      -- ** scroll-margin-block
    , scrollMarginBlock
      -- ** scroll-margin-block-end
    , scrollMarginBlockEnd
      -- ** scroll-margin-block-start
    , scrollMarginBlockStart
      -- ** scroll-margin-bottom
    , scrollMarginBottom
      -- ** scroll-margin-inline
    , scrollMarginInline
      -- ** scroll-margin-inline-end
    , scrollMarginInlineEnd
      -- ** scroll-margin-inline-start
    , scrollMarginInlineStart
      -- ** scroll-margin-left
    , scrollMarginLeft
      -- ** scroll-margin-right
    , scrollMarginRight
      -- ** scroll-margin-top
    , scrollMarginTop
      -- ** scroll-padding
    , scrollPadding
      -- ** scroll-padding-block
    , scrollPaddingBlock
      -- ** scroll-padding-block-end
    , scrollPaddingBlockEnd
      -- ** scroll-padding-block-start
    , scrollPaddingBlockStart
      -- ** scroll-padding-bottom
    , scrollPaddingBottom
      -- ** scroll-padding-inline
    , scrollPaddingInline
      -- ** scroll-padding-inline-end
    , scrollPaddingInlineEnd
      -- ** scroll-padding-inline-start
    , scrollPaddingInlineStart
      -- ** scroll-padding-left
    , scrollPaddingLeft
      -- ** scroll-padding-right
    , scrollPaddingRight
      -- ** scroll-padding-top
    , scrollPaddingTop
      -- ** scroll-snap-align
    , scrollSnapAlign
      -- ** scroll-snap-stop
    , scrollSnapStop
      -- ** scroll-snap-type
    , scrollSnapType
      -- ** scrollbar-color
    , scrollbarColor
      -- ** tab-size
    , tabSize
      -- ** table-layout
    , tableLayout
      -- ** text-align
    , textAlign
      -- ** text-align-last
    , textAlignLast
      -- ** text-decoration
    , textDecoration
      -- ** text-decoration-color
    , textDecorationColor
      -- ** text-decoration-line
    , textDecorationLine
      -- ** text-decoration-style
    , textDecorationStyle
      -- ** text-decoration-thickness
    , textDecorationThickness
      -- ** text-indent
    , textIndent
      -- ** text-justify
    , textJustify
      -- ** text-orientation
    , textOrientation
      -- ** text-overflow
    , textOverflow
      -- ** text-shadow
    , textShadow
      -- ** text-transform
    , textTransform
      -- ** top
    , top
      -- ** transform
    , transform
      -- ** transform-origin
    , transformOrigin
      -- ** transform-style
    , transformStyle
      -- ** transition
    , transition
      -- ** transition-delay
    , transitionDelay
      -- ** transition-duration
    , transitionDuration
      -- ** transition-property
    , transitionProperty
      -- ** transition-timing-function
    , transitionTimingFunction
      -- ** translate
    , translate
      -- ** unicode-bidi
    , unicodeBidi
      -- ** user-select
    , userSelect
      -- ** vertical-align
    , verticalAlign
      -- ** visibility
    , visibility
      -- ** white-space
    , whiteSpace
      -- ** widows
    , widows
      -- ** width
    , width
      -- ** word-break
    , wordBreak
      -- ** word-spacing
    , wordSpacing
      -- ** word-wrap
    , wordWrap
      -- ** writing-mode
    , writingMode
      -- ** z-index
    , zIndex
    ) where


import Prelude hiding (String, all, filter)

import Css.DataTypes.Alignment
import Css.DataTypes.Animation
import Css.DataTypes.Color     hiding (accentColor, color)
import Css.DataTypes.Numeric
import Css.DataTypes.Textual

import Css.Keywords
import Data.Foldable           (fold)
import Data.List               (intersperse)
import Data.Text.Lazy.Builder  (Builder, singleton)
import Html                    (Buildable(..))


-- * PROPERTIES


-- | Represents the CSS @accent-color@ property.
class Buildable a => AccentColor a


instance {-# OVERLAPPING #-}       AccentColor Auto
instance (Buildable a, Color a) => AccentColor a

instance {-# OVERLAPPING #-}       AccentColor Inherit
instance {-# OVERLAPPING #-}       AccentColor Initial
instance {-# OVERLAPPING #-}       AccentColor Revert
instance {-# OVERLAPPING #-}       AccentColor Unset



-- | Generates a CSS @accent-color@ property with the given value.
accentColor :: AccentColor a => a -> Builder
accentColor = prop "accent-color:"
{-# INLINE accentColor #-}


-- | Represents the CSS @align-content@ property.
class Buildable a => AlignContent a


instance AlignContent BaselinePosition
instance AlignContent ContentDistribution
instance AlignContent ContentPosition
instance AlignContent Normal
instance AlignContent OverflowPosition
instance AlignContent Stretch

instance AlignContent Inherit
instance AlignContent Initial
instance AlignContent Revert
instance AlignContent Unset


-- | Generates a CSS @align-content@ property with the given value.
alignContent :: AlignContent a => a -> Builder
alignContent = prop "align-content:"
{-# INLINE alignContent #-}


-- | Represents the CSS @align-items@ property.
class Buildable a => AlignItems a


instance AlignItems BaselinePosition
instance AlignItems ContentPosition
instance AlignItems Normal
instance AlignItems OverflowPosition
instance AlignItems SafeSelfEnd
instance AlignItems SafeSelfStart
instance AlignItems SelfPosition
instance AlignItems Stretch
instance AlignItems UnsafeSelfEnd
instance AlignItems UnsafeSelfStart

instance AlignItems Inherit
instance AlignItems Initial
instance AlignItems Revert
instance AlignItems Unset


-- | Generates a CSS @align-items@ property with the given value.
alignItems :: AlignItems a => a -> Builder
alignItems = prop "align-items:"
{-# INLINE alignItems #-}


-- | Represents the CSS @align-self@ property.
class Buildable a => AlignSelf a


instance AlignSelf Auto
instance AlignSelf BaselinePosition
instance AlignSelf ContentPosition
instance AlignSelf Normal
instance AlignSelf OverflowPosition
instance AlignSelf SafeSelfEnd
instance AlignSelf SafeSelfStart
instance AlignSelf SelfPosition
instance AlignSelf Stretch
instance AlignSelf UnsafeSelfEnd
instance AlignSelf UnsafeSelfStart

instance AlignSelf Inherit
instance AlignSelf Initial
instance AlignSelf Revert
instance AlignSelf Unset


-- | Generates a CSS @align-self@ property with the given value.
alignSelf :: AlignSelf a => a -> Builder
alignSelf = prop "align-self:"
{-# INLINE alignSelf #-}


-- | Represents the CSS @all@ property.
class Buildable a => All a


instance All Inherit
instance All Initial
instance All Revert
instance All Unset


-- | Generates a CSS @all@ property with the given value.
all :: All a => a -> Builder
all = prop "all:"
{-# INLINE all #-}


-- TODO
-- | Generates a CSS @animation@ property with the given value.
animation :: [Builder] -> Builder
animation = props "animation:"
{-# INLINE animation #-}


-- | Represents the CSS @animation-composition@ property.
class Buildable a => AnimationComposition a


instance AnimationComposition SingleAnimationComposition

instance AnimationComposition Inherit
instance AnimationComposition Initial
instance AnimationComposition Revert
instance AnimationComposition Unset


-- | Generates a CSS @animation-composition@ property with the given value.
animationComposition :: AnimationComposition a => a -> Builder
animationComposition = prop "animation-composition:"
{-# INLINE animationComposition #-}


-- | Represents the CSS @animation-delay@ property.
class Buildable a => AnimationDelay a


instance AnimationDelay Time

instance AnimationDelay Inherit
instance AnimationDelay Initial
instance AnimationDelay Revert
instance AnimationDelay Unset


-- | Generates a CSS @animation-delay@ property with the given value.
animationDelay :: AnimationDelay a => a -> Builder
animationDelay = prop "animation-delay:"
{-# INLINE animationDelay #-}


-- | Represents the CSS @animation-direction@ property.
class Buildable a => AnimationDirection a


instance AnimationDirection Normal
instance AnimationDirection SingleAnimationDirection

instance AnimationDirection Inherit
instance AnimationDirection Initial
instance AnimationDirection Revert
instance AnimationDirection Unset


-- | Generates a CSS @animation-direction@ property with the given value.
animationDirection :: AnimationDirection a => a -> Builder
animationDirection = prop "animation-direction:"
{-# INLINE animationDirection #-}


-- | Represents the CSS @animation-duration@ property.
class Buildable a => AnimationDuration a


instance AnimationDuration Time

instance AnimationDuration Inherit
instance AnimationDuration Initial
instance AnimationDuration Revert
instance AnimationDuration Unset


-- | Generates a CSS @animation-duration@ property with the given value.
animationDuration :: AnimationDuration a => a -> Builder
animationDuration = prop "animation-duration:"
{-# INLINE animationDuration #-}


-- | Represents the CSS @animation-fill-mode@ property.
class Buildable a => AnimationFillMode a


instance AnimationFillMode None
instance AnimationFillMode SingleAnimationFillMode

instance AnimationFillMode Inherit
instance AnimationFillMode Initial
instance AnimationFillMode Revert
instance AnimationFillMode Unset


-- | Generates a CSS @animation-fill-mode@ property with the given value.
animationFillMode :: AnimationFillMode a => a -> Builder
animationFillMode = prop "animation-fill-mode:"
{-# INLINE animationFillMode #-}


-- | Represents the CSS @animation-iteration-count@ property.
class Buildable a => AnimationIterationCount a


instance AnimationIterationCount Number
instance AnimationIterationCount SingleAnimationIterationCount

instance AnimationIterationCount Inherit
instance AnimationIterationCount Initial
instance AnimationIterationCount Revert
instance AnimationIterationCount Unset


-- | Generates a CSS @animation-iteration-count@ property with the given value.
animationIterationCount :: AnimationIterationCount a => a -> Builder
animationIterationCount = prop "animation-iteration-count:"
{-# INLINE animationIterationCount #-}


-- | Represents the CSS @animation-name@ property.
class Buildable a => AnimationName a


instance AnimationName CustomIdent
instance AnimationName None
instance AnimationName String

instance AnimationName Inherit
instance AnimationName Initial
instance AnimationName Revert
instance AnimationName Unset


-- | Generates a CSS @animation-name@ property with the given value.
animationName :: AnimationName a => a -> Builder
animationName = prop "animation-name:"
{-# INLINE animationName #-}


-- | Represents the CSS @animation-play-state@ property.
class Buildable a => AnimationPlayState a


instance AnimationPlayState SingleAnimationPlayState

instance AnimationPlayState Inherit
instance AnimationPlayState Initial
instance AnimationPlayState Revert
instance AnimationPlayState Unset


-- | Generates a CSS @animation-play-state@ property with the given value.
animationPlayState :: AnimationPlayState a => a -> Builder
animationPlayState = prop "animation-play-state:"
{-# INLINE animationPlayState #-}


-- TODO
-- | Generates a CSS @animation-timing-function@ property with the given value.
animationTimingFunction :: Builder -> Builder
animationTimingFunction = prop "animation-timing-function:"
{-# INLINE animationTimingFunction #-}


-- | Represents the CSS @aspect-ratio@ property.
class Buildable a => AspectRatio a


instance AspectRatio Number
instance AspectRatio Ratio

instance AspectRatio Inherit
instance AspectRatio Initial
instance AspectRatio Revert
instance AspectRatio Unset


-- | Generates a CSS @aspect-ratio@ property with the given value.
aspectRatio :: AspectRatio a => a -> Builder
aspectRatio = prop "aspect-ratio:"
{-# INLINE aspectRatio #-}


-- | Generates a CSS @backdrop-filter@ property with the given value.
backdropFilter :: Builder -> Builder
backdropFilter = prop "backdrop-filter:"
{-# INLINE backdropFilter #-}


-- | Generates a CSS @backface-visibility@ property with the given value.
backfaceVisibility :: Builder -> Builder
backfaceVisibility = prop "backface-visibility:"
{-# INLINE backfaceVisibility #-}


-- | Generates a CSS @background@ property with the given value.
background :: Builder -> Builder
background = prop "background:"
{-# INLINE background #-}


-- | Generates a CSS @background-attachment@ property with the given value.
backgroundAttachment :: Builder -> Builder
backgroundAttachment = prop "background-attachment:"
{-# INLINE backgroundAttachment #-}


-- | Generates a CSS @background-blend-mode@ property with the given value.
backgroundBlendMode :: Builder -> Builder
backgroundBlendMode = prop "background-blend-mode:"
{-# INLINE backgroundBlendMode #-}


-- | Generates a CSS @background-clip@ property with the given value.
backgroundClip :: Builder -> Builder
backgroundClip = prop "background-clip:"
{-# INLINE backgroundClip #-}


-- | Generates a CSS @background-color@ property with the given value.
backgroundColor :: Builder -> Builder
backgroundColor = prop "background-color:"
{-# INLINE backgroundColor #-}


-- | Generates a CSS @background-image@ property with the given value.
backgroundImage :: Builder -> Builder
backgroundImage = prop "background-image:"
{-# INLINE backgroundImage #-}


-- | Generates a CSS @background-origin@ property with the given value.
backgroundOrigin :: Builder -> Builder
backgroundOrigin = prop "background-origin:"
{-# INLINE backgroundOrigin #-}


-- | Generates a CSS @background-position@ property with the given value.
backgroundPosition :: Builder -> Builder
backgroundPosition = prop "background-position:"
{-# INLINE backgroundPosition #-}


-- | Generates a CSS @background-position-x@ property with the given value.
backgroundPositionX :: Builder -> Builder
backgroundPositionX = prop "background-position-x:"
{-# INLINE backgroundPositionX #-}


-- | Generates a CSS @background-position-y@ property with the given value.
backgroundPositionY :: Builder -> Builder
backgroundPositionY = prop "background-position-y:"
{-# INLINE backgroundPositionY #-}


-- | Generates a CSS @background-repeat@ property with the given value.
backgroundRepeat :: Builder -> Builder
backgroundRepeat = prop "background-repeat:"
{-# INLINE backgroundRepeat #-}


-- | Generates a CSS @background-size@ property with the given value.
backgroundSize :: Builder -> Builder
backgroundSize = prop "background-size:"
{-# INLINE backgroundSize #-}


-- | Generates a CSS @block-size@ property with the given value.
blockSize :: Builder -> Builder
blockSize = prop "block-size:"
{-# INLINE blockSize #-}


-- | Generates a CSS @border@ property with the given value.
border :: Builder -> Builder
border = prop "border:"
{-# INLINE border #-}


-- | Generates a CSS @border-block@ property with the given value.
borderBlock :: Builder -> Builder
borderBlock = prop "border-block:"
{-# INLINE borderBlock #-}


-- | Generates a CSS @border-block-color@ property with the given value.
borderBlockColor :: Builder -> Builder
borderBlockColor = prop "border-block-color:"
{-# INLINE borderBlockColor #-}


-- | Generates a CSS @border-block-end-color@ property with the given value.
borderBlockEndColor :: Builder -> Builder
borderBlockEndColor = prop "border-block-end-color:"
{-# INLINE borderBlockEndColor #-}


-- | Generates a CSS @border-block-end-style@ property with the given value.
borderBlockEndStyle :: Builder -> Builder
borderBlockEndStyle = prop "border-block-end-style:"
{-# INLINE borderBlockEndStyle #-}


-- | Generates a CSS @border-block-end-width@ property with the given value.
borderBlockEndWidth :: Builder -> Builder
borderBlockEndWidth = prop "border-block-end-width:"
{-# INLINE borderBlockEndWidth #-}


-- | Generates a CSS @border-block-start-color@ property with the given value.
borderBlockStartColor :: Builder -> Builder
borderBlockStartColor = prop "border-block-start-color:"
{-# INLINE borderBlockStartColor #-}


-- | Generates a CSS @border-block-start-style@ property with the given value.
borderBlockStartStyle :: Builder -> Builder
borderBlockStartStyle = prop "border-block-start-style:"
{-# INLINE borderBlockStartStyle #-}


-- | Generates a CSS @border-block-start-width@ property with the given value.
borderBlockStartWidth :: Builder -> Builder
borderBlockStartWidth = prop "border-block-start-width:"
{-# INLINE borderBlockStartWidth #-}


-- | Generates a CSS @border-block-style@ property with the given value.
borderBlockStyle :: Builder -> Builder
borderBlockStyle = prop "border-block-style:"
{-# INLINE borderBlockStyle #-}


-- | Generates a CSS @border-block-width@ property with the given value.
borderBlockWidth :: Builder -> Builder
borderBlockWidth = prop "border-block-width:"
{-# INLINE borderBlockWidth #-}


-- | Generates a CSS @border-bottom@ property with the given value.
borderBottom :: Builder -> Builder
borderBottom = prop "border-bottom:"
{-# INLINE borderBottom #-}


-- | Generates a CSS @border-bottom-color@ property with the given value.
borderBottomColor :: Builder -> Builder
borderBottomColor = prop "border-bottom-color:"
{-# INLINE borderBottomColor #-}


-- | Generates a CSS @border-bottom-left-radius@ property with the given value.
borderBottomLeftRadius :: Builder -> Builder
borderBottomLeftRadius = prop "border-bottom-left-radius:"
{-# INLINE borderBottomLeftRadius #-}


-- | Generates a CSS @border-bottom-right-radius@ property with the given value.
borderBottomRightRadius :: Builder -> Builder
borderBottomRightRadius = prop "border-bottom-right-radius:"
{-# INLINE borderBottomRightRadius #-}


-- | Generates a CSS @border-bottom-style@ property with the given value.
borderBottomStyle :: Builder -> Builder
borderBottomStyle = prop "border-bottom-style:"
{-# INLINE borderBottomStyle #-}


-- | Generates a CSS @border-bottom-width@ property with the given value.
borderBottomWidth :: Builder -> Builder
borderBottomWidth = prop "border-bottom-width:"
{-# INLINE borderBottomWidth #-}


-- | Generates a CSS @border-collapse@ property with the given value.
borderCollapse :: Builder -> Builder
borderCollapse = prop "border-collapse:"
{-# INLINE borderCollapse #-}


-- | Generates a CSS @border-color@ property with the given value.
borderColor :: Builder -> Builder
borderColor = prop "border-color:"
{-# INLINE borderColor #-}


-- | Generates a CSS @border-end-end-radius@ property with the given value.
borderEndEndRadius :: Builder -> Builder
borderEndEndRadius = prop "border-end-end-radius:"
{-# INLINE borderEndEndRadius #-}


-- | Generates a CSS @border-end-start-radius@ property with the given value.
borderEndStartRadius :: Builder -> Builder
borderEndStartRadius = prop "border-end-start-radius:"
{-# INLINE borderEndStartRadius #-}


-- | Generates a CSS @border-image@ property with the given value.
borderImage :: Builder -> Builder
borderImage = prop "border-image:"
{-# INLINE borderImage #-}


-- | Generates a CSS @border-image-outset@ property with the given value.
borderImageOutset :: Builder -> Builder
borderImageOutset = prop "border-image-outset:"
{-# INLINE borderImageOutset #-}


-- | Generates a CSS @border-image-repeat@ property with the given value.
borderImageRepeat :: Builder -> Builder
borderImageRepeat = prop "border-image-repeat:"
{-# INLINE borderImageRepeat #-}


-- | Generates a CSS @border-image-slice@ property with the given value.
borderImageSlice :: Builder -> Builder
borderImageSlice = prop "border-image-slice:"
{-# INLINE borderImageSlice #-}


-- | Generates a CSS @border-image-source@ property with the given value.
borderImageSource :: Builder -> Builder
borderImageSource = prop "border-image-source:"
{-# INLINE borderImageSource #-}


-- | Generates a CSS @border-image-width@ property with the given value.
borderImageWidth :: Builder -> Builder
borderImageWidth = prop "border-image-width:"
{-# INLINE borderImageWidth #-}


-- | Generates a CSS @border-inline@ property with the given value.
borderInline :: Builder -> Builder
borderInline = prop "border-inline:"
{-# INLINE borderInline #-}


-- | Generates a CSS @border-inline-color@ property with the given value.
borderInlineColor :: Builder -> Builder
borderInlineColor = prop "border-inline-color:"
{-# INLINE borderInlineColor #-}


-- | Generates a CSS @border-inline-end-color@ property with the given value.
borderInlineEndColor :: Builder -> Builder
borderInlineEndColor = prop "border-inline-end-color:"
{-# INLINE borderInlineEndColor #-}


-- | Generates a CSS @border-inline-end-style@ property with the given value.
borderInlineEndStyle :: Builder -> Builder
borderInlineEndStyle = prop "border-inline-end-style:"
{-# INLINE borderInlineEndStyle #-}


-- | Generates a CSS @border-inline-end-width@ property with the given value.
borderInlineEndWidth :: Builder -> Builder
borderInlineEndWidth = prop "border-inline-end-width:"
{-# INLINE borderInlineEndWidth #-}


-- | Generates a CSS @border-inline-start-color@ property with the given value.
borderInlineStartColor :: Builder -> Builder
borderInlineStartColor = prop "border-inline-start-color:"
{-# INLINE borderInlineStartColor #-}


-- | Generates a CSS @border-inline-start-style@ property with the given value.
borderInlineStartStyle :: Builder -> Builder
borderInlineStartStyle = prop "border-inline-start-style:"
{-# INLINE borderInlineStartStyle #-}


-- | Generates a CSS @border-inline-start-width@ property with the given value.
borderInlineStartWidth :: Builder -> Builder
borderInlineStartWidth = prop "border-inline-start-width:"
{-# INLINE borderInlineStartWidth #-}


-- | Generates a CSS @border-inline-style@ property with the given value.
borderInlineStyle :: Builder -> Builder
borderInlineStyle = prop "border-inline-style:"
{-# INLINE borderInlineStyle #-}


-- | Generates a CSS @border-inline-width@ property with the given value.
borderInlineWidth :: Builder -> Builder
borderInlineWidth = prop "border-inline-width:"
{-# INLINE borderInlineWidth #-}


-- | Generates a CSS @border-left@ property with the given value.
borderLeft :: Builder -> Builder
borderLeft = prop "border-left:"
{-# INLINE borderLeft #-}


-- | Generates a CSS @border-left-color@ property with the given value.
borderLeftColor :: Builder -> Builder
borderLeftColor = prop "border-left-color:"
{-# INLINE borderLeftColor #-}


-- | Generates a CSS @border-left-style@ property with the given value.
borderLeftStyle :: Builder -> Builder
borderLeftStyle = prop "border-left-style:"
{-# INLINE borderLeftStyle #-}


-- | Generates a CSS @border-left-width@ property with the given value.
borderLeftWidth :: Builder -> Builder
borderLeftWidth = prop "border-left-width:"
{-# INLINE borderLeftWidth #-}


-- | Generates a CSS @border-radius@ property with the given value.
borderRadius :: Builder -> Builder
borderRadius = prop "border-radius:"
{-# INLINE borderRadius #-}


-- | Generates a CSS @border-right@ property with the given value.
borderRight :: Builder -> Builder
borderRight = prop "border-right:"
{-# INLINE borderRight #-}


-- | Generates a CSS @border-right-color@ property with the given value.
borderRightColor :: Builder -> Builder
borderRightColor = prop "border-right-color:"
{-# INLINE borderRightColor #-}


-- | Generates a CSS @border-right-style@ property with the given value.
borderRightStyle :: Builder -> Builder
borderRightStyle = prop "border-right-style:"
{-# INLINE borderRightStyle #-}


-- | Generates a CSS @border-right-width@ property with the given value.
borderRightWidth :: Builder -> Builder
borderRightWidth = prop "border-right-width:"
{-# INLINE borderRightWidth #-}


-- | Generates a CSS @border-spacing@ property with the given value.
borderSpacing :: Builder -> Builder
borderSpacing = prop "border-spacing:"
{-# INLINE borderSpacing #-}


-- | Generates a CSS @border-start-end-radius@ property with the given value.
borderStartEndRadius :: Builder -> Builder
borderStartEndRadius = prop "border-start-end-radius:"
{-# INLINE borderStartEndRadius #-}


-- | Generates a CSS @border-start-start-radius@ property with the given value.
borderStartStartRadius :: Builder -> Builder
borderStartStartRadius = prop "border-start-start-radius:"
{-# INLINE borderStartStartRadius #-}


-- | Generates a CSS @border-style@ property with the given value.
borderStyle :: Builder -> Builder
borderStyle = prop "border-style:"
{-# INLINE borderStyle #-}


-- | Generates a CSS @border-top@ property with the given value.
borderTop :: Builder -> Builder
borderTop = prop "border-top:"
{-# INLINE borderTop #-}


-- | Generates a CSS @border-top-color@ property with the given value.
borderTopColor :: Builder -> Builder
borderTopColor = prop "border-top-color:"
{-# INLINE borderTopColor #-}


-- | Generates a CSS @border-top-left-radius@ property with the given value.
borderTopLeftRadius :: Builder -> Builder
borderTopLeftRadius = prop "border-top-left-radius:"
{-# INLINE borderTopLeftRadius #-}


-- | Generates a CSS @border-top-right-radius@ property with the given value.
borderTopRightRadius :: Builder -> Builder
borderTopRightRadius = prop "border-top-right-radius:"
{-# INLINE borderTopRightRadius #-}


-- | Generates a CSS @border-top-style@ property with the given value.
borderTopStyle :: Builder -> Builder
borderTopStyle = prop "border-top-style:"
{-# INLINE borderTopStyle #-}


-- | Generates a CSS @border-top-width@ property with the given value.
borderTopWidth :: Builder -> Builder
borderTopWidth = prop "border-top-width:"
{-# INLINE borderTopWidth #-}


-- | Generates a CSS @border-width@ property with the given value.
borderWidth :: Builder -> Builder
borderWidth = prop "border-width:"
{-# INLINE borderWidth #-}


-- | Generates a CSS @bottom@ property with the given value.
bottom :: Builder -> Builder
bottom = prop "bottom:"
{-# INLINE bottom #-}


-- | Generates a CSS @box-decoration-break@ property with the given value.
boxDecorationBreak :: Builder -> Builder
boxDecorationBreak = prop "box-decoration-break:"
{-# INLINE boxDecorationBreak #-}


-- | Generates a CSS @box-reflect@ property with the given value.
boxReflect :: Builder -> Builder
boxReflect = prop "box-reflect:"
{-# INLINE boxReflect #-}


-- | Generates a CSS @box-shadow@ property with the given value.
boxShadow :: Builder -> Builder
boxShadow = prop "box-shadow:"
{-# INLINE boxShadow #-}


-- | Generates a CSS @box-sizing@ property with the given value.
boxSizing :: Builder -> Builder
boxSizing = prop "box-sizing:"
{-# INLINE boxSizing #-}


-- | Generates a CSS @break-after@ property with the given value.
breakAfter :: Builder -> Builder
breakAfter = prop "break-after:"
{-# INLINE breakAfter #-}


-- | Generates a CSS @break-before@ property with the given value.
breakBefore :: Builder -> Builder
breakBefore = prop "break-before:"
{-# INLINE breakBefore #-}


-- | Generates a CSS @break-inside@ property with the given value.
breakInside :: Builder -> Builder
breakInside = prop "break-inside:"
{-# INLINE breakInside #-}


-- | Generates a CSS @caption-side@ property with the given value.
captionSide :: Builder -> Builder
captionSide = prop "caption-side:"
{-# INLINE captionSide #-}


-- | Generates a CSS @caret-color@ property with the given value.
caretColor :: Builder -> Builder
caretColor = prop "caret-color:"
{-# INLINE caretColor #-}


-- | Generates a CSS @clear@ property with the given value.
clear :: Builder -> Builder
clear = prop "clear:"
{-# INLINE clear #-}


-- | Generates a CSS @clip@ property with the given value.
clip :: Builder -> Builder
clip = prop "clip:"
{-# INLINE clip #-}


-- | Generates a CSS @clip-path@ property with the given value.
clipPath :: Builder -> Builder
clipPath = prop "clip-path:"
{-# INLINE clipPath #-}


-- | Generates a CSS @color@ property with the given value.
color :: Builder -> Builder
color = prop "color:"
{-# INLINE color #-}


-- | Generates a CSS @column-count@ property with the given value.
columnCount :: Builder -> Builder
columnCount = prop "column-count:"
{-# INLINE columnCount #-}


-- | Generates a CSS @column-fill@ property with the given value.
columnFill :: Builder -> Builder
columnFill = prop "column-fill:"
{-# INLINE columnFill #-}


-- | Generates a CSS @column-gap@ property with the given value.
columnGap :: Builder -> Builder
columnGap = prop "column-gap:"
{-# INLINE columnGap #-}


-- | Generates a CSS @column-rule@ property with the given value.
columnRule :: Builder -> Builder
columnRule = prop "column-rule:"
{-# INLINE columnRule #-}


-- | Generates a CSS @column-rule-color@ property with the given value.
columnRuleColor :: Builder -> Builder
columnRuleColor = prop "column-rule-color:"
{-# INLINE columnRuleColor #-}


-- | Generates a CSS @column-rule-style@ property with the given value.
columnRuleStyle :: Builder -> Builder
columnRuleStyle = prop "column-rule-style:"
{-# INLINE columnRuleStyle #-}


-- | Generates a CSS @column-rule-width@ property with the given value.
columnRuleWidth :: Builder -> Builder
columnRuleWidth = prop "column-rule-width:"
{-# INLINE columnRuleWidth #-}


-- | Generates a CSS @column-span@ property with the given value.
columnSpan :: Builder -> Builder
columnSpan = prop "column-span:"
{-# INLINE columnSpan #-}


-- | Generates a CSS @column-width@ property with the given value.
columnWidth :: Builder -> Builder
columnWidth = prop "column-width:"
{-# INLINE columnWidth #-}


-- | Generates a CSS @columns@ property with the given value.
columns :: Builder -> Builder
columns = prop "columns:"
{-# INLINE columns #-}


-- | Generates a CSS @content@ property with the given value.
content :: Builder -> Builder
content = prop "content:"
{-# INLINE content #-}


-- | Generates a CSS @counter-increment@ property with the given value.
counterIncrement :: Builder -> Builder
counterIncrement = prop "counter-increment:"
{-# INLINE counterIncrement #-}


-- | Generates a CSS @counter-reset@ property with the given value.
counterReset :: Builder -> Builder
counterReset = prop "counter-reset:"
{-# INLINE counterReset #-}


-- | Generates a CSS @cursor@ property with the given value.
cursor :: Builder -> Builder
cursor = prop "cursor:"
{-# INLINE cursor #-}


-- | Generates a CSS @direction@ property with the given value.
direction :: Builder -> Builder
direction = prop "direction:"
{-# INLINE direction #-}


-- | Generates a CSS @display@ property with the given value.
display :: Builder -> Builder
display = prop "display:"
{-# INLINE display #-}


-- | Generates a CSS @empty-cells@ property with the given value.
emptyCells :: Builder -> Builder
emptyCells = prop "empty-cells:"
{-# INLINE emptyCells #-}


-- | Generates a CSS @filter@ property with the given value.
filter :: Builder -> Builder
filter = prop "filter:"
{-# INLINE filter #-}


-- | Generates a CSS @flex@ property with the given value.
flex :: Builder -> Builder
flex = prop "flex:"
{-# INLINE flex #-}


-- | Generates a CSS @flex-basis@ property with the given value.
flexBasis :: Builder -> Builder
flexBasis = prop "flex-basis:"
{-# INLINE flexBasis #-}


-- | Generates a CSS @flex-direction@ property with the given value.
flexDirection :: Builder -> Builder
flexDirection = prop "flex-direction:"
{-# INLINE flexDirection #-}


-- | Generates a CSS @flex-flow@ property with the given value.
flexFlow :: Builder -> Builder
flexFlow = prop "flex-flow:"
{-# INLINE flexFlow #-}


-- | Generates a CSS @flex-grow@ property with the given value.
flexGrow :: Builder -> Builder
flexGrow = prop "flex-grow:"
{-# INLINE flexGrow #-}


-- | Generates a CSS @flex-shrink@ property with the given value.
flexShrink :: Builder -> Builder
flexShrink = prop "flex-shrink:"
{-# INLINE flexShrink #-}


-- | Generates a CSS @flex-wrap@ property with the given value.
flexWrap :: Builder -> Builder
flexWrap = prop "flex-wrap:"
{-# INLINE flexWrap #-}


-- | Generates a CSS @float@ property with the given value.
float :: Builder -> Builder
float = prop "float:"
{-# INLINE float #-}


-- | Generates a CSS @font@ property with the given value.
font :: Builder -> Builder
font = prop "font:"
{-# INLINE font #-}


-- | Generates a CSS @font-family@ property with the given value.
fontFamily :: Builder -> Builder
fontFamily = prop "font-family:"
{-# INLINE fontFamily #-}


-- | Generates a CSS @font-feature-settings@ property with the given value.
fontFeatureSettings :: Builder -> Builder
fontFeatureSettings = prop "font-feature-settings:"
{-# INLINE fontFeatureSettings #-}


-- | Generates a CSS @font-kerning@ property with the given value.
fontKerning :: Builder -> Builder
fontKerning = prop "font-kerning:"
{-# INLINE fontKerning #-}


-- | Generates a CSS @font-size@ property with the given value.
fontSize :: Builder -> Builder
fontSize = prop "font-size:"
{-# INLINE fontSize #-}


-- | Generates a CSS @font-size-adjust@ property with the given value.
fontSizeAdjust :: Builder -> Builder
fontSizeAdjust = prop "font-size-adjust:"
{-# INLINE fontSizeAdjust #-}


-- | Generates a CSS @font-stretch@ property with the given value.
fontStretch :: Builder -> Builder
fontStretch = prop "font-stretch:"
{-# INLINE fontStretch #-}


-- | Generates a CSS @font-style@ property with the given value.
fontStyle :: Builder -> Builder
fontStyle = prop "font-style:"
{-# INLINE fontStyle #-}


-- | Generates a CSS @font-variant@ property with the given value.
fontVariant :: Builder -> Builder
fontVariant = prop "font-variant:"
{-# INLINE fontVariant #-}


-- | Generates a CSS @font-variant-caps@ property with the given value.
fontVariantCaps :: Builder -> Builder
fontVariantCaps = prop "font-variant-caps:"
{-# INLINE fontVariantCaps #-}


-- | Generates a CSS @font-weight@ property with the given value.
fontWeight :: Builder -> Builder
fontWeight = prop "font-weight:"
{-# INLINE fontWeight #-}


-- | Generates a CSS @gap@ property with the given value.
gap :: Builder -> Builder
gap = prop "gap:"
{-# INLINE gap #-}


-- | Generates a CSS @grid@ property with the given value.
grid :: Builder -> Builder
grid = prop "grid:"
{-# INLINE grid #-}


-- | Generates a CSS @grid-area@ property with the given value.
gridArea :: Builder -> Builder
gridArea = prop "grid-area:"
{-# INLINE gridArea #-}


-- | Generates a CSS @grid-auto-columns@ property with the given value.
gridAutoColumns :: Builder -> Builder
gridAutoColumns = prop "grid-auto-columns:"
{-# INLINE gridAutoColumns #-}


-- | Generates a CSS @grid-auto-flow@ property with the given value.
gridAutoFlow :: Builder -> Builder
gridAutoFlow = prop "grid-auto-flow:"
{-# INLINE gridAutoFlow #-}


-- | Generates a CSS @grid-auto-rows@ property with the given value.
gridAutoRows :: Builder -> Builder
gridAutoRows = prop "grid-auto-rows:"
{-# INLINE gridAutoRows #-}


-- | Generates a CSS @grid-column@ property with the given value.
gridColumn :: Builder -> Builder
gridColumn = prop "grid-column:"
{-# INLINE gridColumn #-}


-- | Generates a CSS @grid-column-end@ property with the given value.
gridColumnEnd :: Builder -> Builder
gridColumnEnd = prop "grid-column-end:"
{-# INLINE gridColumnEnd #-}


-- | Generates a CSS @grid-column-gap@ property with the given value.
gridColumnGap :: Builder -> Builder
gridColumnGap = prop "grid-column-gap:"
{-# INLINE gridColumnGap #-}


-- | Generates a CSS @grid-column-start@ property with the given value.
gridColumnStart :: Builder -> Builder
gridColumnStart = prop "grid-column-start:"
{-# INLINE gridColumnStart #-}


-- | Generates a CSS @grid-gap@ property with the given value.
gridGap :: Builder -> Builder
gridGap = prop "grid-gap:"
{-# INLINE gridGap #-}


-- | Generates a CSS @grid-row@ property with the given value.
gridRow :: Builder -> Builder
gridRow = prop "grid-row:"
{-# INLINE gridRow #-}


-- | Generates a CSS @grid-row-end@ property with the given value.
gridRowEnd :: Builder -> Builder
gridRowEnd = prop "grid-row-end:"
{-# INLINE gridRowEnd #-}


-- | Generates a CSS @grid-row-gap@ property with the given value.
gridRowGap :: Builder -> Builder
gridRowGap = prop "grid-row-gap:"
{-# INLINE gridRowGap #-}


-- | Generates a CSS @grid-row-start@ property with the given value.
gridRowStart :: Builder -> Builder
gridRowStart = prop "grid-row-start:"
{-# INLINE gridRowStart #-}


-- | Generates a CSS @grid-template@ property with the given value.
gridTemplate :: Builder -> Builder
gridTemplate = prop "grid-template:"
{-# INLINE gridTemplate #-}


-- | Generates a CSS @grid-template-areas@ property with the given value.
gridTemplateAreas :: Builder -> Builder
gridTemplateAreas = prop "grid-template-areas:"
{-# INLINE gridTemplateAreas #-}


-- | Generates a CSS @grid-template-columns@ property with the given value.
gridTemplateColumns :: Builder -> Builder
gridTemplateColumns = prop "grid-template-columns:"
{-# INLINE gridTemplateColumns #-}


-- | Generates a CSS @grid-template-rows@ property with the given value.
gridTemplateRows :: Builder -> Builder
gridTemplateRows = prop "grid-template-rows:"
{-# INLINE gridTemplateRows #-}


-- | Generates a CSS @hanging-punctuation@ property with the given value.
hangingPunctuation :: Builder -> Builder
hangingPunctuation = prop "hanging-punctuation:"
{-# INLINE hangingPunctuation #-}


-- | Generates a CSS @height@ property with the given value.
height :: Builder -> Builder
height = prop "height:"
{-# INLINE height #-}


-- | Generates a CSS @hyphens@ property with the given value.
hyphens :: Builder -> Builder
hyphens = prop "hyphens:"
{-# INLINE hyphens #-}


-- | Generates a CSS @image-rendering@ property with the given value.
imageRendering :: Builder -> Builder
imageRendering = prop "image-rendering:"
{-# INLINE imageRendering #-}


-- | Generates a CSS @inline-size@ property with the given value.
inlineSize :: Builder -> Builder
inlineSize = prop "inline-size:"
{-# INLINE inlineSize #-}


-- | Generates a CSS @inset@ property with the given value.
inset :: Builder -> Builder
inset = prop "inset:"
{-# INLINE inset #-}


-- | Generates a CSS @inset-block@ property with the given value.
insetBlock :: Builder -> Builder
insetBlock = prop "inset-block:"
{-# INLINE insetBlock #-}


-- | Generates a CSS @inset-block-end@ property with the given value.
insetBlockEnd :: Builder -> Builder
insetBlockEnd = prop "inset-block-end:"
{-# INLINE insetBlockEnd #-}


-- | Generates a CSS @inset-block-start@ property with the given value.
insetBlockStart :: Builder -> Builder
insetBlockStart = prop "inset-block-start:"
{-# INLINE insetBlockStart #-}


-- | Generates a CSS @inset-inline@ property with the given value.
insetInline :: Builder -> Builder
insetInline = prop "inset-inline:"
{-# INLINE insetInline #-}


-- | Generates a CSS @inset-inline-end@ property with the given value.
insetInlineEnd :: Builder -> Builder
insetInlineEnd = prop "inset-inline-end:"
{-# INLINE insetInlineEnd #-}


-- | Generates a CSS @inset-inline-start@ property with the given value.
insetInlineStart :: Builder -> Builder
insetInlineStart = prop "inset-inline-start:"
{-# INLINE insetInlineStart #-}


-- | Generates a CSS @isolation@ property with the given value.
isolation :: Builder -> Builder
isolation = prop "isolation:"
{-# INLINE isolation #-}


-- | Generates a CSS @justify-content@ property with the given value.
justifyContent :: Builder -> Builder
justifyContent = prop "justify-content:"
{-# INLINE justifyContent #-}


-- | Generates a CSS @justify-items@ property with the given value.
justifyItems :: Builder -> Builder
justifyItems = prop "justify-items:"
{-# INLINE justifyItems #-}


-- | Generates a CSS @justify-self@ property with the given value.
justifySelf :: Builder -> Builder
justifySelf = prop "justify-self:"
{-# INLINE justifySelf #-}


-- | Generates a CSS @left@ property with the given value.
left :: Builder -> Builder
left = prop "left:"
{-# INLINE left #-}


-- | Generates a CSS @letter-spacing@ property with the given value.
letterSpacing :: Builder -> Builder
letterSpacing = prop "letter-spacing:"
{-# INLINE letterSpacing #-}


-- | Generates a CSS @line-height@ property with the given value.
lineHeight :: Builder -> Builder
lineHeight = prop "line-height:"
{-# INLINE lineHeight #-}


-- | Generates a CSS @list-style@ property with the given value.
listStyle :: Builder -> Builder
listStyle = prop "list-style:"
{-# INLINE listStyle #-}


-- | Generates a CSS @list-style-image@ property with the given value.
listStyleImage :: Builder -> Builder
listStyleImage = prop "list-style-image:"
{-# INLINE listStyleImage #-}


-- | Generates a CSS @list-style-position@ property with the given value.
listStylePosition :: Builder -> Builder
listStylePosition = prop "list-style-position:"
{-# INLINE listStylePosition #-}


-- | Generates a CSS @list-style-type@ property with the given value.
listStyleType :: Builder -> Builder
listStyleType = prop "list-style-type:"
{-# INLINE listStyleType #-}


-- | Generates a CSS @margin@ property with the given value.
margin :: Builder -> Builder
margin = prop "margin:"
{-# INLINE margin #-}


-- | Generates a CSS @margin-block@ property with the given value.
marginBlock :: Builder -> Builder
marginBlock = prop "margin-block:"
{-# INLINE marginBlock #-}


-- | Generates a CSS @margin-block-end@ property with the given value.
marginBlockEnd :: Builder -> Builder
marginBlockEnd = prop "margin-block-end:"
{-# INLINE marginBlockEnd #-}


-- | Generates a CSS @margin-block-start@ property with the given value.
marginBlockStart :: Builder -> Builder
marginBlockStart = prop "margin-block-start:"
{-# INLINE marginBlockStart #-}


-- | Generates a CSS @margin-bottom@ property with the given value.
marginBottom :: Builder -> Builder
marginBottom = prop "margin-bottom:"
{-# INLINE marginBottom #-}


-- | Generates a CSS @margin-inline@ property with the given value.
marginInline :: Builder -> Builder
marginInline = prop "margin-inline:"
{-# INLINE marginInline #-}


-- | Generates a CSS @margin-inline-end@ property with the given value.
marginInlineEnd :: Builder -> Builder
marginInlineEnd = prop "margin-inline-end:"
{-# INLINE marginInlineEnd #-}


-- | Generates a CSS @margin-inline-start@ property with the given value.
marginInlineStart :: Builder -> Builder
marginInlineStart = prop "margin-inline-start:"
{-# INLINE marginInlineStart #-}


-- | Generates a CSS @margin-left@ property with the given value.
marginLeft :: Builder -> Builder
marginLeft = prop "margin-left:"
{-# INLINE marginLeft #-}


-- | Generates a CSS @margin-right@ property with the given value.
marginRight :: Builder -> Builder
marginRight = prop "margin-right:"
{-# INLINE marginRight #-}


-- | Generates a CSS @margin-top@ property with the given value.
marginTop :: Builder -> Builder
marginTop = prop "margin-top:"
{-# INLINE marginTop #-}


-- | Generates a CSS @mask-image@ property with the given value.
maskImage :: Builder -> Builder
maskImage = prop "mask-image:"
{-# INLINE maskImage #-}


-- | Generates a CSS @mask-mode@ property with the given value.
maskMode :: Builder -> Builder
maskMode = prop "mask-mode:"
{-# INLINE maskMode #-}


-- | Generates a CSS @mask-origin@ property with the given value.
maskOrigin :: Builder -> Builder
maskOrigin = prop "mask-origin:"
{-# INLINE maskOrigin #-}


-- | Generates a CSS @mask-position@ property with the given value.
maskPosition :: Builder -> Builder
maskPosition = prop "mask-position:"
{-# INLINE maskPosition #-}


-- | Generates a CSS @mask-repeat@ property with the given value.
maskRepeat :: Builder -> Builder
maskRepeat = prop "mask-repeat:"
{-# INLINE maskRepeat #-}


-- | Generates a CSS @mask-size@ property with the given value.
maskSize :: Builder -> Builder
maskSize = prop "mask-size:"
{-# INLINE maskSize #-}


-- | Generates a CSS @max-block-size@ property with the given value.
maxBlockSize :: Builder -> Builder
maxBlockSize = prop "max-block-size:"
{-# INLINE maxBlockSize #-}


-- | Generates a CSS @max-height@ property with the given value.
maxHeight :: Builder -> Builder
maxHeight = prop "max-height:"
{-# INLINE maxHeight #-}


-- | Generates a CSS @max-inline-size@ property with the given value.
maxInlineSize :: Builder -> Builder
maxInlineSize = prop "max-inline-size:"
{-# INLINE maxInlineSize #-}


-- | Generates a CSS @max-width@ property with the given value.
maxWidth :: Builder -> Builder
maxWidth = prop "max-width:"
{-# INLINE maxWidth #-}


-- | Generates a CSS @min-block-size@ property with the given value.
minBlockSize :: Builder -> Builder
minBlockSize = prop "min-block-size:"
{-# INLINE minBlockSize #-}


-- | Generates a CSS @min-inline-size@ property with the given value.
minInlineSize :: Builder -> Builder
minInlineSize = prop "min-inline-size:"
{-# INLINE minInlineSize #-}


-- | Generates a CSS @min-height@ property with the given value.
minHeight :: Builder -> Builder
minHeight = prop "min-height:"
{-# INLINE minHeight #-}


-- | Generates a CSS @min-width@ property with the given value.
minWidth :: Builder -> Builder
minWidth = prop "min-width:"
{-# INLINE minWidth #-}


-- | Generates a CSS @mix-blend-mode@ property with the given value.
mixBlendMode :: Builder -> Builder
mixBlendMode = prop "mix-blend-mode:"
{-# INLINE mixBlendMode #-}


-- | Generates a CSS @object-fit@ property with the given value.
objectFit :: Builder -> Builder
objectFit = prop "object-fit:"
{-# INLINE objectFit #-}


-- | Generates a CSS @object-position@ property with the given value.
objectPosition :: Builder -> Builder
objectPosition = prop "object-position:"
{-# INLINE objectPosition #-}


-- | Generates a CSS @offset@ property with the given value.
offset :: Builder -> Builder
offset = prop "offset:"
{-# INLINE offset #-}


-- | Generates a CSS @offset-anchor@ property with the given value.
offsetAnchor :: Builder -> Builder
offsetAnchor = prop "offset-anchor:"
{-# INLINE offsetAnchor #-}


-- | Generates a CSS @offset-distance@ property with the given value.
offsetDistance :: Builder -> Builder
offsetDistance = prop "offset-distance:"
{-# INLINE offsetDistance #-}


-- | Generates a CSS @offset-path@ property with the given value.
offsetPath :: Builder -> Builder
offsetPath = prop "offset-path:"
{-# INLINE offsetPath #-}


-- | Generates a CSS @offset-rotate@ property with the given value.
offsetRotate :: Builder -> Builder
offsetRotate = prop "offset-rotate:"
{-# INLINE offsetRotate #-}


-- | Generates a CSS @opacity@ property with the given value.
opacity :: Builder -> Builder
opacity = prop "opacity:"
{-# INLINE opacity #-}


-- | Generates a CSS @order@ property with the given value.
order :: Builder -> Builder
order = prop "order:"
{-# INLINE order #-}


-- | Generates a CSS @orphans@ property with the given value.
orphans :: Builder -> Builder
orphans = prop "orphans:"
{-# INLINE orphans #-}


-- | Generates a CSS @outline@ property with the given value.
outline :: Builder -> Builder
outline = prop "outline:"
{-# INLINE outline #-}


-- | Generates a CSS @outline-color@ property with the given value.
outlineColor :: Builder -> Builder
outlineColor = prop "outline-color:"
{-# INLINE outlineColor #-}


-- | Generates a CSS @outline-offset@ property with the given value.
outlineOffset :: Builder -> Builder
outlineOffset = prop "outline-offset:"
{-# INLINE outlineOffset #-}


-- | Generates a CSS @outline-style@ property with the given value.
outlineStyle :: Builder -> Builder
outlineStyle = prop "outline-style:"
{-# INLINE outlineStyle #-}


-- | Generates a CSS @outline-width@ property with the given value.
outlineWidth :: Builder -> Builder
outlineWidth = prop "outline-width:"
{-# INLINE outlineWidth #-}


-- | Generates a CSS @overflow@ property with the given value.
overflow :: Builder -> Builder
overflow = prop "overflow:"
{-# INLINE overflow #-}


-- | Generates a CSS @overflow-anchor@ property with the given value.
overflowAnchor :: Builder -> Builder
overflowAnchor = prop "overflow-anchor:"
{-# INLINE overflowAnchor #-}


-- | Generates a CSS @overflow-wrap@ property with the given value.
overflowWrap :: Builder -> Builder
overflowWrap = prop "overflow-wrap:"
{-# INLINE overflowWrap #-}


-- | Generates a CSS @overflow-x@ property with the given value.
overflowX :: Builder -> Builder
overflowX = prop "overflow-x:"
{-# INLINE overflowX #-}


-- | Generates a CSS @overflow-y@ property with the given value.
overflowY :: Builder -> Builder
overflowY = prop "overflow-y:"
{-# INLINE overflowY #-}


-- | Generates a CSS @overscroll-behavior@ property with the given value.
overscrollBehavior :: Builder -> Builder
overscrollBehavior = prop "overscroll-behavior:"
{-# INLINE overscrollBehavior #-}


-- | Generates a CSS @overscroll-behavior-block@ property with the given value.
overscrollBehaviorBlock :: Builder -> Builder
overscrollBehaviorBlock = prop "overscroll-behavior-block:"
{-# INLINE overscrollBehaviorBlock #-}


-- | Generates a CSS @overscroll-behavior-inline@ property with the given value.
overscrollBehaviorInline :: Builder -> Builder
overscrollBehaviorInline = prop "overscroll-behavior-inline:"
{-# INLINE overscrollBehaviorInline #-}


-- | Generates a CSS @overscroll-behavior-x@ property with the given value.
overscrollBehaviorX :: Builder -> Builder
overscrollBehaviorX = prop "overscroll-behavior-x:"
{-# INLINE overscrollBehaviorX #-}


-- | Generates a CSS @overscroll-behavior-y@ property with the given value.
overscrollBehaviorY :: Builder -> Builder
overscrollBehaviorY = prop "overscroll-behavior-y:"
{-# INLINE overscrollBehaviorY #-}


-- | Generates a CSS @padding@ property with the given value.
padding :: Builder -> Builder
padding = prop "padding:"
{-# INLINE padding #-}


-- | Generates a CSS @padding-block@ property with the given value.
paddingBlock :: Builder -> Builder
paddingBlock = prop "padding-block:"
{-# INLINE paddingBlock #-}


-- | Generates a CSS @padding-block-end@ property with the given value.
paddingBlockEnd :: Builder -> Builder
paddingBlockEnd = prop "padding-block-end:"
{-# INLINE paddingBlockEnd #-}


-- | Generates a CSS @padding-block-start@ property with the given value.
paddingBlockStart :: Builder -> Builder
paddingBlockStart = prop "padding-block-start:"
{-# INLINE paddingBlockStart #-}


-- | Generates a CSS @padding-bottom@ property with the given value.
paddingBottom :: Builder -> Builder
paddingBottom = prop "padding-bottom:"
{-# INLINE paddingBottom #-}


-- | Generates a CSS @padding-inline@ property with the given value.
paddingInline :: Builder -> Builder
paddingInline = prop "padding-inline:"
{-# INLINE paddingInline #-}


-- | Generates a CSS @padding-inline-end@ property with the given value.
paddingInlineEnd :: Builder -> Builder
paddingInlineEnd = prop "padding-inline-end:"
{-# INLINE paddingInlineEnd #-}


-- | Generates a CSS @padding-inline-start@ property with the given value.
paddingInlineStart :: Builder -> Builder
paddingInlineStart = prop "padding-inline-start:"
{-# INLINE paddingInlineStart #-}


-- | Generates a CSS @padding-left@ property with the given value.
paddingLeft :: Builder -> Builder
paddingLeft = prop "padding-left:"
{-# INLINE paddingLeft #-}


-- | Generates a CSS @padding-right@ property with the given value.
paddingRight :: Builder -> Builder
paddingRight = prop "padding-right:"
{-# INLINE paddingRight #-}


-- | Generates a CSS @padding-top@ property with the given value.
paddingTop :: Builder -> Builder
paddingTop = prop "padding-top:"
{-# INLINE paddingTop #-}


-- | Generates a CSS @page-break-after@ property with the given value.
pageBreakAfter :: Builder -> Builder
pageBreakAfter = prop "page-break-after:"
{-# INLINE pageBreakAfter #-}


-- | Generates a CSS @page-break-before@ property with the given value.
pageBreakBefore :: Builder -> Builder
pageBreakBefore = prop "page-break-before:"
{-# INLINE pageBreakBefore #-}


-- | Generates a CSS @page-break-inside@ property with the given value.
pageBreakInside :: Builder -> Builder
pageBreakInside = prop "page-break-inside:"
{-# INLINE pageBreakInside #-}


-- | Generates a CSS @paint-order@ property with the given value.
paintOrder :: Builder -> Builder
paintOrder = prop "paint-order:"
{-# INLINE paintOrder #-}


-- | Generates a CSS @perspective@ property with the given value.
perspective :: Builder -> Builder
perspective = prop "perspective:"
{-# INLINE perspective #-}


-- | Generates a CSS @perspective-origin@ property with the given value.
perspectiveOrigin :: Builder -> Builder
perspectiveOrigin = prop "perspective-origin:"
{-# INLINE perspectiveOrigin #-}


-- | Generates a CSS @place-content@ property with the given value.
placeContent :: Builder -> Builder
placeContent = prop "place-content:"
{-# INLINE placeContent #-}


-- | Generates a CSS @place-items@ property with the given value.
placeItems :: Builder -> Builder
placeItems = prop "place-items:"
{-# INLINE placeItems #-}


-- | Generates a CSS @place-self@ property with the given value.
placeSelf :: Builder -> Builder
placeSelf = prop "place-self:"
{-# INLINE placeSelf #-}


-- | Generates a CSS @pointer-events@ property with the given value.
pointerEvents :: Builder -> Builder
pointerEvents = prop "pointer-events:"
{-# INLINE pointerEvents #-}


-- | Generates a CSS @position@ property with the given value.
position :: Builder -> Builder
position = prop "position:"
{-# INLINE position #-}


-- | Generates a CSS @quotes@ property with the given value.
quotes :: Builder -> Builder
quotes = prop "quotes:"
{-# INLINE quotes #-}


-- | Generates a CSS @resize@ property with the given value.
resize :: Builder -> Builder
resize = prop "resize:"
{-# INLINE resize #-}


-- | Generates a CSS @right@ property with the given value.
right :: Builder -> Builder
right = prop "right:"
{-# INLINE right #-}


-- | Generates a CSS @rotate@ property with the given value.
rotate :: Builder -> Builder
rotate = prop "rotate:"
{-# INLINE rotate #-}


-- | Generates a CSS @row-gap@ property with the given value.
rowGap :: Builder -> Builder
rowGap = prop "row-gap:"
{-# INLINE rowGap #-}


-- | Generates a CSS @scale@ property with the given value.
scale :: Builder -> Builder
scale = prop "scale:"
{-# INLINE scale #-}


-- | Generates a CSS @scroll-behavior@ property with the given value.
scrollBehavior :: Builder -> Builder
scrollBehavior = prop "scroll-behavior:"
{-# INLINE scrollBehavior #-}


-- | Generates a CSS @scroll-margin@ property with the given value.
scrollMargin :: Builder -> Builder
scrollMargin = prop "scroll-margin:"
{-# INLINE scrollMargin #-}


-- | Generates a CSS @scroll-margin-block@ property with the given value.
scrollMarginBlock :: Builder -> Builder
scrollMarginBlock = prop "scroll-margin-block:"
{-# INLINE scrollMarginBlock #-}


-- | Generates a CSS @scroll-margin-block-end@ property with the given value.
scrollMarginBlockEnd :: Builder -> Builder
scrollMarginBlockEnd = prop "scroll-margin-block-end:"
{-# INLINE scrollMarginBlockEnd #-}


-- | Generates a CSS @scroll-margin-block-start@ property with the given value.
scrollMarginBlockStart :: Builder -> Builder
scrollMarginBlockStart = prop "scroll-margin-block-start:"
{-# INLINE scrollMarginBlockStart #-}


-- | Generates a CSS @scroll-margin-bottom@ property with the given value.
scrollMarginBottom :: Builder -> Builder
scrollMarginBottom = prop "scroll-margin-bottom:"
{-# INLINE scrollMarginBottom #-}


-- | Generates a CSS @scroll-margin-inline@ property with the given value.
scrollMarginInline :: Builder -> Builder
scrollMarginInline = prop "scroll-margin-inline:"
{-# INLINE scrollMarginInline #-}


-- | Generates a CSS @scroll-margin-inline-end@ property with the given value.
scrollMarginInlineEnd :: Builder -> Builder
scrollMarginInlineEnd = prop "scroll-margin-inline-end:"
{-# INLINE scrollMarginInlineEnd #-}


-- | Generates a CSS @scroll-margin-inline-start@ property with the given value.
scrollMarginInlineStart :: Builder -> Builder
scrollMarginInlineStart = prop "scroll-margin-inline-start:"
{-# INLINE scrollMarginInlineStart #-}


-- | Generates a CSS @scroll-margin-left@ property with the given value.
scrollMarginLeft :: Builder -> Builder
scrollMarginLeft = prop "scroll-margin-left:"
{-# INLINE scrollMarginLeft #-}


-- | Generates a CSS @scroll-margin-right@ property with the given value.
scrollMarginRight :: Builder -> Builder
scrollMarginRight = prop "scroll-margin-right:"
{-# INLINE scrollMarginRight #-}


-- | Generates a CSS @scroll-margin-top@ property with the given value.
scrollMarginTop :: Builder -> Builder
scrollMarginTop = prop "scroll-margin-top:"
{-# INLINE scrollMarginTop #-}


-- | Generates a CSS @scroll-padding@ property with the given value.
scrollPadding :: Builder -> Builder
scrollPadding = prop "scroll-padding:"
{-# INLINE scrollPadding #-}


-- | Generates a CSS @scroll-padding-block@ property with the given value.
scrollPaddingBlock :: Builder -> Builder
scrollPaddingBlock = prop "scroll-padding-block:"
{-# INLINE scrollPaddingBlock #-}


-- | Generates a CSS @scroll-padding-block-end@ property with the given value.
scrollPaddingBlockEnd :: Builder -> Builder
scrollPaddingBlockEnd = prop "scroll-padding-block-end:"
{-# INLINE scrollPaddingBlockEnd #-}


-- | Generates a CSS @scroll-padding-block-start@ property with the given value.
scrollPaddingBlockStart :: Builder -> Builder
scrollPaddingBlockStart = prop "scroll-padding-block-start:"
{-# INLINE scrollPaddingBlockStart #-}


-- | Generates a CSS @scroll-padding-bottom@ property with the given value.
scrollPaddingBottom :: Builder -> Builder
scrollPaddingBottom = prop "scroll-padding-bottom:"
{-# INLINE scrollPaddingBottom #-}


-- | Generates a CSS @scroll-padding-inline@ property with the given value.
scrollPaddingInline :: Builder -> Builder
scrollPaddingInline = prop "scroll-padding-inline:"
{-# INLINE scrollPaddingInline #-}


-- | Generates a CSS @scroll-padding-inline-end@ property with the given value.
scrollPaddingInlineEnd :: Builder -> Builder
scrollPaddingInlineEnd = prop "scroll-padding-inline-end:"
{-# INLINE scrollPaddingInlineEnd #-}


-- | Generates a CSS @scroll-padding-inline-start@ property with the given value.
scrollPaddingInlineStart :: Builder -> Builder
scrollPaddingInlineStart = prop "scroll-padding-inline-start:"
{-# INLINE scrollPaddingInlineStart #-}


-- | Generates a CSS @scroll-padding-left@ property with the given value.
scrollPaddingLeft :: Builder -> Builder
scrollPaddingLeft = prop "scroll-padding-left:"
{-# INLINE scrollPaddingLeft #-}


-- | Generates a CSS @scroll-padding-right@ property with the given value.
scrollPaddingRight :: Builder -> Builder
scrollPaddingRight = prop "scroll-padding-right:"
{-# INLINE scrollPaddingRight #-}


-- | Generates a CSS @scroll-padding-top@ property with the given value.
scrollPaddingTop :: Builder -> Builder
scrollPaddingTop = prop "scroll-padding-top:"
{-# INLINE scrollPaddingTop #-}


-- | Generates a CSS @scroll-snap-align@ property with the given value.
scrollSnapAlign :: Builder -> Builder
scrollSnapAlign = prop "scroll-snap-align:"
{-# INLINE scrollSnapAlign #-}


-- | Generates a CSS @scroll-snap-stop@ property with the given value.
scrollSnapStop :: Builder -> Builder
scrollSnapStop = prop "scroll-snap-stop:"
{-# INLINE scrollSnapStop #-}


-- | Generates a CSS @scroll-snap-type@ property with the given value.
scrollSnapType :: Builder -> Builder
scrollSnapType = prop "scroll-snap-type:"
{-# INLINE scrollSnapType #-}


-- | Generates a CSS @scrollbar-color@ property with the given value.
scrollbarColor :: Builder -> Builder
scrollbarColor = prop "scrollbar-color:"
{-# INLINE scrollbarColor #-}


-- | Generates a CSS @tab-size@ property with the given value.
tabSize :: Builder -> Builder
tabSize = prop "tab-size:"
{-# INLINE tabSize #-}


-- | Generates a CSS @table-layout@ property with the given value.
tableLayout :: Builder -> Builder
tableLayout = prop "table-layout:"
{-# INLINE tableLayout #-}


-- | Generates a CSS @text-align@ property with the given value.
textAlign :: Builder -> Builder
textAlign = prop "text-align:"
{-# INLINE textAlign #-}


-- | Generates a CSS @text-align-last@ property with the given value.
textAlignLast :: Builder -> Builder
textAlignLast = prop "text-align-last:"
{-# INLINE textAlignLast #-}


-- | Generates a CSS @text-decoration@ property with the given value.
textDecoration :: Builder -> Builder
textDecoration = prop "text-decoration:"
{-# INLINE textDecoration #-}


-- | Generates a CSS @text-decoration-color@ property with the given value.
textDecorationColor :: Builder -> Builder
textDecorationColor = prop "text-decoration-color:"
{-# INLINE textDecorationColor #-}


-- | Generates a CSS @text-decoration-line@ property with the given value.
textDecorationLine :: Builder -> Builder
textDecorationLine = prop "text-decoration-line:"
{-# INLINE textDecorationLine #-}


-- | Generates a CSS @text-decoration-style@ property with the given value.
textDecorationStyle :: Builder -> Builder
textDecorationStyle = prop "text-decoration-style:"
{-# INLINE textDecorationStyle #-}


-- | Generates a CSS @text-decoration-thickness@ property with the given value.
textDecorationThickness :: Builder -> Builder
textDecorationThickness = prop "text-decoration-thickness:"
{-# INLINE textDecorationThickness #-}


-- | Generates a CSS @text-indent@ property with the given value.
textIndent :: Builder -> Builder
textIndent = prop "text-indent:"
{-# INLINE textIndent #-}


-- | Generates a CSS @text-justify@ property with the given value.
textJustify :: Builder -> Builder
textJustify = prop "text-justify:"
{-# INLINE textJustify #-}


-- | Generates a CSS @text-orientation@ property with the given value.
textOrientation :: Builder -> Builder
textOrientation = prop "text-orientation:"
{-# INLINE textOrientation #-}


-- | Generates a CSS @text-overflow@ property with the given value.
textOverflow :: Builder -> Builder
textOverflow = prop "text-overflow:"
{-# INLINE textOverflow #-}


-- | Generates a CSS @text-shadow@ property with the given value.
textShadow :: Builder -> Builder
textShadow = prop "text-shadow:"
{-# INLINE textShadow #-}


-- | Generates a CSS @text-transform@ property with the given value.
textTransform :: Builder -> Builder
textTransform = prop "text-transform:"
{-# INLINE textTransform #-}


-- | Generates a CSS @top@ property with the given value.
top :: Builder -> Builder
top = prop "top:"
{-# INLINE top #-}


-- | Generates a CSS @transform@ property with the given value.
transform :: Builder -> Builder
transform = prop "transform:"
{-# INLINE transform #-}


-- | Generates a CSS @transform-origin@ property with the given value.
transformOrigin :: Builder -> Builder
transformOrigin = prop "transform-origin:"
{-# INLINE transformOrigin #-}


-- | Generates a CSS @transform-style@ property with the given value.
transformStyle :: Builder -> Builder
transformStyle = prop "transform-style:"
{-# INLINE transformStyle #-}


-- | Generates a CSS @transition@ property with the given value.
transition :: Builder -> Builder
transition = prop "transition:"
{-# INLINE transition #-}


-- | Generates a CSS @transition-delay@ property with the given value.
transitionDelay :: Builder -> Builder
transitionDelay = prop "transition-delay:"
{-# INLINE transitionDelay #-}


-- | Generates a CSS @transition-duration@ property with the given value.
transitionDuration :: Builder -> Builder
transitionDuration = prop "transition-duration:"
{-# INLINE transitionDuration #-}


-- | Generates a CSS @transition-property@ property with the given value.
transitionProperty :: Builder -> Builder
transitionProperty = prop "transition-property:"
{-# INLINE transitionProperty #-}


-- | Generates a CSS @transition-timing-function@ property with the given value.
transitionTimingFunction :: Builder -> Builder
transitionTimingFunction = prop "transition-timing-function:"
{-# INLINE transitionTimingFunction #-}


-- | Generates a CSS @translate@ property with the given value.
translate :: Builder -> Builder
translate = prop "translate:"
{-# INLINE translate #-}


-- | Generates a CSS @unicode-bidi@ property with the given value.
unicodeBidi :: Builder -> Builder
unicodeBidi = prop "unicode-bidi:"
{-# INLINE unicodeBidi #-}


-- | Generates a CSS @user-select@ property with the given value.
userSelect :: Builder -> Builder
userSelect = prop "user-select:"
{-# INLINE userSelect #-}


-- | Generates a CSS @vertical-align@ property with the given value.
verticalAlign :: Builder -> Builder
verticalAlign = prop "vertical-align:"
{-# INLINE verticalAlign #-}


-- | Generates a CSS @visibility@ property with the given value.
visibility :: Builder -> Builder
visibility = prop "visibility:"
{-# INLINE visibility #-}


-- | Generates a CSS @white-space@ property with the given value.
whiteSpace :: Builder -> Builder
whiteSpace = prop "white-space:"
{-# INLINE whiteSpace #-}


-- | Generates a CSS @widows@ property with the given value.
widows :: Builder -> Builder
widows = prop "widows:"
{-# INLINE widows #-}


-- | Generates a CSS @width@ property with the given value.
width :: Builder -> Builder
width = prop "width:"
{-# INLINE width #-}


-- | Generates a CSS @word-break@ property with the given value.
wordBreak :: Builder -> Builder
wordBreak = prop "word-break:"
{-# INLINE wordBreak #-}


-- | Generates a CSS @word-spacing@ property with the given value.
wordSpacing :: Builder -> Builder
wordSpacing = prop "word-spacing:"
{-# INLINE wordSpacing #-}


-- | Generates a CSS @word-wrap@ property with the given value.
wordWrap :: Builder -> Builder
wordWrap = prop "word-wrap:"
{-# INLINE wordWrap #-}


-- | Generates a CSS @writing-mode@ property with the given value.
writingMode :: Builder -> Builder
writingMode = prop "writing-mode:"
{-# INLINE writingMode #-}


-- | Generates a CSS @z-index@ property with the given value.
zIndex :: Builder -> Builder
zIndex = prop "z-index:"
{-# INLINE zIndex #-}


-- HELPER FUNCTIONS


prop :: Buildable a => Builder -> a -> Builder
prop key value = key <> build value <> singleton ';'


props :: Builder -> [Builder] -> Builder
props key value = key <> value' <> singleton ';' where value' = fold $ intersperse (singleton ' ') value
