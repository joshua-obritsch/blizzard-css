{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Animation
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Animation" module provides a set of types and functions for generating animation-related data types in CSS.
module Css.DataTypes.Animation
    ( -- * Data Types
      -- ** \<easing-function\>
      EasingFunction
      -- ** \<single-animation-composition\>
    , SingleAnimationComposition
      -- ** \<single-animation-direction\>
    , SingleAnimationDirection
      -- ** \<single-animation-fill-mode\>
    , SingleAnimationFillMode
      -- ** \<single-animation-iteration-count\>
    , SingleAnimationIterationCount
      -- ** \<single-animation-play-state\>
    , SingleAnimationPlayState
      -- ** \<step-position\>
    , StepPosition
    , StepPositionKeyword

      -- * \<cubic-bezier-easing-function\>
      -- ** ease
    , ease
      -- ** ease-in
    , easeIn
      -- ** ease-in-out
    , easeInOut
      -- ** ease-out
    , easeOut
      -- ** cubic-bezier
    , cubicBezier

      -- * \<easing-function\>
      -- ** linear
    , linear

      -- * \<single-animation-composition\>
      -- ** accumulate
    , accumulate
      -- ** add
    , add
      -- ** replace
    , replace

      -- * \<single-animation-direction\>
      -- ** alternate
    , alternate
      -- ** alternate-reverse
    , alternateReverse
      -- ** reverse
    , reverse

      -- * \<single-animation-fill-mode\>
      -- ** backwards
    , backwards
      -- ** both
    , both
      -- ** forwards
    , forwards

      -- * \<single-animation-iteration-count\>
      -- ** infinite
    , infinite

      -- * \<single-animation-play-state\>
      -- ** paused
    , paused
      -- ** running
    , running

      -- * \<step-easing-function\>
      -- ** step-end
    , stepEnd
      -- ** step-start
    , stepStart
      -- ** steps
    , steps

      -- * \<step-position\>
      -- ** jumpBoth
    , jumpBoth
      -- ** jumpEnd
    , jumpEnd
      -- ** jumpNone
    , jumpNone
      -- ** jumpStart
    , jumpStart
    ) where


import Prelude hiding (reverse)

import Css.Internal
import Css.Keywords           (End, Start)
import Css.DataTypes.Numeric  (Number)
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Buildable(..))


-- * DATA TYPES


newtype EasingFunction = EasingFunction Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<single-animation-composition\>@ data type.
newtype SingleAnimationComposition = SingleAnimationComposition Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<single-animation-direction\>@ data type.
newtype SingleAnimationDirection = SingleAnimationDirection Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<single-animation-fill-mode\>@ data type.
newtype SingleAnimationFillMode = SingleAnimationFillMode Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<single-animation-iteration-count\>@ data type.
newtype SingleAnimationIterationCount = SingleAnimationIterationCount Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<single-animation-play-state\>@ data type.
newtype SingleAnimationPlayState = SingleAnimationPlayState Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<step-position\>@ data type.
class Buildable a => StepPosition a


instance StepPosition End
instance StepPosition Start
instance StepPosition StepPositionKeyword


-- | Represents a CSS keyword in the @\<step-position\>@ data type.
newtype StepPositionKeyword = StepPositionKeyword Builder
    deriving (Buildable, Show)


-- * CUBIC-BEZIER-EASING-FUNCTION


-- | Generates the CSS @ease@ @\<cubic-bezier-easing-function\>@ value.
ease :: EasingFunction
ease = EasingFunction "ease"
{-# INLINE ease #-}


-- | Generates the CSS @ease-in@ @\<cubic-bezier-easing-function\>@ value.
easeIn :: EasingFunction
easeIn = EasingFunction "ease-in"
{-# INLINE easeIn #-}


-- | Generates the CSS @ease-in-out@ @\<cubic-bezier-easing-function\>@ value.
easeInOut :: EasingFunction
easeInOut = EasingFunction "ease-in-out"
{-# INLINE easeInOut #-}


-- | Generates the CSS @ease-out@ @\<cubic-bezier-easing-function\>@ value.
easeOut :: EasingFunction
easeOut = EasingFunction "ease-out"
{-# INLINE easeOut #-}


-- | Generates a CSS @\<cubic-bezier-easing-function\>@ value.
cubicBezier :: (Number a, Number b, Number c, Number d) => a -> b -> c -> d -> EasingFunction
cubicBezier x1 y1 x2 y2
    =  EasingFunction
    $  "cubic-bezier("
    <> build x1
    <> singleton ','
    <> build y1
    <> singleton ','
    <> build x2
    <> singleton ','
    <> build y2
    <> singleton ')'


-- * EASING-FUNCTION


-- | Generates the CSS @linear@ @\<animation-timing-function\>@ value.
linear :: EasingFunction
linear = EasingFunction "linear"
{-# INLINE linear #-}


-- * SINGLE-ANIMATION-COMPOSITION


-- | Generates the CSS @accumulate@ @\<single-animation-composition\>@ value.
accumulate :: SingleAnimationComposition
accumulate = SingleAnimationComposition "accumulate"
{-# INLINE accumulate #-}


-- | Generates the CSS @add@ @\<single-animation-composition\>@ value.
add :: SingleAnimationComposition
add = SingleAnimationComposition "add"
{-# INLINE add #-}


-- | Generates the CSS @replace@ @\<single-animation-composition\>@ value.
replace :: SingleAnimationComposition
replace = SingleAnimationComposition "replace"
{-# INLINE replace #-}


-- * SINGLE-ANIMATION-DIRECTION


-- | Generates the CSS @alternate@ @\<single-animation-direction\>@ value.
alternate :: SingleAnimationDirection
alternate = SingleAnimationDirection "alternate"
{-# INLINE alternate #-}


-- | Generates the CSS @alternate-reverse@ @\<single-animation-direction\>@ value.
alternateReverse :: SingleAnimationDirection
alternateReverse = SingleAnimationDirection "alternate-reverse"
{-# INLINE alternateReverse #-}


-- | Generates the CSS @reverse@ @\<single-animation-direction\>@ value.
reverse :: SingleAnimationDirection
reverse = SingleAnimationDirection "reverse"
{-# INLINE reverse #-}


-- * SINGLE-ANIMATION-FILL-MODE


-- | Generates the CSS @backwards@ @\<single-animation-fill-mode\>@ value.
backwards :: SingleAnimationFillMode
backwards = SingleAnimationFillMode "backwards"
{-# INLINE backwards #-}


-- | Generates the CSS @both@ @\<single-animation-fill-mode\>@ value.
both :: SingleAnimationFillMode
both = SingleAnimationFillMode "both"
{-# INLINE both #-}


-- | Generates the CSS @forwards@ @\<single-animation-fill-mode\>@ value.
forwards :: SingleAnimationFillMode
forwards = SingleAnimationFillMode "forwards"
{-# INLINE forwards #-}


-- * SINGLE-ANIMATION-ITERATION-COUNT


-- | Generates the CSS @infinite@ @\<single-animation-fill-mode\>@ value.
infinite :: SingleAnimationIterationCount
infinite = SingleAnimationIterationCount "infinite"
{-# INLINE infinite #-}


-- * SINGLE-ANIMATION-PLAY-STATE


-- | Generates the CSS @paused@ @\<single-animation-play-state\>@ value.
paused :: SingleAnimationPlayState
paused = SingleAnimationPlayState "paused"
{-# INLINE paused #-}


-- | Generates the CSS @running@ @\<single-animation-play-state\>@ value.
running :: SingleAnimationPlayState
running = SingleAnimationPlayState "running"
{-# INLINE running #-}


-- * STEP-EASING-FUNCTION


-- | Generates the CSS @step-end@ @\<step-easing-function\>@ value.
stepEnd :: EasingFunction
stepEnd = EasingFunction "step-end"
{-# INLINE stepEnd #-}


-- | Generates the CSS @step-start@ @\<step-easing-function\>@ value.
stepStart :: EasingFunction
stepStart = EasingFunction "step-start"
{-# INLINE stepStart #-}


-- | Generates a CSS @\<step-easing-function\>@ value.
steps :: StepPosition a => Integer -> a -> EasingFunction
steps intervalCount stepPosition
    =  EasingFunction
    $  "steps("
    <> build intervalCount
    <> singleton ','
    <> build stepPosition
    <> singleton ')'


-- * STEP-POSITION


-- | Generates the CSS @jump-both@ @\<step-position\>@ value.
jumpBoth :: StepPositionKeyword
jumpBoth = StepPositionKeyword "jump-both"
{-# INLINE jumpBoth #-}


-- | Generates the CSS @jump-end@ @\<step-position\>@ value.
jumpEnd :: StepPositionKeyword
jumpEnd = StepPositionKeyword "jump-end"
{-# INLINE jumpEnd #-}


-- | Generates the CSS @jump-none@ @\<step-position\>@ value.
jumpNone :: StepPositionKeyword
jumpNone = StepPositionKeyword "jump-none"
{-# INLINE jumpNone #-}


-- | Generates the CSS @jump-start@ @\<step-position\>@ value.
jumpStart :: StepPositionKeyword
jumpStart = StepPositionKeyword "jump-start"
{-# INLINE jumpStart #-}
