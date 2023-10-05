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
    ( -- * Single Animation Composition
      -- ** \<single-animation-composition\>
      SingleAnimationComposition
      -- ** accumulate
    , accumulate
      -- ** add
    , add
      -- ** replace
    , replace

      -- * Single Animation Direction
      -- ** \<single-animation-direction\>
    , SingleAnimationDirection
      -- ** alternate
    , alternate
      -- ** alternate-reverse
    , alternateReverse
      -- ** reverse
    , reverse

      -- * Single Animation Fill Mode
      -- ** \<single-animation-fill-mode\>
    , SingleAnimationFillMode
      -- ** backwards
    , backwards
      -- ** both
    , both
      -- ** forwards
    , forwards

      -- * Single Animation Iteration Count
      -- ** \<single-animation-iteration-count\>
    , SingleAnimationIterationCount
      -- ** infinite
    , infinite

      -- * Single Animation Play State
      -- ** \<single-animation-play-state\>
    , SingleAnimationPlayState
      -- ** paused
    , paused
      -- ** running
    , running
    ) where


import Prelude hiding (reverse)

import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * Single Animation Composition


-- | Represents the CSS @\<single-animation-composition\>@ data type.
newtype SingleAnimationComposition = SingleAnimationComposition Builder
    deriving (Buildable, Show)


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


-- * Single Animation Direction


-- | Represents the CSS @\<single-animation-direction\>@ data type.
newtype SingleAnimationDirection = SingleAnimationDirection Builder
    deriving (Buildable, Show)


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


-- * Single Animation Fill Mode


-- | Represents the CSS @\<single-animation-fill-mode\>@ data type.
newtype SingleAnimationFillMode = SingleAnimationFillMode Builder
    deriving (Buildable, Show)


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


-- * Single Animation Iteration Count


-- | Represents the CSS @\<single-animation-iteration-count\>@ data type.
newtype SingleAnimationIterationCount = SingleAnimationIterationCount Builder
    deriving (Buildable, Show)


-- | Generates the CSS @infinite@ @\<single-animation-fill-mode\>@ value.
infinite :: SingleAnimationIterationCount
infinite = SingleAnimationIterationCount "infinite"
{-# INLINE infinite #-}


-- * Single Animation Play State


-- | Represents the CSS @\<single-animation-play-state\>@ data type.
newtype SingleAnimationPlayState = SingleAnimationPlayState Builder
    deriving (Buildable, Show)


-- | Generates the CSS @paused@ @\<single-animation-play-state\>@ value.
paused :: SingleAnimationPlayState
paused = SingleAnimationPlayState "paused"
{-# INLINE paused #-}


-- | Generates the CSS @running@ @\<single-animation-play-state\>@ value.
running :: SingleAnimationPlayState
running = SingleAnimationPlayState "running"
{-# INLINE running #-}
