{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Alignment
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Alignment" module provides a set of types and functions for generating alignment-related data types in CSS.
module Css.DataTypes.Alignment
    ( -- * Data Types
      -- ** \<align-*\>
      Align
      -- ** \<content-distribution\>
    , ContentDistribution
      -- ** \<self-position\>
    , SelfPosition

      -- * \<baseline-position\>
      -- ** baseline
    , baseline
      -- ** first baseline
    , firstBaseline
      -- ** last baseline
    , lastBaseline

      -- * \<content-distribution\>
      -- ** space-around
    , spaceAround
      -- ** space-between
    , spaceBetween
      -- ** space-evenly
    , spaceEvenly
      -- ** stretch
    , stretch

      -- * \<content-position\>
      -- ** center
    , center
      -- ** flex-end
    , flexEnd
      -- ** flex-start
    , flexStart

      -- * \<overflow-position\>
      -- ** safe center
    , safeCenter
      -- ** safe end
    , safeEnd
      -- ** safe flex-end
    , safeFlexEnd
      -- ** safe flex-start
    , safeFlexStart
      -- ** safe self-end
    , safeSelfEnd
      -- ** safe self-start
    , safeSelfStart
      -- ** safe start
    , safeStart
      -- ** unsafe center
    , unsafeCenter
      -- ** unsafe end
    , unsafeEnd
      -- ** unsafe flex-end
    , unsafeFlexEnd
      -- ** unsafe flex-start
    , unsafeFlexStart
      -- ** unsafe self-end
    , unsafeSelfEnd
      -- ** unsafe self-start
    , unsafeSelfStart
      -- ** unsafe start
    , unsafeStart

      -- * \<self-position\>
      -- ** self-end
    , selfEnd
      -- ** self-start
    , selfStart
    ) where


import Prelude hiding (last)

import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<align-*\>@ data type.
newtype Align = Align Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<content-distribution\>@ data type.
newtype ContentDistribution = ContentDistribution Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<self-position\>@ data type.
newtype SelfPosition = SelfPosition Builder
    deriving (Buildable, Show)


-- * BASELINE-POSITION


-- | Generates the CSS @baseline@ @\<baseline-position\>@ value.
baseline :: Align
baseline = Align "baseline"
{-# INLINE baseline #-}


-- | Generates the CSS @first baseline@ @\<baseline-position\>@ value.
firstBaseline :: Align
firstBaseline = Align "first baseline"
{-# INLINE firstBaseline #-}


-- | Generates the CSS @last baseline@ @\<baseline-position\>@ value.
lastBaseline :: Align
lastBaseline = Align "last baseline"
{-# INLINE lastBaseline #-}


-- * CONTENT-DISTRIBUTION


-- | Generates the CSS @space-around@ @\<content-distribution\>@ value.
spaceAround :: ContentDistribution
spaceAround = ContentDistribution "space-around"
{-# INLINE spaceAround #-}


-- | Generates the CSS @space-between@ @\<content-distribution\>@ value.
spaceBetween :: ContentDistribution
spaceBetween = ContentDistribution "space-between"
{-# INLINE spaceBetween #-}


-- | Generates the CSS @space-evenly@ @\<content-distribution\>@ value.
spaceEvenly :: ContentDistribution
spaceEvenly = ContentDistribution "space-evenly"
{-# INLINE spaceEvenly #-}


-- | Generates the CSS @stretch@ @\<content-distribution\>@ value.
stretch :: Align
stretch = Align "stretch"


-- * CONTENT-POSITION


-- | Generates the CSS @center@ @\<content-position\>@ value.
center :: Align
center = Align "center"
{-# INLINE center #-}


-- | Generates the CSS @flex-end@ @\<content-position\>@ value.
flexEnd :: Align
flexEnd = Align "flex-end"
{-# INLINE flexEnd #-}


-- | Generates the CSS @flex-start@ @\<content-position\>@ value.
flexStart :: Align
flexStart = Align "flex-start"
{-# INLINE flexStart #-}


-- * OVERFLOW-POSITION


-- | Generates the CSS @safe center@ @\<overflow-position\>@ value.
safeCenter :: Align
safeCenter = Align "safe center"
{-# INLINE safeCenter #-}


-- | Generates the CSS @safe end@ @\<overflow-position\>@ value.
safeEnd :: Align
safeEnd = Align "safe end"
{-# INLINE safeEnd #-}


-- | Generates the CSS @safe flex-end@ @\<overflow-position\>@ value.
safeFlexEnd :: Align
safeFlexEnd = Align "safe flex-end"
{-# INLINE safeFlexEnd #-}


-- | Generates the CSS @safe flex-start@ @\<overflow-position\>@ value.
safeFlexStart :: Align
safeFlexStart = Align "safe flex-start"
{-# INLINE safeFlexStart #-}


-- | Generates the CSS @safe self-end@ @\<overflow-position\>@ value.
safeSelfEnd :: SelfPosition
safeSelfEnd = SelfPosition "safe self-end"
{-# INLINE safeSelfEnd #-}


-- | Generates the CSS @safe self-start@ @\<overflow-position\>@ value.
safeSelfStart :: SelfPosition
safeSelfStart = SelfPosition "safe self-start"
{-# INLINE safeSelfStart #-}


-- | Generates the CSS @safe start@ @\<overflow-position\>@ value.
safeStart :: Align
safeStart = Align "safe start"
{-# INLINE safeStart #-}


-- | Generates the CSS @unsafe center@ @\<overflow-position\>@ value.
unsafeCenter :: Align
unsafeCenter = Align "unsafe center"
{-# INLINE unsafeCenter #-}


-- | Generates the CSS @unsafe end@ @\<overflow-position\>@ value.
unsafeEnd :: Align
unsafeEnd = Align "unsafe end"
{-# INLINE unsafeEnd #-}


-- | Generates the CSS @unsafe flex-end@ @\<overflow-position\>@ value.
unsafeFlexEnd :: Align
unsafeFlexEnd = Align "unsafe flex-end"
{-# INLINE unsafeFlexEnd #-}


-- | Generates the CSS @unsafe flex-start@ @\<overflow-position\>@ value.
unsafeFlexStart :: Align
unsafeFlexStart = Align "unsafe flex-start"
{-# INLINE unsafeFlexStart #-}


-- | Generates the CSS @unsafe self-end@ @\<overflow-position\>@ value.
unsafeSelfEnd :: SelfPosition
unsafeSelfEnd = SelfPosition "unsafe self-end"
{-# INLINE unsafeSelfEnd #-}


-- | Generates the CSS @unsafe self-start@ @\<overflow-position\>@ value.
unsafeSelfStart :: SelfPosition
unsafeSelfStart = SelfPosition "unsafe self-start"
{-# INLINE unsafeSelfStart #-}


-- | Generates the CSS @unsafe start@ @\<overflow-position\>@ value.
unsafeStart :: Align
unsafeStart = Align "unsafe start"
{-# INLINE unsafeStart #-}


-- * SELF-POSITION


-- | Generates the CSS @self-end@ @\<self-position\>@ value.
selfEnd :: SelfPosition
selfEnd = SelfPosition "self-end"
{-# INLINE selfEnd #-}


-- | Generates the CSS @self-start@ @\<self-position\>@ value.
selfStart :: SelfPosition
selfStart = SelfPosition "self-start"
{-# INLINE selfStart #-}
