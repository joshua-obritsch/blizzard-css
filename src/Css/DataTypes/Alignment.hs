{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module    : Css.DataTypes.Alignment
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Alignment" module provides a set of types and functions for generating alignment-related data types in CSS.
module Css.DataTypes.Alignment
    ( -- * Baseline Position
      -- ** \<baseline-position\>
      BaselinePosition
      -- ** baseline
    , baseline
      -- ** first baseline
    , firstBaseline
      -- ** last baseline
    , lastBaseline

      -- * Content Distribution
      -- ** \<content-distribution\>
    , ContentDistribution
    , Stretch
      -- ** space-around
    , spaceAround
      -- ** space-between
    , spaceBetween
      -- ** space-evenly
    , spaceEvenly
      -- ** stretch
    , stretch

      -- * Content Position
      -- ** \<content-position\>
    , ContentPosition
      -- ** center
    , center
      -- ** end
    , end
      -- ** flex-end
    , flexEnd
      -- ** flex-start
    , flexStart
      -- ** start
    , start

      -- * Overflow Position
      -- ** \<overflow-position\>
    , OverflowPosition
    , SafeSelfEnd
    , SafeSelfStart
    , UnsafeSelfEnd
    , UnsafeSelfStart
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

      -- * Self Position
      -- ** \<self-position\>
    , SelfPosition
      -- ** self-end
    , selfEnd
      -- ** self-start
    , selfStart
    ) where


import Prelude hiding (last)

import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * Baseline Position


-- | Represents the CSS @\<baseline-position\>@ data type.
newtype BaselinePosition = BaselinePosition Builder
    deriving (Buildable, Show)


-- | Generates the CSS @baseline@ keyword.
baseline :: BaselinePosition
baseline = BaselinePosition "baseline"
{-# INLINE baseline #-}


-- | Generates the CSS @first baseline@ @\<baseline-position\>@ value.
firstBaseline :: BaselinePosition
firstBaseline = BaselinePosition "first baseline"
{-# INLINE firstBaseline #-}


-- | Generates the CSS @last baseline@ @\<baseline-position\>@ value.
lastBaseline :: BaselinePosition
lastBaseline = BaselinePosition "last baseline"
{-# INLINE lastBaseline #-}


-- * Content Distribution


-- | Represents the CSS @\<content-distribution\>@ data type.
newtype ContentDistribution = ContentDistribution Builder
    deriving (Buildable, Show)


-- | Represents the CSS @stretch@ @\<content-distribution\>@ value.
newtype Stretch = Stretch Builder
    deriving (Buildable, Show)


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
stretch :: Stretch
stretch = Stretch "stretch"


-- * Content Position


-- | Represents the CSS @\<content-position\>@ data type.
newtype ContentPosition = ContentPosition Builder
    deriving (Buildable, Show)


-- | Generates the CSS @center@ @\<content-position\>@ value.
center :: ContentPosition
center = ContentPosition "center"
{-# INLINE center #-}


-- | Generates the CSS @end@ @\<content-position\>@ value.
end :: ContentPosition
end = ContentPosition "end"
{-# INLINE end #-}


-- | Generates the CSS @flex-end@ @\<content-position\>@ value.
flexEnd :: ContentPosition
flexEnd = ContentPosition "flex-end"
{-# INLINE flexEnd #-}


-- | Generates the CSS @flex-start@ @\<content-position\>@ value.
flexStart :: ContentPosition
flexStart = ContentPosition "flex-start"
{-# INLINE flexStart #-}


-- | Generates the CSS @start@ @\<content-position\>@ value.
start :: ContentPosition
start = ContentPosition "start"
{-# INLINE start #-}


-- * Overflow Position


-- | Represents the CSS @\<overflow-position\>@ data type.
newtype OverflowPosition = OverflowPosition Builder
    deriving (Buildable, Show)


-- | Represents the CSS @safe self-end@ @\<overflow-position\>@ value.
newtype SafeSelfEnd = SafeSelfEnd Builder
    deriving (Buildable, Show)


-- | Represents the CSS @safe self-start@ @\<overflow-position\>@ value.
newtype SafeSelfStart = SafeSelfStart Builder
    deriving (Buildable, Show)


-- | Represents the CSS @unsafe self-end@ @\<overflow-position\>@ value.
newtype UnsafeSelfEnd = UnsafeSelfEnd Builder
    deriving (Buildable, Show)


-- | Represents the CSS @unsafe self-start@ @\<overflow-position\>@ value.
newtype UnsafeSelfStart = UnsafeSelfStart Builder
    deriving (Buildable, Show)


-- | Generates the CSS @safe center@ @\<overflow-position\>@ value.
safeCenter :: OverflowPosition
safeCenter = OverflowPosition "safe center"
{-# INLINE safeCenter #-}


-- | Generates the CSS @safe end@ @\<overflow-position\>@ value.
safeEnd :: OverflowPosition
safeEnd = OverflowPosition "safe end"
{-# INLINE safeEnd #-}


-- | Generates the CSS @safe flex-end@ @\<overflow-position\>@ value.
safeFlexEnd :: OverflowPosition
safeFlexEnd = OverflowPosition "safe flex-end"
{-# INLINE safeFlexEnd #-}


-- | Generates the CSS @safe flex-start@ @\<overflow-position\>@ value.
safeFlexStart :: OverflowPosition
safeFlexStart = OverflowPosition "safe flex-start"
{-# INLINE safeFlexStart #-}


-- | Generates the CSS @safe self-end@ @\<overflow-position\>@ value.
safeSelfEnd :: SafeSelfEnd
safeSelfEnd = SafeSelfEnd "safe self-end"
{-# INLINE safeSelfEnd #-}


-- | Generates the CSS @safe self-start@ @\<overflow-position\>@ value.
safeSelfStart :: SafeSelfStart
safeSelfStart = SafeSelfStart "safe self-start"
{-# INLINE safeSelfStart #-}


-- | Generates the CSS @safe start@ @\<overflow-position\>@ value.
safeStart :: OverflowPosition
safeStart = OverflowPosition "safe start"
{-# INLINE safeStart #-}


-- | Generates the CSS @unsafe center@ @\<overflow-position\>@ value.
unsafeCenter :: OverflowPosition
unsafeCenter = OverflowPosition "unsafe center"
{-# INLINE unsafeCenter #-}


-- | Generates the CSS @unsafe end@ @\<overflow-position\>@ value.
unsafeEnd :: OverflowPosition
unsafeEnd = OverflowPosition "unsafe end"
{-# INLINE unsafeEnd #-}


-- | Generates the CSS @unsafe flex-end@ @\<overflow-position\>@ value.
unsafeFlexEnd :: OverflowPosition
unsafeFlexEnd = OverflowPosition "unsafe flex-end"
{-# INLINE unsafeFlexEnd #-}


-- | Generates the CSS @unsafe flex-start@ @\<overflow-position\>@ value.
unsafeFlexStart :: OverflowPosition
unsafeFlexStart = OverflowPosition "unsafe flex-start"
{-# INLINE unsafeFlexStart #-}


-- | Generates the CSS @unsafe self-end@ @\<overflow-position\>@ value.
unsafeSelfEnd :: UnsafeSelfEnd
unsafeSelfEnd = UnsafeSelfEnd "unsafe self-end"
{-# INLINE unsafeSelfEnd #-}


-- | Generates the CSS @unsafe self-start@ @\<overflow-position\>@ value.
unsafeSelfStart :: UnsafeSelfStart
unsafeSelfStart = UnsafeSelfStart "unsafe self-start"
{-# INLINE unsafeSelfStart #-}


-- | Generates the CSS @unsafe start@ @\<overflow-position\>@ value.
unsafeStart :: OverflowPosition
unsafeStart = OverflowPosition "unsafe start"
{-# INLINE unsafeStart #-}


-- * Self Position


-- | Represents the CSS @\<self-position\>@ data type.
newtype SelfPosition = SelfPosition Builder
    deriving (Buildable, Show)


-- | Generates the CSS @self-end@ @\<self-position\>@ value.
selfEnd :: SelfPosition
selfEnd = SelfPosition "self-end"
{-# INLINE selfEnd #-}


-- | Generates the CSS @self-start@ @\<self-position\>@ value.
selfStart :: SelfPosition
selfStart = SelfPosition "self-start"
{-# INLINE selfStart #-}
