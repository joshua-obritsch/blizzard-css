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
    ( -- * Baseline Position
      -- ** \<baseline-position\>
      BaselinePosition
      -- ** \[ first \| last \]
    , FirstLast
      -- ** first
    , first
      -- ** last
    , last

      -- * Content Distribution
      -- ** \<content-distribution\>
    , ContentDistribution
      -- ** \'space-around\'
    , spaceAround
      -- ** \'space-between\'
    , spaceBetween
      -- ** \'space-evenly\'
    , spaceEvenly

      -- * Content Position
      -- ** \<content-position\>
    , ContentPosition
      -- ** \'center\'
    , center
      -- ** \'end\'
    , end
      -- ** \'flex-end\'
    , flexEnd
      -- ** \'flex-start\'
    , flexStart
      -- ** \'start\'
    , start

      -- * Overflow Position
      -- ** \'safe\'
    , safe
      -- ** \'unsafe\'
    , unsafe
    ) where


import Prelude hiding (last)

import Css.Internal
import Css.Keywords           (Baseline, Stretch)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * Baseline Position


-- | Represents the CSS @\<baseline-position\>@ data type.
class Buildable a => BaselinePosition a


instance BaselinePosition Baseline
instance BaselinePosition FirstLast


-- | Represents the CSS @[ first | last ]@ production in @\<baseline-position\>@.
newtype FirstLast = FirstLast Builder
    deriving (Buildable, Show)


-- | Generates the CSS @first@ value in the @[ first | last ]@ production.
first :: Baseline -> FirstLast
first = FirstLast . (<>) "first " . build
{-# INLINE first #-}


-- | Generates the CSS @last@ value in the @[ first | last ]@ production.
last :: Baseline -> FirstLast
last = FirstLast . (<>) "last " . build
{-# INLINE last #-}


-- * Content Distribution


-- | Represents the CSS @\<content-distribution\>@ data type.
newtype ContentDistribution = ContentDistribution Builder
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
stretch :: ContentDistribution
stretch = ContentDistribution "stretch"
{-# INLINE stretch #-}


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


-- | Generates the CSS @safe@ @\<overflow-position\>@ value.
safe :: ContentPosition -> ContentPosition
safe = ContentPosition . (<>) "safe " . build
{-# INLINE safe #-}


-- | Generates the CSS @unsafe@ @\<overflow-position\>@ value.
unsafe :: ContentPosition -> ContentPosition
unsafe = ContentPosition . (<>) "unsafe " . build
{-# INLINE unsafe #-}
