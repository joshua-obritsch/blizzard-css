{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Fonts
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Fonts" module provides a set of types and functions for generating font-related data types in CSS.
module Css.DataTypes.Fonts
    ( -- * Absolute Sizes
      -- ** \<absolute-size\>
      AbsoluteSize
      -- ** \'large\'
    , large
      -- ** \'medium\'
    , medium
      -- ** \'small\'
    , small
      -- ** \'x-large\'
    , xLarge
      -- ** \'x-small\'
    , xSmall
      -- ** \'xx-large\'
    , xxLarge
      -- ** \'xx-small\'
    , xxSmall
      -- ** \'xxx-large\'
    , xxxLarge
    ) where


import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * ABSOLUTE SIZES


-- | Represents the CSS @\<absolute-size\>@ data type.
newtype AbsoluteSize = AbsoluteSize Builder
    deriving (Buildable, Show)


-- | Generates the CSS @large@ @\<absolute-size\>@ value.
large :: AbsoluteSize
large = AbsoluteSize "large"
{-# INLINE large #-}


-- | Generates the CSS @medium@ @\<absolute-size\>@ value.
medium :: AbsoluteSize
medium = AbsoluteSize "medium"
{-# INLINE medium #-}


-- | Generates the CSS @small@ @\<absolute-size\>@ value.
small :: AbsoluteSize
small = AbsoluteSize "small"
{-# INLINE small #-}


-- | Generates the CSS @x-large@ @\<absolute-size\>@ value.
xLarge :: AbsoluteSize
xLarge = AbsoluteSize "x-large"
{-# INLINE xLarge #-}


-- | Generates the CSS @x-small@ @\<absolute-size\>@ value.
xSmall :: AbsoluteSize
xSmall = AbsoluteSize "x-small"
{-# INLINE xSmall #-}


-- | Generates the CSS @xx-large@ @\<absolute-size\>@ value.
xxLarge :: AbsoluteSize
xxLarge = AbsoluteSize "xx-large"
{-# INLINE xxLarge #-}


-- | Generates the CSS @xx-small@ @\<absolute-size\>@ value.
xxSmall :: AbsoluteSize
xxSmall = AbsoluteSize "xx-small"
{-# INLINE xxSmall #-}


-- | Generates the CSS @xxx-large@ @\<absolute-size\>@ value.
xxxLarge :: AbsoluteSize
xxxLarge = AbsoluteSize "xxx-large"
{-# INLINE xxxLarge #-}
