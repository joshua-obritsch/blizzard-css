{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Background
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Background" module provides a set of types and functions for generating background-related data types in CSS.
module Css.DataTypes.Background
    ( -- * Attachments
      -- ** \<attachment\>
      Attachment
      -- ** fixed
    , fixed
      -- ** local
    , local
      -- ** scroll
    , scroll
    ) where


import Css.Internal
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Buildable(..))


-- * BASELINE-POSITION


-- | Represents the CSS @\<attachment\>@ data type.
newtype Attachment = Attachment Builder
    deriving (Buildable, Show)


-- | Generates the CSS @fixed@ @\<attachment\>@ value.
fixed :: Attachment
fixed = Attachment "fixed"
{-# INLINE fixed #-}


-- | Generates the CSS @local@ @\<attachment\>@ value.
local :: Attachment
local = Attachment "local"
{-# INLINE local #-}


-- | Generates the CSS @scroll@ @\<attachment\>@ value.
scroll :: Attachment
scroll = Attachment "scroll"
{-# INLINE scroll #-}


-- * REPEAT-STYLE


class Buildable a => RepeatStyle2 a


instance RepeatStyle2 RepeatStyle
instance RepeatStyle2 (RepeatStyle, RepeatStyle)


-- | Represents the CSS @\<repeat-style\>@ data type.
newtype RepeatStyle = RepeatStyle Builder
    deriving (Buildable, Show)


-- | Generates the CSS @no-repeat@ @\<repeat-style\>@ value.
noRepeat :: RepeatStyle
noRepeat = RepeatStyle "no-repeat"
{-# INLINE noRepeat #-}


-- | Generates the CSS @repeat@ @\<repeat-style\>@ value.
repeat :: RepeatStyle
repeat = RepeatStyle "repeat"
{-# INLINE repeat #-}


-- | Generates the CSS @round@ @\<repeat-style\>@ value.
round :: RepeatStyle
round = RepeatStyle "round"
{-# INLINE round #-}


-- | Generates the CSS @space@ @\<repeat-style\>@ value.
space :: RepeatStyle
space = RepeatStyle "space"
{-# INLINE space #-}
