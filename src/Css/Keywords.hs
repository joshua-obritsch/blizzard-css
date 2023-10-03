{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Keywords
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Keywords" module provides a set of functions for generating CSS keywords.
module Css.Keywords
    ( -- * Keywords
      -- ** Auto
      Auto
    , auto
      -- ** Baseline
    , Baseline
    , baseline
      -- ** None
    , None
    , none
      -- ** Normal
    , Normal
    , normal
      -- ** Stretch
    , Stretch
    , stretch
    ) where


import Css.Internal           (lazyShow)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- KEYWORDS


-- | Represents the CSS @auto@ keyword.
newtype Auto = Auto Builder
    deriving (Buildable, Show)


-- | Generates the CSS @auto@ keyword.
auto :: Auto
auto = Auto "auto"


-- | Represents the CSS @baseline@ keyword.
newtype Baseline = Baseline Builder
    deriving (Buildable, Show)


-- | Generates the CSS @baseline@ keyword.
baseline :: Baseline
baseline = Baseline "baseline"
{-# INLINE baseline #-}


-- | Represents the CSS @none@ keyword.
newtype None = None Builder
    deriving (Buildable, Show)


-- | Generates the CSS @none@ keyword.
none :: None
none = None "none"


-- | Represents the CSS @normal@ keyword.
newtype Normal = Normal Builder
    deriving (Buildable, Show)


-- | Generates the CSS @normal@ keyword.
normal :: Normal
normal = Normal "normal"


-- | Represents the CSS @stretch@ keyword.
newtype Stretch = Stretch Builder
    deriving (Buildable, Show)


-- | Generates the CSS @stretch@ keyword.
stretch :: Stretch
stretch = Stretch "stretch"
