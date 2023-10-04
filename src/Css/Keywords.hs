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
      -- ** Inherit
    , Inherit
    , inherit
      -- ** Initial
    , Initial
    , initial
      -- ** None
    , None
    , none
      -- ** Normal
    , Normal
    , normal
      -- ** Revert
    , Revert
    , revert
      -- ** Unset
    , Unset
    , unset
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


-- | Represents the CSS @inherit@ keyword.
newtype Inherit = Inherit Builder
    deriving (Buildable, Show)


-- | Generates the CSS @inhert@ keyword.
inherit :: Inherit
inherit = Inherit "inherit"


-- | Represents the CSS @initial@ keyword.
newtype Initial = Initial Builder
    deriving (Buildable, Show)


-- | Generates the CSS @initial@ keyword.
initial :: Initial
initial = Initial "initial"


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


-- | Represents the CSS @revert@ keyword.
newtype Revert = Revert Builder
    deriving (Buildable, Show)


-- | Generates the CSS @revert@ keyword.
revert :: Revert
revert = Revert "revert"


-- | Represents the CSS @unset@ keyword.
newtype Unset = Unset Builder
    deriving (Buildable, Show)


-- | Generates the CSS @unset@ keyword.
unset :: Unset
unset = Unset "unset"
