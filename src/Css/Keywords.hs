{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Keywords
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Keywords" module provides a set of functions for generating CSS keywords.
module Css.Keywords
    ( -- * Types
      -- ** Auto
      Auto
      -- ** None
    , None

      -- * Keywords
    , auto
    , none
    ) where


import Css.Internal           (lazyShow)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- TYPES


-- | Represents the CSS @none@ keyword.
newtype Auto = Auto { unAuto :: Builder }


instance Buildable Auto where build = unAuto
instance Show      Auto where show  = lazyShow


-- | Represents the CSS @none@ keyword.
newtype None = None { unNone :: Builder }


instance Buildable None where build = unNone
instance Show      None where show  = lazyShow


-- KEYWORDS


-- | Generates the CSS @auto@ keyword.
auto :: Auto
auto = Auto "auto"


-- | Generates the CSS @none@ keyword.
none :: None
none = None "none"
