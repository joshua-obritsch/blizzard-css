{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Intl
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Intl" module provides a set of functions for internationalization and merging attributes in HTML.
module Css.Attributes
    ( -- * Attributes
      -- ** css
      css
    ) where


import Data.Foldable          (fold)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Attribute(..))


-- ATTRIBUTES


-- | Generates a CSS hash for the HTML @class@ attribute from the given value.
css :: [Builder] -> Attribute
css = TextAttribute " css=\"" . fold
