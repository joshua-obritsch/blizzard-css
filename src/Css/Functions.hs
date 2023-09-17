{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Functions
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Functions" module provides a set of functions that correspond to CSS functions.
module Css.Functions
    ( -- * Functions
      -- ** attr
      attr
    ) where


import Data.Text.Lazy.Builder (Builder, singleton)


attr :: Builder -> Builder
attr value = "attr(" <> value <> singleton ')'


calc :: Builder -> Builder
calc value = "calc(" <> value <> singleton ')'
