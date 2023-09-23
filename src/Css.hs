-- | Module    : Css
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css" module provides a set of functions for generating CSS.
module Css
    ( -- * Constructors
      -- ** build
      build
    , module Css.Colors
    , module Css.Dimension
    , module Css.Functions
    , module Css.Percentage
    , module Css.Properties
    , module Css.Values
    ) where


import Css.Colors     hiding (tan)
import Css.Dimension  hiding (rem)
import Css.Functions
import Css.Percentage
import Css.Properties hiding (all, filter)
import Css.Values     hiding (color)

import Css.Internal           (extractAndHash)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Html)


-- CONSTRUCTORS


build :: Html lng -> Builder
build = extractAndHash
