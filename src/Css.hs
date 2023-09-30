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
    , module Css.Functions
    , module Css.DataTypes.Colors
    , module Css.DataTypes.Numeric
    , module Css.Keywords
    , module Css.Properties
    , module Css.Values
    ) where


import Css.Functions
import Css.DataTypes.Colors   hiding (color, accentColor, tan)
import Css.DataTypes.Numeric  hiding (rem)
import Css.Keywords
import Css.Properties         hiding (all, filter)
import Css.Values             hiding (color)

import Css.Internal           (extractAndHash)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Html)


-- CONSTRUCTORS


build :: Html lng -> Builder
build = extractAndHash
