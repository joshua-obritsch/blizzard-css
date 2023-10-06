{-# LANGUAGE FlexibleInstances #-}

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
    , module Css.DataTypes.Animation
    , module Css.DataTypes.Alignment
    , module Css.DataTypes.Background
    , module Css.DataTypes.Color
    , module Css.DataTypes.Filter
    , module Css.DataTypes.Fonts
    , module Css.DataTypes.Numeric
    , module Css.DataTypes.Textual
    , module Css.DataTypes.Ui
    , module Css.Keywords
    , module Css.Properties
    ) where


import Css.DataTypes.Animation  hiding (reverse)
import Css.DataTypes.Alignment  hiding (last)
import Css.DataTypes.Background
import Css.DataTypes.Color      hiding (color, accentColor, tan)
import Css.DataTypes.Filter     hiding (opacity)
import Css.DataTypes.Fonts
import Css.DataTypes.Numeric    hiding (rem)
import Css.DataTypes.Textual
import Css.DataTypes.Ui         hiding (button, meter, textarea)
import Css.Keywords
import Css.Properties           hiding (all, filter)

import Css.Internal                     (extractAndHash)
import Data.Text.Lazy.Builder           (Builder)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Html                             (Html)


-- CONSTRUCTORS


build :: Html lng -> Builder
build = extractAndHash
