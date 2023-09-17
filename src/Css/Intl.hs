-- | Module    : Css.Intl
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Intl" module provides a set of functions for internationalization and merging attributes in HTML.
module Css.Intl
    ( -- * Internationalization
      -- ** translate
      translate
    ) where


import Css.Internal           (extractAndHashIntl)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Html, Translatable)


-- INTERNATIONALIZATION


-- | Translates all multilingual HTML text nodes and converts 'Html.Html' to 'Data.Text.Lazy.Builder.Builder'.
--
-- Corresponds to 'Html.Intl.translate' in /blizzard-html/. Also hashes and builds CSS.
translate :: Translatable a => (a -> Builder) -> Html a -> Builder
translate = extractAndHashIntl
