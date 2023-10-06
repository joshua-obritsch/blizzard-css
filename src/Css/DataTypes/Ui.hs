{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Ui
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Ui" module provides a set of types and functions for generating UI-related data types in CSS.
module Css.DataTypes.Ui
    ( -- * Data Types
      -- ** \<compat-auto\>
      CompatAuto
      -- ** \<compat-special\>
    , CompatSpecial

      -- * \<compat-auto\>
      -- ** button
    , button
      -- ** checkbox
    , checkbox
      -- ** listbox
    , listbox
      -- ** menulist
    , menulist
      -- ** meter
    , meter
      -- ** progress-bar
    , progressBar
      -- ** radio
    , radio
      -- ** searchfield
    , searchfield
      -- ** textarea
    , textarea

      -- * \<compat-special\>
      -- ** menulistButton
    , menulistButton
      -- ** textfield
    , textfield
    ) where


import Prelude hiding (String)

import Css.Internal
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<compat-auto\>@ data type.
newtype CompatAuto = CompatAuto Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<compat-special\>@ data type.
newtype CompatSpecial = CompatSpecial Builder
    deriving (Buildable, Show)


-- * COMPAT-AUTO


-- | Generates the CSS @button@ @\<compat-auto\>@ value.
button :: CompatAuto
button = CompatAuto "button"
{-# INLINE button #-}


-- | Generates the CSS @checkbox@ @\<compat-auto\>@ value.
checkbox :: CompatAuto
checkbox = CompatAuto "checkbox"
{-# INLINE checkbox #-}


-- | Generates the CSS @listbox@ @\<compat-auto\>@ value.
listbox :: CompatAuto
listbox = CompatAuto "listbox"
{-# INLINE listbox #-}


-- | Generates the CSS @menulist@ @\<compat-auto\>@ value.
menulist :: CompatAuto
menulist = CompatAuto "menulist"
{-# INLINE menulist #-}


-- | Generates the CSS @meter@ @\<compat-auto\>@ value.
meter :: CompatAuto
meter = CompatAuto "meter"
{-# INLINE meter #-}


-- | Generates the CSS @progress-bar@ @\<compat-auto\>@ value.
progressBar :: CompatAuto
progressBar = CompatAuto "progress-bar"
{-# INLINE progressBar #-}


-- | Generates the CSS @radio@ @\<compat-auto\>@ value.
radio :: CompatAuto
radio = CompatAuto "radio"
{-# INLINE radio #-}


-- | Generates the CSS @searchfield@ @\<compat-auto\>@ value.
searchfield :: CompatAuto
searchfield = CompatAuto "searchfield"
{-# INLINE searchfield #-}


-- | Generates the CSS @textarea@ @\<compat-auto\>@ value.
textarea :: CompatAuto
textarea = CompatAuto "textarea"
{-# INLINE textarea #-}


-- * COMPAT-SPECIAL


-- | Generates the CSS @menulist-button@ @\<compat-special\>@ value.
menulistButton :: CompatAuto
menulistButton = CompatAuto "menulist-button"
{-# INLINE menulistButton #-}


-- | Generates the CSS @textfield@ @\<compat-special\>@ value.
textfield :: CompatAuto
textfield = CompatAuto "textfield"
{-# INLINE textfield #-}
