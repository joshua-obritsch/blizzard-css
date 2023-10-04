{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Module    : Css.DataTypes.Textual
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Textual" module provides a set of types and functions for generating textual data types in CSS.
module Css.DataTypes.Textual
    ( -- * Identifiers
      -- ** \<custom-ident\>
      CustomIdent
    , customIdent

      -- * Quoted Strings
      -- ** \<string\>
    , String
    , string
    ) where


import Prelude hiding (String)

import Css.Internal
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Buildable(..))


-- * Identifiers


-- | Represents the CSS @\<custom-ident\>@ data type.
newtype CustomIdent = CustomIdent Builder
    deriving (Buildable, Show)


-- | Generates a CSS @\<custom-ident\>@ value.
customIdent :: Builder -> CustomIdent
customIdent = CustomIdent
{-# INLINE customIdent #-}


-- * Quoted Strings


-- | Represents the CSS @\<string\>@ data type.
newtype String = String Builder
    deriving (Buildable, Show)


-- | Generates a CSS @\<string\>@ value.
string :: Builder -> String
string value = String $ singleton '"' <> value <> singleton '"'
{-# INLINE string #-}
