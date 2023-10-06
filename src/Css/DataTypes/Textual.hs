{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.DataTypes.Textual
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.DataTypes.Textual" module provides a set of types and functions for generating textual data types in CSS.
module Css.DataTypes.Textual
    ( -- * Data Types
      -- ** \<custom-ident\>
      CustomIdent
      -- ** \<string\>
    , String
      -- ** \<url\>
    , Url

      -- * \<custom-ident\>
    , customIdent

      -- * \<string\>
    , string

      -- * \<url\>
      -- ** \<src()\>
    , src
      -- ** \<url()\>
    , url
    ) where


import Prelude hiding (String)

import Css.Internal
import Data.Text.Lazy.Builder (Builder, singleton)
import Html                   (Buildable(..))


-- * DATA TYPES


-- | Represents the CSS @\<custom-ident\>@ data type.
newtype CustomIdent = CustomIdent Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<string\>@ data type.
newtype String = String Builder
    deriving (Buildable, Show)


-- | Represents the CSS @\<url\>@ data type.
newtype Url = Url Builder
    deriving (Buildable, Show)


-- * CUSTOM-IDENT


-- | Generates a CSS @\<custom-ident\>@ value.
customIdent :: Builder -> CustomIdent
customIdent = CustomIdent
{-# INLINE customIdent #-}


-- * STRING


-- | Generates a CSS @\<string\>@ value.
string :: Builder -> String
string value = String $ singleton '"' <> value <> singleton '"'
{-# INLINE string #-}


-- * URL


-- | Generates a CSS @\<src()\>@ value.
src :: Builder -> Url
src value = Url $ "src(\"" <> value <> "\")"
{-# INLINE src #-}


-- | Generates a CSS @\<url()\>@ value.
url :: Builder -> Url
url value = Url $ "url(\"" <> value <> "\")"
{-# INLINE url #-}
