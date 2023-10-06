{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Keywords
-- Copyright   : (c) Joshua Obritsch, 2022
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Keywords" module provides a set of functions for generating CSS keywords.
module Css.Keywords
    ( -- * Keywords
      -- ** auto
      Auto
    , auto
      -- ** border-box
    , BorderBox
    , borderBox
      -- ** content-box
    , ContentBox
    , contentBox
      -- ** end
    , End
    , end
      -- ** hidden
    , Hidden
    , hidden
      -- ** inherit
    , Inherit
    , inherit
      -- ** initial
    , Initial
    , initial
      -- ** none
    , None
    , none
      -- ** normal
    , Normal
    , normal
      -- ** padding-box
    , PaddingBox
    , paddingBox
      -- ** revert
    , Revert
    , revert
      -- ** start
    , Start
    , start
      -- ** unset
    , Unset
    , unset
      -- ** visible
    , Visible
    , visible
    ) where


import Css.Internal           (lazyShow)
import Data.Text.Lazy.Builder (Builder)
import Html                   (Buildable(..))


-- KEYWORDS


-- | Represents the CSS @auto@ keyword.
newtype Auto = Auto Builder
    deriving (Buildable, Show)


-- | Generates the CSS @auto@ keyword.
auto :: Auto
auto = Auto "auto"


-- | Represents the CSS @border-box@ keyword.
newtype BorderBox = BorderBox Builder
    deriving (Buildable, Show)


-- | Generates the CSS @border-box@ keyword.
borderBox :: BorderBox
borderBox = BorderBox "border-box"


-- | Represents the CSS @content-box@ keyword.
newtype ContentBox = ContentBox Builder
    deriving (Buildable, Show)


-- | Generates the CSS @content-box@ keyword.
contentBox :: ContentBox
contentBox = ContentBox "content-box"


-- | Represents the CSS @end@ keyword.
newtype End = End Builder
    deriving (Buildable, Show)


-- | Generates the CSS @end@ keyword.
end :: End
end = End "end"


-- | Represents the CSS @hidden@ keyword.
newtype Hidden = Hidden Builder
    deriving (Buildable, Show)


-- | Generates the CSS @hidden@ keyword.
hidden :: Hidden
hidden = Hidden "hidden"


-- | Represents the CSS @inherit@ keyword.
newtype Inherit = Inherit Builder
    deriving (Buildable, Show)


-- | Generates the CSS @inhert@ keyword.
inherit :: Inherit
inherit = Inherit "inherit"


-- | Represents the CSS @initial@ keyword.
newtype Initial = Initial Builder
    deriving (Buildable, Show)


-- | Generates the CSS @initial@ keyword.
initial :: Initial
initial = Initial "initial"


-- | Represents the CSS @none@ keyword.
newtype None = None Builder
    deriving (Buildable, Show)


-- | Generates the CSS @none@ keyword.
none :: None
none = None "none"


-- | Represents the CSS @normal@ keyword.
newtype Normal = Normal Builder
    deriving (Buildable, Show)


-- | Generates the CSS @normal@ keyword.
normal :: Normal
normal = Normal "normal"


-- | Represents the CSS @padding-box@ keyword.
newtype PaddingBox = PaddingBox Builder
    deriving (Buildable, Show)


-- | Generates the CSS @padding-box@ keyword.
paddingBox :: PaddingBox
paddingBox = PaddingBox "padding-box"


-- | Represents the CSS @revert@ keyword.
newtype Revert = Revert Builder
    deriving (Buildable, Show)


-- | Generates the CSS @revert@ keyword.
revert :: Revert
revert = Revert "revert"


-- | Represents the CSS @start@ keyword.
newtype Start = Start Builder
    deriving (Buildable, Show)


-- | Generates the CSS @start@ keyword.
start :: Start
start = Start "start"


-- | Represents the CSS @unset@ keyword.
newtype Unset = Unset Builder
    deriving (Buildable, Show)


-- | Generates the CSS @unset@ keyword.
unset :: Unset
unset = Unset "unset"


-- | Represents the CSS @visible@ keyword.
newtype Visible = Visible Builder
    deriving (Buildable, Show)


-- | Generates the CSS @visible@ keyword.
visible :: Visible
visible = Visible "visible"
