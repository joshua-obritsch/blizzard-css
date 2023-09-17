{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Css.Internal
-- Copyright   : (c) Joshua Obritsch, 2021
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Css.Internal" module provides a set of utility functions for hashing and building CSS.
module Css.Internal
    ( -- * Utilities
      -- ** extractAndHash
      extractAndHash
      -- ** extractAndHashIntl
    , extractAndHashIntl
      -- ** prop
    , prop
      -- ** props
    , props
    ) where


import Prelude hiding (compare, foldl)

import Data.Bits                  (xor)
import Data.Foldable              (fold)
import Data.List                  (intersperse, partition)
import Data.Map                   (Map, empty, foldrWithKey, insert)
import Data.Text.Lazy             (Text, foldl)
import Data.Text.Lazy.Builder     (Builder, singleton, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Html                       (Html(..), Attribute(..), Buildable(..), Translatable(..))


-- UTILITIES


extractAndHash :: Html lng -> Builder
extractAndHash html = buildWithMap map html'
  where
    (map, html') = extractWithMap empty html


extractAndHashIntl :: Translatable a => (a -> Builder) -> Html a -> Builder
extractAndHashIntl lang html = buildIntlWithMap lang map html'
  where
    (map, html') = extractWithMap empty html


prop :: Builder -> Builder -> Builder
prop key value = key <> value <> singleton ';'


props :: Builder -> [Builder] -> Builder
props key value = key <> value' <> singleton ';'
  where
    value' = fold $ intersperse (singleton ' ') value


buildWithMap :: Map Builder Builder -> Html lng -> Builder
buildWithMap map html = case html of
    ParentNode "<head"  "</head>" attributes children -> buildHead map attributes children
    ParentNode startTag endTag    []         []       -> startTag <>                     singleton '>' <>                    endTag
    ParentNode startTag endTag    attributes []       -> startTag <> build attributes <> singleton '>' <>                    endTag
    ParentNode startTag endTag    []         children -> startTag <>                     singleton '>' <> build' children <> endTag
    ParentNode startTag endTag    attributes children -> startTag <> build attributes <> singleton '>' <> build' children <> endTag
    LeafNode   startTag           []                  -> startTag <>                     singleton '>'
    LeafNode   startTag           attributes          -> startTag <> build attributes <> singleton '>'
    RootNode   startTag                      []       -> startTag
    RootNode   startTag                      children -> startTag <>                                      build' children
    TextNode   text                                   -> text
    IntlNode   intl                                   -> text
      where text = defaultLanguage intl
  where
    build' = foldr ((<>) . buildWithMap map) mempty


buildIntlWithMap :: Translatable a => (a -> Builder) -> Map Builder Builder -> Html a -> Builder
buildIntlWithMap lang map html = case html of
    ParentNode "<head"  "</head>" attributes children -> buildHead map attributes children
    ParentNode startTag endTag    []         []       -> startTag <>                     singleton '>' <>                    endTag
    ParentNode startTag endTag    attributes []       -> startTag <> build attributes <> singleton '>' <>                    endTag
    ParentNode startTag endTag    []         children -> startTag <>                     singleton '>' <> build' children <> endTag
    ParentNode startTag endTag    attributes children -> startTag <> build attributes <> singleton '>' <> build' children <> endTag
    LeafNode   startTag           []                  -> startTag <>                     singleton '>'
    LeafNode   startTag           attributes          -> startTag <> build attributes <> singleton '>'
    RootNode   startTag                      []       -> startTag
    RootNode   startTag                      children -> startTag <>                                      build' children
    TextNode   text                                   -> text
    IntlNode   intl                                   -> text
      where text = lang intl
  where
    build' = foldr ((<>) . buildIntlWithMap lang map) mempty


buildHead :: Map Builder Builder -> [Attribute] -> [Html lng] -> Builder
buildHead map attributes children = "<head" <> build attributes <> singleton '>' <> build' children <> buildCss map <> "</head>"
  where
    build' = foldr ((<>) . build) mempty


buildCss :: Map Builder Builder -> Builder
buildCss map = "<style>" <> buildStyles map <> "</style>"


buildStyles :: Map Builder Builder -> Builder
buildStyles = foldrWithKey (\key value acc -> acc <> singleton '.' <> key <> singleton '{' <> value <> singleton '}') mempty


extractWithMap :: Map Builder Builder -> Html lng -> (Map Builder Builder, Html lng)
extractWithMap map html = case html of
    ParentNode startTag endTag []         []       -> (map  , ParentNode startTag endTag []          []       )
    ParentNode startTag endTag attributes []       -> (map' , ParentNode startTag endTag attributes' []       )
      where (map', attributes') = merge map attributes
    ParentNode startTag endTag []         children -> (map' , ParentNode startTag endTag []          children')
      where (map', children') = extractList map children
    ParentNode startTag endTag attributes children -> (map'', ParentNode startTag endTag attributes' children')
      where
        (map' , attributes') = merge map attributes
        (map'', children'  ) = extractList map' children
    LeafNode   startTag        []                  -> (map  , LeafNode   startTag        []                   )
    LeafNode   startTag        attributes          -> (map' , LeafNode   startTag        attributes'          )
      where (map', attributes') = merge map attributes
    RootNode   startTag                   []       -> (map  , RootNode   startTag                    []       )
    RootNode   startTag                   children -> (map' , RootNode   startTag                    children')
      where (map', children') = extractList map children
    TextNode   text                                -> (map  , TextNode   text                                 )
    IntlNode   intl                                -> (map  , IntlNode   intl                                 )


extractList :: Map Builder Builder -> [Html lng] -> (Map Builder Builder, [Html lng])
extractList map = foldr (\child (map', list) -> insert list (extractWithMap map' child)) (map, [])
  where
    insert list (map', child) = (map', child : list)


merge :: Map Builder Builder -> [Attribute] -> (Map Builder Builder, [Attribute])
merge map []          = (map, [])
merge map [attribute] = mergeSingle   map attribute
merge map attributes  = mergeMultiple map attributes


mergeSingle :: Map Builder Builder -> Attribute -> (Map Builder Builder, [Attribute])
mergeSingle map attribute = case attribute of
    TextAttribute " css=\"" value -> (map', [ TextAttribute " class=\"" key ]) where key = hash value; map' = insert key value map
    other                         -> (map , [ other                         ])


mergeMultiple :: Map Builder Builder -> [Attribute] -> (Map Builder Builder, [Attribute])
mergeMultiple map (attribute:attributes) = case attribute of
    TextAttribute " css=\""   value -> mapCss   filtered unfiltered map value
    TextAttribute " class=\"" value -> mapClass filtered unfiltered map value
    other                           -> mapOther          unfiltered map other
  where
    (filtered, unfiltered) = partition (compare attribute) attributes


mapCss :: [Attribute] -> [Attribute] -> Map Builder Builder -> Builder -> (Map Builder Builder, [Attribute])
mapCss filtered unfiltered map value = (map', TextAttribute " class=\"" value' : list)
  where
    (map', value') = foldText (insert key value map, key) filtered where key = hash value
    (_   , list  ) = merge empty unfiltered


mapClass :: [Attribute] -> [Attribute] -> Map Builder Builder -> Builder -> (Map Builder Builder, [Attribute])
mapClass filtered unfiltered map value = (map', TextAttribute " class=\"" value' : list)
  where
    (map', value') = foldText (map, value) filtered
    (_   , list  ) = merge empty unfiltered


mapOther :: [Attribute] -> Map Builder Builder -> Attribute -> (Map Builder Builder, [Attribute])
mapOther unfiltered map attribute = (map', attribute : list)
  where
    (map', list) = merge map unfiltered


foldText :: (Map Builder Builder, Builder) -> [Attribute] -> (Map Builder Builder, Builder)
foldText = foldr f
  where
    f (TextAttribute " css=\""   value) (map, text) = (insert key value map, text <> singleton ' ' <> key  ) where key = hash value
    f (TextAttribute " class=\"" value) (map, text) = (map                 , text <> singleton ' ' <> value)
    f _                                 acc         = acc


compare :: Attribute -> Attribute -> Bool
compare (TextAttribute " css=\""   _) (TextAttribute " css=\""   _) = True
compare (TextAttribute " css=\""   _) (TextAttribute " class=\"" _) = True
compare (TextAttribute " class=\"" _) (TextAttribute " css=\""   _) = True
compare _                             _                             = False


hash :: Builder -> Builder
hash text = singleton '_' <> (decimal . abs . foldl (\acc char -> 17 * acc `xor` fromEnum char) 7879 . toLazyText) text
