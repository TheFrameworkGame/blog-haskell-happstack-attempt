{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings #-}

module Happstack.Crud where

import Data.Typeable
import Data.Data
import Data.Generics.Aliases
import Control.Monad.Reader
import Data.Generics
import Data.Maybe
import Data.Tree

import Text.Blaze.Html5 ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (Html)
import Text.Blaze.Renderer.Pretty (renderHtml)

import Happstack.Lite               (ServerPart, Response, ok, notFound, toResponse)

import Monad                        (zipWithM_)
import Data.Text                    (Text)
import Char                         (toUpper,toLower)

-- Common Types
type Template = Html -> Response
type Fields = [String]

-- genericCreateOf

-- Provides a read-only view on a type
genericReadViewOf :: Data d => d -> Template -> Fields -> ServerPart Response
genericReadViewOf value template exclude =
    let
        cons = toConstr value
        fields = constrFields cons
        values = gmapQ render value
        toDiv k v = 
            if k `elem` exclude then
                no
            else
                do H.div ! A.class_ "row" $ do
                    H.div ! A.class_ "label" $ H.toHtml $ formatTitle k
                    H.div ! A.class_ "value" $ H.toHtml v
    in
        ok $ template $ do
            H.h2 $ H.toHtml $ show cons
            zipWithM_ toDiv fields values

-- genericUpdateOf
-- genericListOf
-- genericDeleteOf
-- genericCrudOf

-- Utility Functions
formatTitle :: String -> String
formatTitle str = (toUpper $ head str) : (tail str)

type R a = a -> String
render :: Data a => R a
render = (showConstr . toConstr) `extQ` (renderShow :: R Text)

-- TODO: figure out how to abstract over type classes as well as types
renderShow :: Show a => R a
renderShow x = let str = show x in take (length str - 2 ) $ drop 1 str

no = return ()
