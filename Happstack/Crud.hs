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

import Happstack.Lite               -- (ServerPart, Response, ok, notFound, toResponse)

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
        ValueInfo name fields values = extractInfo value
        toDiv k v = 
            if k `elem` exclude then
                no
            else
                do H.div ! A.class_ "row" $ do
                    H.div ! A.class_ "label" $ H.toHtml $ formatTitle k
                    H.div ! A.class_ "value" $ H.toHtml v
    in
        ok $ template $ do
            H.h2 $ H.toHtml name
            zipWithM_ toDiv fields values

-- Provides Edit functionality
-- TODO: more detailed generation for different types
-- TODO: add the concept of an id field
-- TODO: figure out how to cast things
genericUpdateViewOf :: (Show d, Data d) => d -> Template -> ServerPart Response
genericUpdateViewOf value template = msum [ viewForm, processForm ]
  where
    viewForm = 
        let
            ValueInfo name fields values = extractInfo value
            toField k v = 
                H.div ! A.class_ "clearfix" $ do
                    H.label ! A.for ka $ kh
                    H.div ! A.class_ "input" $ H.input ! A.type_ "text" ! A.name ka ! A.id ka ! A.value va
                where kh = H.toHtml k
                      ka = H.toValue k
                      va = H.toValue v
        in
            do  method GET
                ok $ template $ do
                    H.div ! A.class_ "row" $ H.form ! A.method "post" $ H.fieldset $ do
                            H.legend $ H.toHtml name
                            zipWithM_ toField fields values
                            H.div ! A.class_ "actions" $ H.input ! A.class_ "btn primary" ! A.type_ "submit" ! A.value "save"
    processForm = do
        method POST
        let ValueInfo name fields values = extractInfo value
        mapM_ (\f -> lookText f >>= \x -> liftIO $ print x) fields
        genericReadViewOf value template []

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

data ValueInfo = ValueInfo String [String] [String]

extractInfo :: Data d => d -> ValueInfo
extractInfo value =
    let
        cons = toConstr value
        fields = constrFields cons
        values = gmapQ render value
    in
        ValueInfo (show cons) fields values

no = return ()

-- De-trealise Tree to Data
tree2data :: Data a => Tree String -> Maybe a
tree2data = gdefault `extR` atString
  where
    atString (Node x []) = Just x
    gdefault (Node x ts) = res
      where

    -- a helper for type capture
        res  = maybe Nothing (kids . fromConstr) con

    -- the type to constructed
        ta   = fromJust res

    -- construct constructor
        con  = readConstr (dataTypeOf ta) x

        -- recursion per kid with accumulation
        perkid ts = const (tail ts, tree2data (head ts))

        -- recurse into kids
        kids x =
          do guard (glength x == length ts)
             snd (gmapAccumM perkid ts x)

-- Trealise Data to Tree
data2tree :: Data a => a -> Tree String
data2tree = gdefault `extQ` atString
  where
    atString (x::String) = Node x []
    gdefault x = Node (render x) (gmapQ data2tree x)

