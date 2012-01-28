{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
--import Happstack.Server
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, div, ul, li, h1, h2, span)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, class_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (div, span)

import Control.Monad

config :: ServerConfig
config =
    ServerConfig { port      = 8001
                 , ramQuota  = 1 * 10^6
                 , diskQuota = 20 * 10^6
                 , tmpDir    = "/tmp/"
                 }

main :: IO ()
main = serve (Just config) blogRoutes

-- Routing
blogRoutes :: ServerPart Response
blogRoutes = msum
  [ --dir "echo"    $ echo
--  , dir "query"   $ queryParams
--  , dir "form"    $ formPage
--  , dir "fortune" $ fortune
--  , dir "files"   $ fileServing
--  , dir "upload"  $ upload
    homePage
  ]

-- Models

data Post = Post {
  title :: String,
  tease :: String,
  author :: String
} deriving (Show)

posts = [
    Post { title = "About the model layer", tease = "The model has a central position in a Play! application.", author = "Bob Johnson" }
  , Post { title = "The MVC application", tease = "A Play! application follows the MVC architectural pattern.", author = "Jeff" }
  , Post { title = "Just a test of YABE", tease = "a test.", author = "Bob Johnson" } ]

-- Views + Controllers
template :: Html -> Response
template body = toResponse $
  H.html $ do
    H.head $ do
      H.title title
    H.body $ do
      div ! class_ "topbar" $ do
        ul ! class_ "nav" $ do
          li ! class_ "active" $ a ! href "/" $ "home"
          li $ a ! href "/admin" $ "admin"
      div ! class_ "container" $ do
        div ! class_ "content" $ do
          div ! class_ "page-header" $ h1 $ "The Framework Game in Happstack"
          body
      H.footer $ do
        p $ do
          "learn "
          a ! href "http://happstack.com/index.html" $ "more"
  where
    title = "The Framework Game"

homePage :: ServerPart Response
homePage =
    ok $ template $ do
      -- TODO: categories
      forM_ posts $ \post -> do
        div ! class_ "row" $ do
          div ! class_ "span8" $ do
            h2 $ a ! href "/post/" $ H.toHtml $ title post
            span ! class_ "post-author" $ H.toHtml $ "by " ++ (author post)
          div ! class_ "span6 tease" $ H.toHtml $ tease post
          

