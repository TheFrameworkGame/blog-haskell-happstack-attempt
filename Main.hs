{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
--import Happstack.Server
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, div, ul, li, h1, h2, span, toValue)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, class_, rel, media)
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
  dir "static"   $ fileServing,
  dir "post"   $ postDetail,
--  , dir "upload"  $ upload
    homePage
  ]

-- Models

data Post = Post {
  title :: String,
  slug :: String,
  tease :: String,
  author :: String
} deriving (Show)

-- TODO: replace this with acid-state
posts = [
    Post { title = "About the model layer", slug = "about-the-model-layer", tease = "The model has a central position in a Play! application.", author = "Bob Johnson" }
  , Post { title = "The MVC application", slug = "the-mvc-application", tease = "A Play! application follows the MVC architectural pattern.", author = "Jeff" }
  , Post { title = "Just a test of YABE", slug = "just-a-test-of-yabe", tease = "a test.", author = "Bob Johnson" } ]

postBySlug :: String -> Post
postBySlug s = head $ filter (\post -> s == (slug post)) posts

-- Views + Controllers

css loc = H.link ! rel "stylesheet" ! media "screen" ! href loc

template :: Html -> Response
template body = toResponse $
  H.html $ do
    H.head $ do
      H.title title
      css "/static/stylesheets/bootstrap.min.css"
      css "/static/stylesheets/main.css"
    H.body $ do
      div ! class_ "topbar" $ div ! class_ "fill" $ div ! class_ "container" $ do
        ul ! class_ "nav" $ do
          li ! class_ "active" $ a ! href "/" $ "home"
          li $ a ! href "/admin" $ "admin"
        ul ! class_ "nav secondary-nav" $ do
          li $ a ! href "/secure/login" $ "login"
      div ! class_ "container" $ do
        div ! class_ "content" $ do
          div ! class_ "page-header" $ h1 $ "The Framework Game in Happstack"
          body
        H.footer $ do
          p $ "learn " >> (a ! href "http://happstack.com/index.html" $ "more")
  where
    title = "The Framework Game"

-- utility methods
hTitle = toHtml . title
hTease = toHtml . tease
hAuthor post = toHtml $ "by " ++ (author post)
getUrl post = toValue $ "/post/" ++ (slug post)

homePage :: ServerPart Response
homePage =
    ok $ template $ do
      -- TODO: categories
      forM_ posts $ \post -> do
        div ! class_ "row" $ do
          div ! class_ "span8" $ do
            h2 $ a ! href (getUrl post) $ hTitle post
            span ! class_ "post-author" $ hAuthor post
          div ! class_ "span6 tease" $ hTease post

postDetail :: ServerPart Response
postDetail =
  path $ \(slugArg :: String) ->
    ok $ template $ do
    let post = postBySlug slugArg
    -- TODO: predecessor, successor
    div ! class_ "row" $ div ! class_ "span14" $ do
      h2 $ a ! href "#" $ hTitle post
      span ! class_ "post-author" $ hAuthor post
      div ! class_ "post-body" $ hTease post

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["index.html"] "static"
