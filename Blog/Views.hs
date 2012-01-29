{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}

module Blog.Views (
    postListView,
    postView,
    err404
) where

import Control.Monad                (forM_)
import Data.Text                    (append)
import Happstack.Lite               (ServerPart, Response, ok, notFound, toResponse)
import Text.Blaze.Html5             (Html, (!), a, form, input, p, toHtml, label,
                                    div, ul, li, h1, h2, span, toValue)
import Text.Blaze.Html5.Attributes  (action, enctype, href, name, size, type_, value,
                                    class_, rel, media)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (div, span)

import Blog.Models

-- Actual Views

postListView :: [Post] -> ServerPart Response
postListView posts = 
    ok $ template $ do
      -- TODO: categories
      forM_ posts $ \post -> do
        div ! class_ "row" $ do
          div ! class_ "span8" $ do
            h2 $ a ! href (getUrl post) $ hTitle post
            span ! class_ "post-author" $ hAuthor post
          div ! class_ "span6 tease" $ hTease post

postView :: Post -> ServerPart Response
postView post = 
    ok $ template $ do
    -- TODO: predecessor, successor
    div ! class_ "row" $ div ! class_ "span14" $ do
      h2 $ a ! href "#" $ hTitle post
      span ! class_ "post-author" $ hAuthor post
      div ! class_ "post-body" $ hBody post

err404 :: String -> ServerPart Response
err404 msg = notFound $ template $ toHtml msg

-- Common site template
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
hBody = toHtml . body
hAuthor post = toHtml $ append "by " $ author post
getUrl post = toValue $ append "/post/"  $ slug post

css loc = H.link ! rel "stylesheet" ! media "screen" ! href loc
