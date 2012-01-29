{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}

module Blog.Controllers (
    homePage,
    postDetail,
    editPost
) where

import Data.Text            (pack, unpack)
import Happstack.Lite

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')

import Text.Digestive
import Text.Digestive.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), toValue)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Forms.Happstack
import Text.Blaze (Html)
import Text.Blaze.Renderer.Pretty (renderHtml)
import Control.Applicative

import Control.Monad (forM_)

import Blog.Models
import Blog.Views

homePage :: AcidState Blog -> ServerPart Response
homePage acid = query' acid (PostsByStatus Published) >>= postListView

postDetail :: AcidState Blog -> ServerPart Response
postDetail acid = postOr404 acid postView

listForm :: (Read a, Show a, Monad m, Functor m) => [a] -> HappstackForm m Html BlazeFormHtml [a]
listForm def = inputTextRead "Can't read list" (Just def) <++ errors

idForm :: (Monad m, Functor m) => HappstackForm m Html BlazeFormHtml PostId
idForm = inputTextRead "id (sigh)" Nothing <++ errors

-- TODO: hide the date and the id
postForm :: (Monad m, Functor m) => Maybe Post
         -> HappstackForm m Html BlazeFormHtml Post
postForm post = Post <$> label "id" ++> idForm
                <*> label "Title" ++> (inputText $ fmap title post)
                <*> label "slug" ++> (inputText $ fmap slug post)
                <*> label "author" ++> (inputText $ fmap author post)
                <*> label "body" ++> (inputText $ fmap body post)
                <*> label "tease" ++> (inputText $ fmap tease post)
                <*> label "date" ++> (inputTextRead undefined $ fmap date post)
                <*> label "Status" ++> (inputTextRead undefined $ fmap status post)
                <*> label "Tags" ++> listForm []

-- Eurgh, manual crud
editPost :: AcidState Blog -> ServerPart Response
editPost acid = postOr404 acid $ \post -> do
    r <- eitherHappstackForm (childErrors ++> (postForm $ Just post)) "Edit Post"
    case r of 
      Left form' -> ok $ template $ do
        let (formHtml', enctype) = renderFormHtml form'
        H.style ! A.type_ "text/css" $ do
            "input {display: block;}\n"
        H.form ! A.enctype (toValue $ show enctype)
               ! A.method "POST" ! A.action "" $ do
          formHtml'
          H.input ! A.type_ "submit" ! A.value "save"
      Right updatedPost -> do
        update' acid (UpdatePost updatedPost)
        seeOther ("/post/"++ (unpack $ slug post)) (toResponse ())

-- Utility
postOr404 :: AcidState Blog -> (Post -> ServerPart Response) -> ServerPart Response
postOr404 acid cb =
  path $ \(slugArg :: String) -> do
    mbPost <- query' acid $ PostBySlug (Slug $ pack slugArg)
    case mbPost of
      Nothing -> err404 $ "Couldn't find post with slug: " ++ slugArg
      (Just post) -> cb post
