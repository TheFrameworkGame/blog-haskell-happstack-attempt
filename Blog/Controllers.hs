{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}

module Blog.Controllers (homePage, postDetail) where

import Data.Text            (pack)
import Happstack.Lite

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query')

import Blog.Models
import Blog.Views

homePage :: AcidState Blog -> ServerPart Response
homePage acid = query' acid (PostsByStatus Published) >>= postListView

postDetail :: AcidState Blog -> ServerPart Response
postDetail acid = 
  path $ \(slugArg :: String) -> do
    mbPost <- query' acid $ PostBySlug (Slug $ pack slugArg)
    case mbPost of
      Nothing -> err404 $ "Couldn't find post with slug: " ++ slugArg
      (Just post) -> postView post
