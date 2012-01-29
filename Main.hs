{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Exception    (bracket)
import Data.Acid            (AcidState, openLocalState)
import Data.Acid.Local      (createCheckpointAndClose)

import Happstack.Lite

import Blog.Models
import Blog.Controllers

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["index.html"] "static"

config :: ServerConfig
config =
    ServerConfig { port      = 8001
                 , ramQuota  = 1 * 10^6
                 , diskQuota = 20 * 10^6
                 , tmpDir    = "/tmp/"
                 }

main :: IO ()
main = 
    do bracket (openLocalState initialBlogState)
               (createCheckpointAndClose)
               (\acid -> serve (Just config) (blogRoutes acid))

-- Routing
blogRoutes :: AcidState Blog -> ServerPart Response
blogRoutes acid = msum
  [
    dir "static"   $ fileServing,
    dir "post"     $ postDetail acid,
    homePage acid
  ]

