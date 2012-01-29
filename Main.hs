{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative  ((<$>), optional)
import Control.Exception    (bracket)
import Control.Monad        (msum, mzero)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced   (update', query')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Text            (Text, pack, append)
import Data.Text.Lazy       (toStrict, unpack)
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..), getCurrentTime)

import Data.Maybe (fromMaybe)
--import Happstack.Server
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, div, ul, li, h1, h2, span, toValue)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, class_, rel, media)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (div, span)

import Control.Monad

-- Models

newtype PostId = PostId { unPostId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)
data Status =
    Draft 
  | Published 
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Status)

data Post = Post
    { postId  :: PostId
    , title   :: Text
    , slug    :: Text
    , author  :: Text
    , body    :: Text
    , tease   :: Text
    , date    :: UTCTime
    , status  :: Status
    , tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Post)

newtype Title     = Title Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Slug      = Slug Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Author    = Author Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Tag       = Tag Text      deriving (Eq, Ord, Data, Typeable, SafeCopy)
-- newtype WordCount = WordCount Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
instance Indexable Post where
    empty = ixSet [ ixFun $ \bp -> [ postId bp ]
                  , ixFun $ \bp -> [ Title  $ title bp  ]
                  , ixFun $ \bp -> [ Slug $ slug bp  ]
                  , ixFun $ \bp -> [ Author $ author bp ]
                  , ixFun $ \bp -> [ status bp ]
                  , ixFun $ \bp -> map Tag (tags bp)
                  , ixFun $ (:[]) . date  -- point-free, just for variety
--                  , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
                  ]

data Blog = Blog
    { nextPostId :: PostId
    , posts      :: IxSet Post
    }
    deriving (Data, Typeable)
             
$(deriveSafeCopy 0 'base ''Blog)

post1 :: Post
post1 = Post { postId = PostId 1
            , title  = pack "About the model layer"
            , slug   = pack "about-the-model-layer"
            , author = pack "Bob Johnson"
            , body   = pack "sigh sigh sigh, why so serious?"
            , tease  = pack "The model has a central position in an application."
            , date   = (read "2011-11-19 18:28:52.607875 UTC")::UTCTime
            , status = Published
            , tags   = []
            }

initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = PostId 2
         , posts      = IxSet.insert post1 empty
         }
-- | create a new, empty post and add it to the database
newPost :: UTCTime -> Update Blog Post
newPost pubDate =
    do b@Blog{..} <- get
       let post = Post { postId = nextPostId
                       , title  = Text.empty
                       , slug   = Text.empty
                       , author = Text.empty
                       , body   = Text.empty
                       , tease  = Text.empty
                       , date   = pubDate
                       , status = Draft
                       , tags   = []
                       }
       put $ b { nextPostId = succ nextPostId
               , posts      = IxSet.insert post posts
               }
       return post
-- | update the post in the database (indexed by PostId)
updatePost :: Post -> Update Blog ()
updatePost updatedPost =
    do b@Blog{..} <- get
       put $ b { posts = IxSet.updateIx (postId updatedPost) updatedPost posts
               }
postById :: PostId -> Query Blog (Maybe Post)
postById pid = 
     do Blog{..} <- ask
        return $ getOne $ posts @= pid

postBySlug :: Slug -> Query Blog (Maybe Post)
postBySlug slug = 
     do Blog{..} <- ask
        return $ getOne $ posts @= slug

postsByStatus :: Status -> Query Blog [Post]
postsByStatus status =
    do Blog{..} <- ask
       return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ posts @= status
$(makeAcidic ''Blog
  [ 'newPost
  , 'updatePost
  , 'postById
  , 'postBySlug
  , 'postsByStatus
  ])

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
hBody = toHtml . body
hAuthor post = toHtml $ append "by " $ author post
getUrl post = toValue $ append "/post/"  $ slug post

homePage :: AcidState Blog -> ServerPart Response
homePage acid = do
    published <- query' acid (PostsByStatus Published)
    ok $ template $ do
      -- TODO: categories
      forM_ published $ \post -> do
        div ! class_ "row" $ do
          div ! class_ "span8" $ do
            h2 $ a ! href (getUrl post) $ hTitle post
            span ! class_ "post-author" $ hAuthor post
          div ! class_ "span6 tease" $ hTease post

postDetail :: AcidState Blog -> ServerPart Response
postDetail acid = 
  path $ \(slugArg :: String) -> do
    mbPost <- query' acid $ PostBySlug (Slug $ pack slugArg)
    case mbPost of
      Nothing ->
        notFound $ template $ "Couldn't find post with slug: " >> toHtml slugArg
      (Just post) ->
        ok $ template $ do
        -- TODO: predecessor, successor
        div ! class_ "row" $ div ! class_ "span14" $ do
          h2 $ a ! href "#" $ hTitle post
          span ! class_ "post-author" $ hAuthor post
          div ! class_ "post-body" $ hBody post

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
  [ --dir "echo"    $ echo
--  , dir "query"   $ queryParams
--  , dir "form"    $ formPage
--  , dir "fortune" $ fortune
    dir "static"   $ fileServing,
    dir "post"   $ postDetail acid,
--  , dir "upload"  $ upload
    homePage acid
  ]

