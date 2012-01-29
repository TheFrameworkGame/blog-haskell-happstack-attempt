{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell
  , TypeFamilies, OverloadedStrings, ScopedTypeVariables #-}
module Blog.Models where

import Control.Monad.State  (get, put)
import Control.Monad.Reader (ask)
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced   (update', query')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet)
import qualified Data.IxSet as IxSet
import Data.Acid            (AcidState, Update, Query, makeAcidic, openLocalState)
import Data.Acid.Advanced   (update', query')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Data            (Data, Typeable)
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Text            (Text, pack, append)
import Data.Text.Lazy       (toStrict, unpack)
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..), getCurrentTime)

newtype PostId = PostId { unPostId :: Integer }
    deriving (Eq, Ord, Data, Enum, Read, Show, Typeable, SafeCopy)
data Status =
    Draft 
  | Published 
    deriving (Eq, Ord, Data, Typeable, Read, Show)

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
instance Indexable Post where
    empty = ixSet [ ixFun $ \bp -> [ postId bp ]
                  , ixFun $ \bp -> [ Title  $ title bp  ]
                  , ixFun $ \bp -> [ Slug $ slug bp  ]
                  , ixFun $ \bp -> [ Author $ author bp ]
                  , ixFun $ \bp -> [ status bp ]
                  , ixFun $ \bp -> map Tag (tags bp)
                  , ixFun $ (:[]) . date  -- point-free, just for variety
                  ]

data Blog = Blog
    { nextPostId :: PostId
    , posts      :: IxSet Post
    }
    deriving (Data, Typeable)
             
$(deriveSafeCopy 0 'base ''Blog)

-- example initial post
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
initialBlogState = Blog { nextPostId = PostId 2, posts = IxSet.insert post1 empty }

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

postBy :: (Typeable a) => a -> Query Blog (Maybe Post)
postBy val = ask >>= \Blog{..} -> return $ getOne $ posts @= val

postById :: PostId -> Query Blog (Maybe Post)
postById pid = postBy pid

postBySlug :: Slug -> Query Blog (Maybe Post)
postBySlug slug = ask >>= \Blog{..} -> return $ getOne $ posts @= slug

postsByStatus :: Status -> Query Blog [Post]
postsByStatus status = ask >>= \Blog{..} -> return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ posts @= status

$(makeAcidic ''Blog
  [ 'newPost
  , 'updatePost
  , 'postById
  , 'postBySlug
  , 'postsByStatus
  ])
