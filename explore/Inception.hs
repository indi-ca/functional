{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid



data Nugget = Nugget {
    index        :: Int,
    hash         :: String,
    content      :: String,
    lastModified :: Int
} deriving (Show)


n1 = Nugget {
    index = 1,
    hash = "1",
    content = "Content one",
    lastModified = 1
}

n2 = Nugget {
    index = 2,
    hash = "2",
    content = "Content two",
    lastModified = 1
}

n3 = Nugget {
    index = 3,
    hash = "3",
    content = "Content three",
    lastModified = 1
}


nuggets = [n1, n2, n3]



-- Now create a fast-indexing tree
data IndexTree m a = Empty
        | Single m a
        | Append m (IndexTree m a) (IndexTree m a)
    deriving (Show, Eq)


-- A helper function
tag :: Monoid m => IndexTree m a -> m
tag (Single m _) = m
tag (Append m _ _) = m


--- +++ operator
(+++) :: Monoid m => IndexTree m a -> IndexTree m a -> IndexTree m a
(+++) Empty tree = tree
(+++) tree Empty = tree
(+++) left right = Append (tag left <> tag right) left right


-- Now sequentially add the items into the Index Tree
insertNode :: (Monoid m, Num m) => a -> IndexTree m a -> IndexTree m a
insertNode node Empty = Single 1 node
insertNode node x@(Single _ _) = (Single 1 node) +++ x
insertNode node x@(Append _ _ _) = (Single 1 node) +++ x


instance Monoid Int where
    mempty = 0
    mappend = (+)





doSomething :: (Monoid m, Num m) => IndexTree m (Nugget)
doSomething = insertNode n1 Empty








