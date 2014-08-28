



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


-- Now sequentiall add the items into the Index Tree



