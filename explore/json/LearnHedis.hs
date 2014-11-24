{-# LANGUAGE OverloadedStrings #-}

module LearnHedis where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.ByteString.Char8(pack, unpack)
import Data.ByteString.Internal(ByteString)
import Data.Functor

import Database.Redis

main :: IO ()
main = do
    putStrLn "Connecting with Redis"

    -- connects to localhost:6379
    conn <- connect defaultConnectInfo

    ret <- rAppend conn "ia" "I am appending"
    liftIO $ putStrLn (show ret)

    putStrLn "Done"

-- What am I going to do next?
-- delete a key
-- read all keys into memory
-- maintain a list

-- i could read the int on the last element of the list
-- and use this to increment instead of the count

-- create a hash
-- and append with current timestamp
-- create a (finger) tree
-- create a bloom filter



-- prefix like "ia"
rAppend :: Connection -> String -> String -> IO (Bool)
rAppend conn prefix fragment = runRedis conn $ do
    -- using a hash
    bob <- hset "hashkey" "field" "value"
    ret <- incr  $ pack (prefix ++ ":count")
    case ret of (Left _) -> return False
                (Right x) -> liftIO $ doRedisSet' conn x (pack fragment)



doRedisSet' :: Connection -> Integer -> ByteString -> IO (Bool)
doRedisSet' conn index value = runRedis conn $ do
    ret <- set (createKey index) value
    bob <- rpush "ia:members" ([pack . show $ index])
    case ret of (Left _) -> return False
                (Right _) -> return True

createKey :: Integer -> ByteString
createKey x = pack ("ia:" ++ show x )



fetchAll :: Connection -> String -> IO ( [(Integer, String)] )
fetchAll conn prefix = runRedis conn $ do
    -- read all the members
    num_members <- llen "ia:members"
    -- TODO: Fix this cheat: don't return an empty list if there is a failure
    case num_members of (Left _) -> return []
                        (Right x) -> liftIO $ fetchAll' conn x

fetchAll' :: Connection -> Integer -> IO ( [(Integer, String)] )
fetchAll' conn index = runRedis conn $ do
    items <- lrange "ia:members" 1 1
    case items of (Left _) -> return []
                  (Right x) -> return []

-- Redis lists are base 0
fetchIndexes' :: Connection -> Integer -> IO ( [String] )
fetchIndexes' conn index = runRedis conn $ do
    items <- lrange "ia:members" 0 index
    case items of (Left _) -> return []
                  (Right x) -> return $ map unpack x

-- What am I doing?
-- I'm taking a list of indexes, and i'm returning a list of fragment contents

-- what's the plan
-- map the indeces, onto keys
-- map the keys onto fetches
-- but it's complicated, because each result of a fetch is an either

-- Remember, there is an IO map
--fetchItems :: Connection -> [String] -> IO ([String])


fetchOneItem :: Connection -> ByteString -> IO (Either Reply Integer)
fetchOneItem conn



