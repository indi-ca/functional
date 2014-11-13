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
    ret <- inject conn "ia" "I am appending"
    liftIO $ putStrLn (show ret)

    putStrLn "Done"




-- prefix like "ia"
inject :: Connection -> String -> String -> IO (Bool)
inject conn prefix fragment = runRedis conn $ do
    ret <- incr  $ pack (prefix ++ ":count")
    case ret of (Left _) -> return False
                (Right x) -> liftIO $ doRedisSet' conn (createKey x) (pack fragment)



doRedisSet' :: Connection -> ByteString -> ByteString -> IO (Bool)
doRedisSet' conn key value = runRedis conn $ do
    ret <- set key value
    case ret of (Left _) -> return False
                (Right _) -> return True

createKey :: Integer -> ByteString
createKey x = pack ("ia:" ++ show x )




