{-# LANGUAGE OverloadedStrings #-}

module LearnHedis where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Control.Applicative

import Database.Redis
import Data.ByteString.Char8(pack, unpack)
import Data.ByteString.Internal(ByteString)

main :: IO ()
main = do
    putStrLn "Connecting with Redis"

    -- connects to localhost:6379
    conn <- connect defaultConnectInfo
    --runRedis conn $ do
    --    set "yo" "yo"
    --    ret <- incr "ia:count"
    --    liftIO $ print (ret)
    --    set "yo" "yo"

    --doRedisCreate conn
    ret <- doIt conn (pack "hello")
    liftIO $ putStrLn (show ret)

    putStrLn "Done"


render :: [Integer] -> String
render (x:xs) = show x


doIt :: Connection -> ByteString -> IO (Bool)
doIt conn value = runRedis conn $ do
    ret <- incr "ia:count"
    case ret of (Left _) -> return False
                (Right x) -> liftIO $ doRedisSet' conn (createKey x) value

doRedisSet' :: Connection -> ByteString -> ByteString -> IO (Bool)
doRedisSet' conn key value = runRedis conn $ do
    ret <- set key value
    case ret of (Left _) -> return False
                (Right _) -> return True


doRedisCreate :: Connection -> IO (Either Reply Status)
doRedisCreate conn = runRedis conn $ do
    ret <- incr "ia:count"

    let key :: ByteString
        yes_no = dealWithTheEither ret
        key = pack ("ia:" ++ (show (ret)))
    liftIO $ putStrLn (show yes_no)
    set key "value"
    set "sally" "yo"


doRedisIncrement :: Connection -> IO (Either Reply Integer)
doRedisIncrement conn = runRedis conn $ do
    ret <- incr "ia:count"
    return ret

doRedisSet :: Connection -> ByteString -> ByteString -> IO (Either Reply Status)
doRedisSet conn key value = runRedis conn $ do
    ret <- set key value
    return ret



createKey :: Integer -> ByteString
createKey x = pack ("ia:" ++ show x )


dealWithTheEither :: Either Reply Integer -> Bool
dealWithTheEither (Left _) = False
dealWithTheEither (Right x) = True


theThingThatCalls :: Connection -> IO (Bool)
theThingThatCalls conn = do
    return True

