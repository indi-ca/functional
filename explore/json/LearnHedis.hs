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
    runRedis conn $ do
        set "yo" "yo"
        ret <- incr "ia:count"
        liftIO $ print (ret)
        set "yo" "yo"

    doRedisCreate conn

    putStrLn "Done"


render :: [Integer] -> String
render (x:xs) = show x


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

-- What do I want?
-- I want to combine two IO functions
-- Call the second, if the first one succeeds, and return True
-- Or just return False




--makeKey :: Int -> String
--makeKey index = "ia" ++ ":" ++ index
