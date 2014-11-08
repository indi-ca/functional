{-# LANGUAGE OverloadedStrings #-}

--module LearnHedis where

import Database.Redis


--redisThing :: Connection -> IO (Either Reply Status)
--redisThing conn = conn $ do
--    set "hello" "hello"


main :: IO ()
main = do
    -- connects to localhost:6379
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        set "hello" "hello"

    putStrLn "Connecting with Redis"


