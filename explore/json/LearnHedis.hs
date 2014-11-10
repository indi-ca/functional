{-# LANGUAGE OverloadedStrings #-}

module LearnHedis where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Control.Applicative

import Database.Redis


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

    putStrLn "Done"


render :: [Integer] -> String
render (x:xs) = show x


--doSomething :: Connection -> IO ()
--doSomething conn = runRedis conn $ do
--    set "bob" "bob"
--    putStrLn "Done"

--makeKey :: Int -> String
--makeKey index = "ia" ++ ":" ++ index
