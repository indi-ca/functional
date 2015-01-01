module HedisInterface where

import Database.Redis(Redis, Connection, Reply, runRedis, connect, defaultConnectInfo, hset, lindex, rpush)
import Data.ByteString.Char8(pack, unpack)
import Data.ByteString.Internal(ByteString)
import Control.Monad.IO.Class(liftIO)


createKey :: String -> Integer -> ByteString
createKey prefix index = pack (prefix ++ ":" ++ (show index))



persistThree prefix list_key k1 v1 k2 v2 k3 v3 = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        index <- (nextIndex list_key) >>= (createList list_key)
        incrResult <- (incrementList list_key) index
        case incrResult of (Left reply) -> return (Left reply)
                           (Right idx) -> createSet' idx prefix k1 v1 k2 v2 k3 v3
    -- TODO: How do I return nothing?
    putStrLn "Done"


createSet' index prefix k1 v1 k2 v2 k3 v3 = do
    ret <- hset (createKey prefix index) (pack k1) (pack v1)
    ret <- hset (createKey prefix index) (pack k2) (pack v2)
    ret <- hset (createKey prefix index) (pack k3) (pack v3)
    return ret



nextIndex :: String -> Redis (Either Reply (Maybe ByteString))
nextIndex list_key = lindex (pack list_key) (-1)

-- Create a new list if necessary
createList :: String -> Either Reply (Maybe ByteString) -> Redis (Either Reply Integer)
createList _ (Left x) = return (Left x)
createList list_key (Right Nothing) = rpush (pack list_key) [(pack "0")] >>= initResponse
createList _ (Right (Just bs)) = return (Right (read $ unpack bs :: Integer))

-- Create an initial response of 0
initResponse :: Either Reply Integer -> Redis (Either Reply Integer)
initResponse (Left x) = return (Left x)
initResponse (Right _) = return (Right 0)

incrementList :: String -> Either Reply Integer -> Redis (Either Reply Integer)
incrementList _ (Left x) = return (Left x)
incrementList list_key (Right x) = rpush (pack list_key) [ (pack $ show (x + 1)) ]






