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
        index <- (switch list_key) (nextIndex list_key)
        yo <- (incrementList list_key) index
        case yo of (Left reply) -> return (Left reply)
                   (Right idx) -> createSet' idx prefix k1 v1 k2 v2 k3 v3
    putStrLn "Done"


createSet' index prefix k1 v1 k2 v2 k3 v3 = do
    ret <- hset (createKey prefix index) (pack k1) (pack v1)
    ret <- hset (createKey prefix index) (pack k2) (pack v2)
    ret <- hset (createKey prefix index) (pack k3) (pack v3)
    return ret



nextIndex :: String -> Redis (Either Reply (Maybe ByteString))
nextIndex list_key = lindex (pack list_key) (-1)


switch :: String -> Redis (Either Reply (Maybe ByteString)) -> Redis (Either Reply Integer)
switch list_key first_result = first_result >>= (switch' list_key)


-- This is some sort of create if not exists
switch' :: String -> Either Reply (Maybe ByteString) -> Redis (Either Reply Integer)
switch' _ (Left x) = return (Left x)
switch' list_key (Right Nothing) = rpush (pack list_key) [(pack "0")] >>= switch''
switch' _ (Right (Just bs)) = return (Right (read $ unpack bs :: Integer))

-- This is a fail or return the init state
switch'' :: Either Reply Integer -> Redis (Either Reply Integer)
switch'' (Left x) = return (Left x)
switch'' (Right _) = return (Right 0)

incrementList :: String -> Either Reply Integer -> Redis (Either Reply Integer)
incrementList _ (Left x) = return (Left x)
incrementList list_key (Right x) = rpush (pack list_key) [ (pack $ show (x + 1)) ]






