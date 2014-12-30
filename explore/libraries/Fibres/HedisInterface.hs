module HedisInterface where


import Database.Redis(Redis, Connection, Reply, runRedis, connect, defaultConnectInfo, hset, lindex, rpush)

import Data.ByteString.Char8(pack, unpack)
import Control.Monad.IO.Class(liftIO)

import Data.ByteString.Internal as Foo(ByteString)





main = do
    conn <- connect defaultConnectInfo
    writeSomething conn
    runRedis conn $ do
        ret <- hset (pack "test") (pack "hello") (pack "value")
        liftIO $ print ("")
    -- ret <- sadd (pack "key") [pack "subkey"] (pack "value")
    putStrLn "Done"




-- this is the thing that get's the next index

writeSomething conn = runRedis conn $ do
    ret <- switch nextIndex
    yo <- incrementer ret
    liftIO $ print yo


nextIndex :: Redis (Either Reply (Maybe Foo.ByteString))
nextIndex = lindex (pack "mylist") (-1)

switch :: Redis (Either Reply (Maybe Foo.ByteString)) -> Redis (Either Reply Integer)
switch first_result = first_result >>= switch'

switch' :: Either Reply (Maybe Foo.ByteString) -> Redis (Either Reply Integer)
switch' (Left x) = return (Left x)
switch' (Right Nothing) = rpush (pack "mylist") [(pack "0")] >>= switch''
switch' (Right (Just bs)) = return (Right (read $ unpack bs :: Integer))

switch'' :: Either Reply Integer -> Redis (Either Reply Integer)
switch'' (Left x) = return (Left x)
switch'' (Right _) = return (Right 0)



-- next, i want to increment the index

incrementer :: Either Reply Integer -> Redis (Either Reply Integer)
incrementer (Left x) = return (Left x)
incrementer (Right x) = rpush (pack "mylist") [ (pack $ show (x + 1))]
