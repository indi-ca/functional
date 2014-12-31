module HedisInterface where


import Database.Redis(Redis, Connection, Reply, runRedis, connect, defaultConnectInfo, hset, lindex, rpush)

import Data.ByteString.Char8(pack, unpack)
import Control.Monad.IO.Class(liftIO)

import Data.ByteString.Internal as Foo(ByteString)





stuff = do
    conn <- connect defaultConnectInfo
    --writeSomething conn
    persistSearch conn "surfboard" "gumtree" "filepath"
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



-- next, i want to create a set with this incremented index
-- it is going to include multiple inserts
-- it really should fail if all of them fail
-- however, i could hack it by discarding the outputs
createSet index query target filepath = do
    ret <- hset (pack $ "fibres:" ++ index) (pack "query") (pack query)
    ret <- hset (pack $ "fibres:" ++ index) (pack "target") (pack target)
    ret <- hset (pack $ "fibres:" ++ index) (pack "filepath") (pack filepath)
    return ret

-- the interface should be agonstic to the details of the items i want to persist
-- another layer of agnostisms


-- first thing that i have to do is to get the index
persistSearch conn query target filepath = runRedis conn $ do
    index <- switch nextIndex
    yo <- incrementer index
    case yo of (Left reply) -> return (Left reply)
               (Right idx) -> createSet (show idx) query target filepath





