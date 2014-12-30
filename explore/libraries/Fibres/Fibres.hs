module Fibres where


import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import System.Directory(getTemporaryDirectory)
import System.FilePath.Posix((</>))
import System.IO(hPutStrLn, openFile, hClose, Handle, IOMode( WriteMode ))

import Database.Redis(Redis, Connection, Reply, runRedis, connect, defaultConnectInfo, hset, lindex, rpush)

import Data.ByteString.Char8(pack, unpack)
import Control.Monad.IO.Class(liftIO)

import Data.ByteString.Internal as Foo(ByteString)

type URL = String

data Search = Search {
    site :: String,
    keyword :: String,
    url :: URL
} deriving Show



--writeSomething :: String -> IO (Either Reply Integer)

main = do
    conn <- connect defaultConnectInfo
    writeSomething conn
    runRedis conn $ do
        ret <- hset (pack "test") (pack "hello") (pack "value")
        liftIO $ print ("")
    -- ret <- sadd (pack "key") [pack "subkey"] (pack "value")
    putStrLn "Done"


--writeSomething conn = runRedis conn $ do
--    ret <- lindex (pack "mylist") 0
--    liftIO $ print ret

writeSomething conn = runRedis conn $ do
    ret <- switch nextIndex
    liftIO $ print ret


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




sample_url = "http://www.gumtree.com.au/s-scooters/spring-hill-brisbane/scooter/k0c18629l3005758?price=0.00__1500.00"
search = Search "gumtree" "scooter" sample_url



getURL :: URL -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

makeHandle :: Search -> IO Handle
makeHandle search = (create_target search) >>= (flip openFile $ WriteMode)

create_target :: Search -> IO FilePath
create_target = createTempFile . target_name

target_name :: Search -> FilePath
target_name search = site search ++ "-" ++ (keyword search) ++ ".html"

createTempFile :: FilePath -> IO FilePath
createTempFile filename = fmap (</> filename) getTemporaryDirectory


save :: Search -> IO ()
save search = do
    content <- getURL (url search)
    handle <- makeHandle search
    hPutStrLn handle content
    hClose handle
