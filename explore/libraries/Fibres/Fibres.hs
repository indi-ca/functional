module Fibres where


import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import System.Directory(getTemporaryDirectory)
import System.FilePath.Posix((</>))
import System.IO(hPutStrLn, openFile, hClose, Handle, IOMode( WriteMode ))


import HedisInterface(persistThree)

type URL = String

data Search = Search {
    site :: String,
    keyword :: String,
    url :: URL
} deriving Show



sample_url = "http://www.gumtree.com.au/s-scooters/spring-hill-brisbane/scooter/k0c18629l3005758?price=0.00__1500.00"
search = Search "gumtree" "scooter" sample_url

persistSearch :: Search -> IO ()
persistSearch search = persistThree "fibres" "search"
                        "site" (site search)
                        "keyword" (keyword search)
                        "url" (url search)



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
