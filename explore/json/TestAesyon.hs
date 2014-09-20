
import Data.Aeson
import Aesyon
import Data.ByteString.Lazy.Char8(pack)


main :: IO ()
main = do
    contents <- readFile "sample.json"
    --let req = decode contents :: Maybe [Nugget]
    let req = decode (pack contents) :: Maybe [Nugget]
    putStrLn $ show req
    putStrLn "done"
