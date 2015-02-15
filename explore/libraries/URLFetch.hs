module URLFetch where

import Network.HTTP



url = "http://www.gumtree.com.au/s-scooters/spring-hill-brisbane/scooter/k0c18629l3005758?price=0.00__1500.00"

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

main :: IO ()
main = do
    putStrLn "Fetching a URL"
    val <- get url
    putStrLn val
