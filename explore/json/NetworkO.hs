import System.IO
--import Network.Socket
import Network.Socket



main :: IO ()
main = withSocketsDo $ do
    putStrLn "Going to accept from a network connection"

    -- Now I need a handle
    listenOn $ PortNumber port
