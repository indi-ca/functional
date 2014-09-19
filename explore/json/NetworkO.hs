--import Network          (PortID(PortNumber), withSocketsDo, listenOn, accept)
import Network
import Network.Socket   (Socket, SocketOption(KeepAlive), close, setSocketOption)
--import Network.Socket

--import System.IO        (Handle, hPutStrLn, hGetLine, hFlush, hClose)
import System.IO


host = "127.0.0.100"
port   = 62005


main :: IO ()
main = withSocketsDo $ do
    putStrLn "Going to accept from a network connection..."
    --sock <- listenOn $ PortNumber port
    putStrLn host
    putStrLn (show port)


    sock <- listenOn (PortNumber (fromIntegral port))

    -- Mark the socket for keep-alive handling since it may be idle
    -- for long periods of time
    setSocketOption sock KeepAlive 1

    putStrLn "Awaiting connection."
    (h,host,port) <- accept sock
    putStrLn $ "Received connection from " ++ host ++ ":" ++ show port
    hSetBuffering h LineBuffering
    --h <- connectTo host (PortNumber (fromIntegral port))
    --hSetBuffering h NoBuffering
    --sockHandler sock
    putStrLn "Listening"
    listen h


listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s
    hPutStrLn h "{\"results\" : [\"one\", \"two\", \"three\", \"four\", \"five\"]}"
  where
    forever a = do a; forever a


-- server
--server = do
--    sock <- listenOn (PortNumber port)
--    putStrLn "Awaiting connection."
--    (h,host,port) <- accept sock
--    putStrLn $ "Received connection from " ++ host ++ ":" ++ show port
--    hSetBuffering h LineBuffering
--    while2 (receive h) (send h)
--    hClose h
--    sClose sock
