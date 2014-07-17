module Main where

import SimpleJSON
import PutJSON


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
