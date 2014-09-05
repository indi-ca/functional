import System.IO

import SimpleJSON
import PutJSON

main :: IO ()
main = do
    putStrLn "hello"
    inputHandle <- openFile "input.txt" ReadMode
    outputHandle <- openFile "output.txt" WriteMode
    --inpStr <- getLine
    something entities outputHandle
    hClose inputHandle
    hClose outputHandle
    putStrLn "done"



data Entity = Entity { firstName :: String }
    deriving Show

myEntity = Entity "Darrly"
myEntityB = Entity "Goashes"

entities = [myEntity, myEntityB]

something :: [Entity] -> Handle -> IO ()
something entities handle = hPutStrLn handle $ doTheStringThing . renderEntities $ entities

--renderJValue $ renderEntity myEntity


-- I've got an entity, and I want to turn it into a JObject
-- I'm going to have a list of entities, and I'll want to turn it into an JArray


renderEntity :: Entity -> JValue
renderEntity entity = JObject [("firstName", JString $ firstName entity)]


renderEntities :: [Entity] -> JValue
renderEntities xs = JArray (map renderEntity xs)


doTheStringThing :: JValue -> String
doTheStringThing j = renderJValue j
