import System.IO
import System.Process
import Data.String(lines)

import SimpleJSON
import PutJSON



main :: IO ()
main = do
    putStrLn "hello"
    inputHandle <- openFile "output.txt" ReadMode

    --bob <- hGetLine inputHandle
    bob <- hGetContents inputHandle
    putStrLn bob

    -- Unnecessary to close, because it is automatically closed for me
    --hClose inputHandle




    outputHandle <- openFile "output.txt" WriteMode
    --inpStr <- getLine
    something entities outputHandle

    hClose outputHandle
    putStrLn "done"




--someLoop :: Handle -> IO String
--someLoop inh =
--    do  ineof <- hIsEOF inh
--        if ineof
--            then return ()
--            else do inpStr <- hGetLine inh
--                    return inpStr
--                    someLoop inh


data Entity = Entity { firstName :: String }
    deriving Show

myEntity = Entity "Darrly"
myEntityB = Entity "Goashes"

entities = [myEntity, myEntityB]

something :: [Entity] -> Handle -> IO ()
something entities handle = hPutStrLn handle $ renderEntitiesCSV $ entities



-- I've got an entity, and I want to turn it into a JObject
-- I'm going to have a list of entities, and I'll want to turn it into an JArray


renderEntityJSON :: Entity -> JValue
renderEntityJSON entity = JObject [("firstName", JString $ firstName entity)]

renderEntitiesJSON :: [Entity] -> String
renderEntitiesJSON xs = renderJValue $ JArray (map renderEntityJSON xs)




renderEntityCSV :: Entity -> String
renderEntityCSV entity = firstName entity

renderEntitiesCSV :: [Entity] -> String
renderEntitiesCSV xs = foldr1 (++) $ map (\x -> renderEntityCSV x ++ "\n") xs


readCSV :: String -> [Entity]
readCSV str = map (\x -> Entity x) $ lines str




