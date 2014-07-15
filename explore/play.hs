import qualified Data.Map as M
--import Data.Map

import Data.Maybe

--b = M.Map 3 5
--b = Tip

f :: M.Map String Integer -> Maybe Integer
f Tip = Just 3

ggg :: Integer -> Maybe Integer
ggg x = Just x

multiThree :: (Num a) => a -> (a -> (a -> a))
multiThree x y z = x * y * z

result = multiThree 3 4 5


--someFunction :: a -> a
--someFunction i = i + 9
--someFunction i = i ++ "bob"


squarish :: Num a => a -> a -> a
squarish i x = i + x * x

cubish :: Num a => a -> a -> a
cubish i x = i + x * x * x


fn_builder :: (Integer -> Integer -> Integer, Integer -> Integer -> Integer)
fn_builder = (squarish, cubish)






-- This is a type constructor and a data constructor
--data MyEntity = Entity Int String
--                deriving (Show)

data MyEntity = Entity {
    primaryID  :: Integer,
    entityName :: String
    } deriving (Show)

-- using application syntax
-- myEntity = Entity 1 "This is an entity"

-- using record syntax
myEntity = Entity {
    primaryID = 1,
    entityName = "NGFW"
    }

mySecondEntity = Entity {
    primaryID = 2,
    entityName = "Mock"
    }

fn :: MyEntity -> String
fn x = entityName x ++ " bob"


-- I want a list of entities
entities = [myEntity, mySecondEntity ]


-- Maybe I want to create a tree of entities ?
-- How about a list of trees?



--myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs then xs
              else myDrop (n-1) (tail xs)



data BookInfo = Book Int String [String]
                deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
            ["Richard Bird", "Oege de Moor"]


data MyBool = False | True

data Fruit = Apple | Orange




-- Have to do something so that ambigious imports don't bother me

class Ideas a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    --x == y = not (x /= y)



main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result
