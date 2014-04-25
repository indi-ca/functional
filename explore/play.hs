

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
    entityName = "This is a new entity"
    }

mySecondEntity = Entity {
    primaryID = 2,
    entityName = "This is my second entity"
    }

fn :: MyEntity -> String
fn x = entityName x ++ " bob"


--myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs then xs
              else myDrop (n-1) (tail xs)



data BookInfo = Book Int String [String]
                deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
            ["Richard Bird", "Oege de Moor"]


data MyBool = False | True

main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result
