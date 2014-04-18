

multiThree :: (Num a) => a -> (a -> (a -> a))
multiThree x y z = x * y * z

result = multiThree 3 4 5


--someFunction :: a -> a
someFunction i = i + 9
someFunction i = i ++ "bob"


squarish :: Num a => a -> a -> a
squarish i x = i + x * x

cubish :: Num a => a -> a -> a
cubish i x = i + x * x * x


fn_builder :: (Integer -> Integer -> Integer, Integer -> Integer -> Integer)
fn_builder = (squarish, cubish)


main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result
