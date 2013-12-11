module Main where

addition a b = a + b
u = [1..10]


-- x is the feature matrix
-- We define x0 = 1 for convenience

x = 1 : [1..10]

-- n is the number of features
let n = length x

main :: IO ()
main = do let z = addition 5 3
          putStrLn $ "The result is: " ++ show z

--main = putStrLn "Hello, World!"
