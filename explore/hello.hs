module Main where

addition a b = a + b
u = [1..10]

main :: IO ()
main = do let z = addition 5 3
          putStrLn $ "The result is: " ++ show z

--main = putStrLn "Hello, World!"
