module Main where

addition a b = a + b


-- x is the feature matrix
-- We define x0 = 1 for convenience

x = 1 : [1..10]
y = 1 : [0, 10..100]

-- n is the number of features
n = length x


triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]



main :: IO ()
main = do let z = rightTriangles'
          putStrLn $ "The result is: " ++ show z
