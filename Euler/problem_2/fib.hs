set = [1..1000-1]
u = 4000000


s :: [Int] -> Int
s x = head x + x !! 1


fib :: [Int] -> [Int]
fib x = if s x < u+1 then fib(s x : x) else x

filtered = sum [x | x <- fib [2, 1], even x ]


main :: IO ()
main = do let z = filtered
          putStrLn $ "The sum of sequence: " ++ show filtered
