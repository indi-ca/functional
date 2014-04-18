set = [1..1000-1]
--filtered = sum [ x | x <- set, x `mod` 3 == 0 || x `mod` 5 == 0]

--bobfib = 1 : 1 : [ a+b | (a,b) <- zip bobfib (tail bobfib) ]


u = 4000000

s :: [Int] -> Int
s x = head x + x !! 1


fib :: [Int] -> [Int]
fib x = if s x < u+1 then fib(s x : x) else x


fiba :: Int -> Int
fiba 0 = 1
fiba 1 = 1
fiba n = fiba (n-1) + fiba (n-2)


addVectors :: (Integer) -> (Integer) -> (Integer)
addVectors x y = (fst x + fst y, snd x + snd y)




--filtered = [ fib x | x <- [1..u]]
--filtered = fibpend [3, 2, 1]
--filtered = fib [2, 1]
--filtered = sum [x | x <- fib [2, 1], even x ]

filtered = addVectors (3, 2) (4, 5)

main :: IO ()
main = do let z = filtered
          putStrLn $ "The sum of sequence: " ++ show filtered
