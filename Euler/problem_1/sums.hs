set = [1..1000-1]
filtered = sum [ x | x <- set, x `mod` 3 == 0 || x `mod` 5 == 0]


main :: IO ()
main = do let z = filtered
          putStrLn $ "The sum of all the multiples of 3 or 5 below 1000: " ++ show filtered
