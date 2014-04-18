-- credit cards


--a = [1, 2]
--b = [20, 40, 60]
--result = [ x * y | x <- a, y <- b ]




-- convert positive integers to a list of digits
-- for 0, or negetive inputs, it should return the empty list
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

-- same as above, but with the digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)


-- double evyr other number beginning from the right
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther x
    | length x == 1      = x
    | length x == 2      = [2 * head x, last x]
    | otherwise          = doubleEveryOther (init (init x)) ++ doubleEveryOther [last (init x), last x]


--result = toDigits (-1)

x = toDigitsRev (12345)
--result = doubleEveryOther x
--result = doubleEveryOther [1, 2, 3]
result = doubleEveryOther [1, 2, 3, 4, 5]




main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result



