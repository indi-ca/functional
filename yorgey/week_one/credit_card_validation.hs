-- credit cards


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


-- the output of doubleEveryOther has a mix of one digit and two digit numbers
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits seq = sum ( [ sum (toDigits x) | x <- seq ] )


-- indicates whether an Integer could be a valid credit card
-- process is
-- first do the doubling
-- add the doubled and undoubled numbers
-- calculate the remainder when divided by 10
-- if the remainder is zero, then the card is valid
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0


result = validate 4012888888881881




main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result



