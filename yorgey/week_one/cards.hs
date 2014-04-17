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



--result = toDigits (-1)
result = toDigitsRev (12345)

main :: IO ()
main = do let z = result
          putStrLn $ "Play: " ++ show result



