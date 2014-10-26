


k :: String -> Int
k _ = 3


m :: Int -> Float
m x = a / 2
    where a = fromIntegral x :: Float


b = fmap m k
