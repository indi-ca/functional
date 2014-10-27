


k :: String -> Int
k _ = 3


m :: Int -> Float
m x = a / 2
    where a = fromIntegral x :: Float


b = fmap m k



runParser :: String -> Maybe (Char, String)
runParser _ = Just ('x', "bob")


n :: Maybe (Char, String) -> Maybe (Int, String)
n _ = Just (5, "bob")


bb = fmap n runParser

--instance Functor (String -> Maybe(Char, String)) where
    --fmap g fn =


--instance Functor (Maybe (a, String)) where
--    fmap _ Nothing = Nothing
--    fmap g (Just (a, str)) = Just (g a, str)


-- Duplicate declarations
--instance Functor ((,)e) where
--    fmap g (x, y) = (g x , y)

first :: (a -> b) -> (a,c) -> (b,c)
first g (x, y) = (g x, y)


mm :: (a -> b) -> Maybe (a, String) -> Maybe (b, String)
mm _ Nothing = Nothing
mm g (Just x) = Just (first g x)
