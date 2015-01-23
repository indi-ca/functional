module TrivialMonad where

-- It's all about transforming the contents,
-- without letting the user unwrap things



data W a = W a
    deriving Show


instance Functor W where
    fmap g (W x) = W (g x)


b = W 3

something = fmap (+1) b


-- there is something that I cannot do
-- But I am doing it
f :: Int -> W Int
f x = W (x + 1)



-- Next
-- I want to apply an underlying operation twice

