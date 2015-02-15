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

-- When you define the increment and wrapping function, f :: Int -> W Int, you say that we can't do this. However, I can define this in Haskell. Is this a typo?

f :: Int -> W Int
f x = W (x + 1)



-- Next
-- I want to apply an underlying operation twice

-- What are the options?
-- apply f twice? what will happen then?
-- I don't want to wrap it twice
-- I don't want to add 1 to a wrapped value

-- can I fmap it twice?

-- we don't want people to unwrap the result
-- need to provide a higher order function that does the unwrapping and application for us


bind :: (a -> W b) -> (W a -> W b)
bind g (W x) = g x


c = bind f (f 1)
d = bind f (bind f (f 1))



-- (1) define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y).
-- Obviously that definition won't do - the left hand side has a W y pattern so it's actually unwrapping.
-- Rewrite this function so that the only unwrapping that happens is carried out by bind.

-- (2) define a function h :: W Int -> W Int -> W Int so that h (W x) (W y) = W (x+y). Again, no unwrapping.

-- I'm hoping that after you've done these exercises you'll see how you can still work freely with data even though it's wrapped.
