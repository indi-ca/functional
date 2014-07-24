-- Fibonacci numbers
-- TODO: See the Euler problem



-- The Fibonacci numbers Fn are deﬁned as the sequence of integers,
-- beginning with 0 and 1, where every integer in the sequence is the
-- sum of the previous two. That is,
-- F0 = 0
-- F1 = 1
-- Fn = Fn−1 + Fn−2 (n ≥ 2)

-- For example, the ﬁrst ﬁfteen Fibonacci numbers are
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .

-- It’s quite likely that you’ve heard of the Fibonacci numbers before.
-- The reason they’re so famous probably has something to do with the
-- simplicity of their deﬁnition combined with the astounding variety of
-- ways that they show up in various areas of mathematics as well as art
-- and nature.


-- EXERCISE 1

-- Translate the above deﬁnition of Fibonacci numbers directly into a
-- recursive function deﬁnition of type
-- so that fib n computes the nth Fibonacci number Fn.

fib :: Integer -> Integer
fib x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fib (x - 1) + fib (x - 2)

-- Now use fib to deﬁne the inﬁnite list of all Fibonacci numbers,
fibs1 :: [Integer]
fibs1 = map fib positive_integers
    where positive_integers = [0..]



-- (Hint: You can write the list of all positive integers as [0..].)
-- Try evaluating fibs1 at the ghci prompt. You will probably get
-- bored watching it after the ﬁrst 30 or so Fibonacci numbers, because
-- fib is ridiculously slow. Although it is a good way to deﬁne the Fi
-- bonacci numbers, it is not a very good way to compute them—in order
-- to compute Fn it essentially ends up adding 1 to itself Fn times!

-- For example, shown at right is the tree of recursive calls made by evaluating fib 5.
-- As you can see, it does a lot of repeated work. In the end, fib has running time
-- O(Fn), which (it turns out) is equivalent to O(ro squared) where ro = 1 + root 5 / 2
-- is the golden ratio
-- That’s right, the running time is exponential in n.
-- What’s more, all this work is also repeated from each element of the list fibs1 to the next.

-- What is this thing?
-- It is a sequence. It's value is basically the sum of the previous two values.
-- The initial calculation of it does not use the previous value.
-- It does everything again.
-- If there are n items, then it has to do n steps.
-- Each successive element requires exponential growth.






-- EXERCISE 2

-- Your task for this exercise is to come up with more efficient implementation.
-- Specifically, define the infinite list

-- fibs2 :: [Integer]
-- so that it has the same elements as fibs1,
-- but computing the first n elements of fibs2 requires only O(n) addition operations.
-- Be sure to use standard recursion pattern(s) from the Prelude as appropriate.

-- Ok, use the previous two values

-- Consider a reversed list, take the head and the head of the rest


a = [1, 2, 3, 4, 5]
b' = [1, 2, 3, 5, 8]
b = [8, 5, 3, 2, 1]
z = []


sumOfFirstTwo :: [Integer] -> Integer
sumOfFirstTwo (x:xs) = x + head xs

g [] x = [1]
g [1] x = [2, 1]
g acc x = sumOfFirstTwo acc : acc

-- This creates a new list for each new number
-- I just need to produce an infinite list of fib integers
fibs2 :: [[Integer]]
fibs2 = scanl g [] positive_integers
    where positive_integers = [0..]


-- Streams

-- We can be more explicit about infinite lists by defining a type Stream representing lists that must be infinite. (The usual list type represents lists that may be infinite but may also have some finite length.)
-- In particular, streams are like lists but with only a “cons” constructor— whereas the list type has two constructors, [] (the empty list) and
-- (:) (cons), there is no such thing as an empty stream. So a stream is simply defined as an element followed by a stream.


-- Exercise 3
-- • Define a data type of polymorphic streams, Stream.
-- • Write a function to convert a Stream to an infinite list,
--    streamToList :: Stream a -> [a]
-- • To test your Stream functions in the succeeding exercises, it will be useful to have an instance of Show for Streams. However, if you put deriving Show after your definition of Stream, as one usually does, the resulting instance will try to print an entire Stream—which,
-- of course, will never finish. Instead, you should make your own instance of Show for Stream,


--   instance Show a => Show (Stream a) where
--     show ...
-- which works by showing only some prefix of a stream (say, the first 20 elements).

-- Hint: you may find your streamToList function useful.



-- Exercise 4
-- Let’s create some simple tools for working with Streams.
-- • Write a function
--    streamRepeat :: a -> Stream a
-- which generates a stream containing infinitely many copies of the given element.
-- • Write a function
-- streamMap :: (a -> b) -> Stream a -> Stream b which applies a function to every element of a Stream.
-- • Write a function
--    streamFromSeed :: (a -> a) -> a -> Stream a
-- which generates a Stream from a “seed” of type a, which is the first element of the stream, and an “unfolding rule” of type a -> a which specifies how to transform the seed into a new seed, to be used for generating the rest of the stream.



-- Exercise 5
-- Now that we have some tools for working with streams, let’s cre-
-- ate a few:
-- • Define the stream
--    nats :: Stream Integer
-- which contains the infinite list of natural numbers 0, 1, 2, . . .
-- • Define the stream
--    ruler :: Stream Integer
-- which corresponds to the ruler function 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
-- where the nth element in the stream (assuming the first element corresponds to n = 1) is the largest power of 2 which evenly divides n.


-- Hint: define a function interleaveStreams which alternates the elements from two streams. Can you use this function to implement ruler in a clever way that does not have to do any divisibility testing?



-- Fibonacci numbers via generating functions (extra credit)
-- This section is optional but very cool, so if you have time I hope you will try it. We will use streams of Integers to compute the Fibonacci numbers in an astounding way.
-- The essential idea is to work with generating functions of the form a0 +a1x+a2x2 +···+anxn +...
-- where x is just a “formal parameter” (that is, we will never actually substitute any values for x; we just use it as a placeholder) and all the coefficients ai are integers. We will store the coefficients a0, a1, a2, . . . in a Stream Integer.

