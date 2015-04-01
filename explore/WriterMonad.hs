module WriterModule where



-- What's going on?
-- I have a function that let's me decide things
-- Why do I need an applyLog?
-- Well, I need an application function
-- Because I have a history, and I want to create a future
-- However, not quite
-- Because, my past is of type Integer (antes)
-- and my future, is of type Bool (despues)

main :: IO ()
main = undefined



applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' = undefined


-- K, I need to pull tho inner string out and join it with the second
applyLog :: (Int, [Char]) -> (Int -> (Bool, [Char])) -> (Bool, [Char])
applyLog (v, log) df = let (y, log') = df v in (y, log ++ log')

-- Remember how the let expression looks:
--      pattern = let intermediate = source in target


-- OK. Here is a new problem
-- A function is evaluated
-- I *know* that the function returns a tuple
-- I want something inside that tuple
-- I'm pretty sure that I can pattern match on it
-- I'm thinking of using where statement with an @pat something
-- But mostly probably what I really need is the let clause
-- that I don't really understand how to use



-- So, there is a decision function somewhere around here

-- [?] Something that I am yet to understand.
-- And when can I use the actual type?
-- This is what I would like to do:
-- decider :: a -> (b, [Char])
-- but, now I'm going to resort to
-- decider :: Int -> (Bool, [Char])

decider :: Int -> (Bool, [Char])
decider x
    | x < 9     =  (False, "less than")
    | otherwise =  (True, "greater than")



--test = decider 7
test = applyLog (7, "this is the past") decider
