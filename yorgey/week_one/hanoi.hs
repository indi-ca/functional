-- Towers of Hanoi


--To move n discs (stacked in increasing size)
-- from peg a to peg b using peg c as temporary storage,

--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String
type Move = (Peg, Peg)


-- should return a list of moves
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
--
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 s d _ = [(s, d)]
hanoi n s d t = hanoi (n-1) s t d ++ hanoi 1 s d t ++ hanoi (n-1) t d s


result = hanoi 3 "a" "b" "c"
