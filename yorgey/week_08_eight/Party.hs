
module Party where

import Data.Monoid
import Data.Tree as Tree
import Employee


-- [?] My monoid was too simple
-- [?] Should I not be deriving Ord to order?



-- maximise the amount of fun
-- determine who to invite

-- Employee names are represented by Strings.
-- type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
-- type Fun  = Integer

-- An Employee consists of a name and a fun score.
-- data Employee = Emp { empName :: Name, empFun :: Fun }
--   deriving (Show, Read, Eq)


-- testCompany :: Tree Employee

-- Guest List
-- Obvious way would be [Employee]
-- But don't want to recompute Fun scores every time
-- GuestList contais both a list of Employees and a Fun score
-- Values of type GuestList should always satisfy the invariant that
-- the sum of all the Fun scores in the list of Employees should be
-- equal to the one, “cached” Fun score.

-- data GuestList = GL [Employee] Fun
--   deriving (Show, Eq)

-- instance Ord GuestList where
--   compare (GL _ f1) (GL _ f2) = compare f1 f2




-- Define tools for working with GuestLists

-- EXERCISE 1.1
-- glCons :: Employee -> GuestList -> GuestList

-- which adds an Employee to the GuestList
-- (updating the cached Fun score appropriately).

-- Of course, in general this is impossible:
-- the updated fun score should depend on whether the Employee being added is already in the list,
-- or if any of their direct subordinates are in the list, and so on.
-- For our purposes, though, you may assume that none of these special cases will hold:
-- that is, glCons should simply add the new Employee and
-- add their fun score without doing any kind of checks.

glEmpty = GL [] 0


glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp _ f1) (GL xs f2) = GL (x : xs) (f1 + f2)


glPop :: GuestList -> GuestList
glPop (GL [] _) = GL [] 0
glPop (GL (x:xs) f) = GL xs (f - empFun x)



-- EXERCISE 1.2
-- A Monoid instance for GuestList.
-- (How is the Monoid instance supposed to work, you ask? You figure it out!)

-- 2 Note that this requires creating an
-- “orphan instance” (a type class instance instance C T which is defined in a
-- module which is distinct from both the
-- modules where C and T are defined),
-- which GHC will warn you about.
-- You can ignore the warning, or add
-- {-# OPTIONS_GHC -fno-warn-orphans #-} to the top of your file.

instance Monoid GuestList where
    mempty = GL [] 0
    mappend x@(GL l1 f1) y@(GL l2 f2) = GL (l1 ++ l2) (f1 + f2)




-- EXERCISE 1.3

-- takes two GuestLists and returns whichever one of them is more fun,
-- (If the scores are equal it does not matter which is returned.)

-- instance Ord GuestList is defined in Employee.hs
-- seems to have no effect on moreFun

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL l1 f1) y@(GL l2 f2) = if f1 > f2 then x else y





-- EXERCISE 2

-- The Data.Tree module from the standard Haskell libraries defines
-- the type of “rose trees”, where each node stores a data element and
-- has any number of children (i.e. a list of subtrees):

-- data Tree a = Node {
--          rootLabel :: a,         -- label value
--          subForest :: [Tree a]   -- zero or more child trees
-- }
-- Strangely, Data.Tree does not define a fold for this type!
-- Rectify the situation by implementing

treeFold :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
treeFold z f (Node x []) = z x
treeFold z f (Node x ys) = f x ( map (treeFold z f) ys )

-- (See if you can figure out what type(s) should replace the dots in the type of treeFold.
-- If you are stuck, look back at the lecture notes from Week 7,
-- or infer the proper type(s) from the remainder of this assignment.)




-- THE ALGORITHM

-- Now let’s actually derive an algorithm to solve this problem.
-- Clearly there must be some sort of recursion involved
-- in fact, it seems that we should be able to do it with a fold.

-- This makes sense though starting from the bottom of the tree and
-- working our way up, we compute the best guest list for each subtree and
-- somehow combine these to decide on the guest list for the next level up, and so on.

-- So we need to write a combining function
-- combineGLs :: Employee -> [GuestList] -> GuestList

-- which takes an employee (the boss of some division) and
-- the optimal guest list for each subdivision under him,
-- and somehow combines this information to compute the best guest list
-- for the entire division.

-- However, this obvious first attempt fails!
-- The problem is that we don’t get enough information from the recursive calls.

-- If the best guest list for some subtree involves inviting that subtree’s boss,
-- then we are stuck,
-- since we might want to consider inviting the boss of the entire tree

-- in which case we don’t want to invite any of
-- the subtree bosses (since they wouldn’t have any fun anyway).

-- But we might be able to do better than just taking the best possible
-- guest list for each subtree and then excluding their bosses.

-- The solution is to generalize the recursion to compute more information,
-- in such a way that we can actually make the recursive step.

-- In particular, instead of just computing the best guest list for a given tree,
-- we will compute two guest lists:

-- 1. the best possible guest list we can create if we invite the boss
-- (that is, the Employee at the root of the tree); and
-- 2. the best possible guest list we can create if we don’t invite the boss.

-- It turns out that this gives us enough information at each step to
-- compute the optimal two guest lists for the next level up.



-- EXERCISE 3

-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)

-- which takes two arguments.
-- The first is the “boss” of the current subtree (let’s call him Bob).
-- The second argument is a list of the results for each subtree under Bob.

-- Each result is a pair of GuestLists:
-- the first GuestList in the pair is the best possible guest list
-- with the boss of that subtree;
-- the second is the best possible guest list without the boss of that subtree.

-- nextLevel should then compute the overall best guest list that includes Bob,
-- and the overall best guest list that doesn’t include Bob.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (glCons boss $ h glPop xs, h id xs)
    where
        h f = mconcat . map (\x -> moreFun (f $ fst x) (snd x))



-- EXERCISE 4

-- which takes a company hierarchy as input and outputs a fun-maximizing guest list.
-- You can test your function on testCompany, provided in Employee.hs.

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold z nextLevel tree
    where
        z x = (glCons x glEmpty, glEmpty)


-- The whole company

-- Of course, the actual tree of employees in your company is much larger!
-- We have provided you with a file, company.txt, containing the entire hierarchy for your company.
-- The contents of this file were created by calling the show function on a Tree Employee,
-- so you can convert it back into a Tree Employee using the read function.


-- EXERCISE 5

-- Implement main :: IO () so that it reads your company’s hierarchy from the
-- file company.txt, and then prints out a formatted guest list,
-- sorted by first name, which looks like

-- Total fun: 23924
-- Adam Debergues
-- Adeline Anselme
-- ...

-- (Note: the above is just an example of the format; it is not the correct output!)
-- You will probably find the readFile and putStrLn functions useful.

-- In other words, your main function should actually be fairly short,
-- calling out to helper functions (whose types do not involve IO) to do most of the work.
-- If you find IO “infecting” all your function types, you are Doing It Wrong.



--readCompany = something <- readFile "company.txt"

main :: IO ()
main = do
    something <- readFile "company.txt"
    putStrLn "done"


--something <- readFile "company.txt"
--let company :: Tree Employee
--    company = read something





