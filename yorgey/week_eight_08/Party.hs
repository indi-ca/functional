
module Party where

import Data.Monoid
import Data.Tree(flatten)
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

emp_a = Emp "Stan" 9
emp_b = Emp "Sarah" 17

glEmpty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons x@(Emp _ f1) (GL xs f2) = GL (x : xs) (f1 + f2)


glA = glCons emp_a glEmpty
glB = glCons emp_b glEmpty

--something = foldr glCons (flatten testCompany) glEmpty


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
-- moreFun :: GuestList -> GuestList -> GuestList

-- which takes two GuestLists and returns whichever one of them is more fun,
-- i.e. has the higher fun score.
-- (If the scores are equal it does not matter which is returned.)


--instance Ord GuestList where

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

-- treeFold :: ... -> Tree a -> b
-- (See if you can figure out what type(s) should replace the dots in the type of treeFold.
-- If you are stuck, look back at the lecture notes from Week 7,
-- or infer the proper type(s) from the remainder of this assignment.)


