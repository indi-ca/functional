
module Party where

import Data.Monoid
import Employee

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
glCons x@(Emp name f1) (GL xs f2) = GL (x : xs) (f1 + f2)

-- EXERCISE 1.2
-- A Monoid instance for GuestList.2 (How is the Monoid instance supposed to work, you ask? You figure it out!)



-- EXERCISE 1.3
-- moreFun :: GuestList -> GuestList -> GuestList

-- which takes two GuestLists and returns whichever one of them is more fun,
-- i.e. has the higher fun score.
-- (If the scores are equal it does not matter which is returned.)







