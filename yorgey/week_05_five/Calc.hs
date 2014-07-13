{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM



-- EXERCISE 1

-- This type is capable of representing expressions involving integer constants, addition, and multiplication. For example, the expression (2 + 3) × 4 would be represented by the value
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Write Version 1 of the calculator: an evaluator for ExprT, with the signature
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.


eval :: ExprT -> Integer
eval (ExprT.Add (Lit x) (Lit y)) = x + y
eval (ExprT.Add x (Lit y)) = eval x + y
eval (ExprT.Add (Lit x) y) = x + eval y
eval (ExprT.Add x y) = eval x + eval y

eval (ExprT.Mul (Lit x) (Lit y)) = x * y
eval (ExprT.Mul x (Lit y)) = eval x * y
eval (ExprT.Mul (Lit x) y) = x * eval y
eval (ExprT.Mul x y) = eval x * eval y




-- EXERCISE 2

-- Parser.hs exports parseExp, which is a parser for arithmetic expressions
-- passing the constructors of ExprT to it as arguments
-- it will convert Strings representing arithmetic expressions into values
-- of type ExprT

-- *Calc> parseExp Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

-- *Calc> parseExp Lit Add Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))

-- *Calc> parseExp Lit Add Mul "2+3*"
-- Nothing



-- Implement a value added function
-- which evaluates a String,
-- producing Nothing for inputs which are not well-formed
-- and Just n for well-formed inputs that evaluate to n
evalStr' :: Maybe ExprT -> Maybe Integer
evalStr' Nothing = Nothing
evalStr' (Just x) = Just (eval x)

evalStr :: String -> Maybe Integer
evalStr str = evalStr' result
    where result = parseExp Lit ExprT.Add ExprT.Mul str



-- EXERCISE 3

-- Decision to abstract away the properties of ExprT with a type class
-- Create a type class called Expr with three methods
-- lit, add and mul
-- which parallel the constructors of ExprT

-- Make an instance of Expr for the ExprT type
-- in such a way that
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
--   == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Think carefully about what types lit, add and mul should have
-- it may be helpful to consider the types of the ExprT constructors
-- by typing :t Lit




-- What are the types that I'm dealing with
-- I think I'm only dealing with one type
-- With different values (Lit, Add, Mul)
-- I have something like this:

 --data ExprT  = Lit Integer
 --            | Add ExprT ExprT
 --            | Mul ExprT ExprT
 --  deriving (Show, Eq)


-- The point of our Expr type class is that we can now write down arithmetic expressions once
-- and have them interpreted in various ways just by using them at various types.

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


-- EXERCISE 4

instance Expr ExprT where
    lit x = Lit x
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y


instance Expr Integer where
    lit x = x
    add = (+)
    mul = (*)



-- i think what this is about is,
-- the result of the calculation is interpreted as True or False
--
-- interpreted as False: every literal value less than or equal to 0
-- False :=: lit x <= 0
-- interpreted as True: all positive Integers
-- True :=: positive Integers
-- and all positive Integers are interpreted as True; “addition” is logical or, “multiplication” is logical and
-- addition: is, logical or
-- multiplication: is, logical and


instance Expr Bool where
    lit x
        | x <= 0    = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y


newtype MinMax  = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)


-- “addition” is taken to be the max function,
-- while “multiplication” is the min function
instance Expr MinMax where
    lit x = MinMax x
    add x y = max x y
    mul x y = min x y


-- all values should be in the ranage 0 . . . 6,
-- and all arithmetic is done modulo 7; for example,
-- 5 + 3 = 1.
instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)


-- there is a good reason why I can only write this is GHCI
--example = mul (add (lit 2) (lit 3)) (lit 4)

-- GHCI tells me that this is Maybe ExprT
--testExp :: Expr a => Maybe a
--testExp = parseExp lit add mul "(3 * -4) + 5"

--testInteger = testExp :: Maybe Integer


-- If I just get the type in GHCI
-- parseExp lit add mul "(3 * -4) + 5" :: Maybe ExprT

testExp :: Expr a => Maybe a
--testExp = parseExp lit add mul "(3 * -4) + 5"
--testExp = parseExp lit add mul "5 * ((3 * -4) + (3 * -4))"
testExp = parseExp lit add mul "(3 * -4) + (9 * -5)"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7


reify :: ExprT -> ExprT
reify = id


-- Start here:
-- parseExp
--  :: (Integer -> a)
--     -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a

-- :t parseExp Lit ExprT.Add ExprT.Mul "(2+3)*4" :: Maybe ExprT
-- :t parseExp lit add mul "(3 * -4) + 5" :: Expr a => Maybe a
-- the second one takes any type. well, it's how it is defined

-- now how do I fix the type?




-- EXERCISE 5

-- StackVM.hs is a software simulation of the custom CPU.
-- The CPU supports six operations
-- the StackExp data type embodies this:

-- data StackExp = PushI Integer
--               | PushB Bool
--               | Add
--               | Mul
--               | And
--               | Or
--                 deriving Show

-- type Program = [StackExp]


-- There is a stack
-- PushI (stores Integer) and PushB (stores Bool) push values onto the top of the stack,

-- Add, Mul, And, and Or each pop the top two items off the top of the stack, perform the appropriate operation,
-- and push the result back onto the top of the stack.

-- For example, executing the program
-- [PushB True, PushI 3, PushI 6, Mul]
-- will result in a stack holding True on the bottom, and 18 on top of that.

-- Silicon goo can happen if
-- * If there are not enough operands on top of the stack,
-- * or if an operation is performed on operands of the wrong type

-- For a more precise specification of the capabilities and behavior of the custom CPU, consult the reference implementation provided in StackVM.hs.

-- Your task is to implement a compiler for arithmetic expressions.

-- Simply create an instance of the Expr type class for Program, so that arithmetic expressions can be interpreted as compiled programs.
-- For any arithmetic expression exp :: Expr a => a it should be the case that

-- stackVM exp == Right [IVal exp]

-- Finally, put together the pieces you have to create a function
-- compile :: String -> Maybe Program

-- which takes Strings representing arithmetic expressions and compiles
-- them into programs that can be run on the custom CPU.



instance Expr Program where
    lit x = [PushI x]
    add x y = [StackVM.Add] ++ x ++ y
    mul x y = [StackVM.Mul] ++ x ++ y

testStackExp :: Expr a => Maybe a
testStackExp = parseExp lit add mul "(3+4)*(2+7)"
testStack  = testStackExp :: Maybe Program


compile :: String -> Maybe Program
compile arithmetic = expression :: Maybe Program
                     where expression = parseExp lit add mul arithmetic

-- I have instances of class methods which respond to type Integer, and type Program




-- EXERCISE 6
-- Some users of your calculator have requested the ability to give
-- names to intermediate values and then reuse these stored values later.


-- To enable this, you first need to give arithmetic expressions the ability to contain variables. Create a new type class HasVars a which contains a single method var :: String -> a. Thus, types which are instances of HasVars have some notion of named variables.


