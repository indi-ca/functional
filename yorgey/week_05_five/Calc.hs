module Calc where

import ExprT
import Parser

--class Addable a where
--    (+) :: a -> a -> Integer

--instance Addable ExprT where
--    (F x1) + (G x2) = x1 + y1



-- EXERCISE 1

-- This type is capable of representing expressions involving integer constants, addition, and multiplication. For example, the expression (2 + 3) × 4 would be represented by the value
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Write Version 1 of the calculator: an evaluator for ExprT, with the signature
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.


eval :: ExprT -> Integer
eval (Add (Lit x) (Lit y)) = x + y
eval (Add x (Lit y)) = eval x + y
eval (Add (Lit x) y) = x + eval y
eval (Add x y) = eval x + eval y

eval (Mul (Lit x) (Lit y)) = x * y
eval (Mul x (Lit y)) = eval x * y
eval (Mul (Lit x) y) = x * eval y
eval (Mul x y) = eval x * eval y




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
evalStr :: String -> Maybe Integer
evalStr str = evalStr' result
    where result = parseExp Lit Add Mul str

evalStr' :: Maybe ExprT -> Maybe Integer
evalStr' Nothing = Nothing
evalStr' (Just x) = Just (eval x)



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


class Expr a where
    lit :: a -> ExprT
    add :: a -> a -> ExprT
    mul :: a -> a -> ExprT

instance Expr Integer where
    lit x = Lit x
    add x y = Add (Lit x) (Lit y)
    mul x y = Mul (Lit x) (Lit y)

instance Expr ExprT where
    lit x = x
    add x y = Add x y
    mul x y = Mul x y







