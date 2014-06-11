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
evalStr x
    | result == Nothing = Nothing
    | result x y = eval result
    where result = parseExp Lit Add Mul x

-- How is Maybe defined again?
-- data Maybe a = Nothing | Just a

