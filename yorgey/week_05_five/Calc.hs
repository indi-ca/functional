module Calc where

import ExprT



-- EXERCISE 1

--Write Version 1 of the calculator: an evaluator for ExprT, with the signature
--For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.


--class Addable a where
--    (+) :: a -> a -> Integer

--instance Addable ExprT where
--    (F x1) + (G x2) = x1 + y1

eval :: ExprT -> Integer
eval (Add (Lit x) (Lit y)) = x + y
eval (Add x (Lit y)) = eval x + y
eval (Add (Lit x) y) = x + eval y
eval (Add x y) = eval x + eval y

eval (Mul (Lit x) (Lit y)) = x * y
eval (Mul x (Lit y)) = eval x * y
eval (Mul (Lit x) y) = x * eval y
eval (Mul x y) = eval x * eval y

