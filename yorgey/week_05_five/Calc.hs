module Calc where

import ExprT



-- EXERCISE 1

--Write Version 1 of the calculator: an evaluator for ExprT, with the signature
--For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
eval :: ExprT -> Integer
