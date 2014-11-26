{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char (isUpper)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = undefined


-- No Parser data constructor

-- I can see something flowing inside.
-- By simply combining two parsers together,
-- I'm expecting them both the satisfy, otherwise the entire thing
-- is Nothing



--makeList :: a -> a -> [a]
--makeList x y = [x, y]

makeList :: a -> [a]
makeList x = [x]



zeroOrMore' :: Parser a -> Parser [a]
zeroOrMore' p = makeList <$> p
    where
        bob str = runParser p str

--something = runParser (zeroOrMore' (satisfy isUpper)) "ABCdEfgH"
--something = runParser (zeroOrMore' (satisfy isUpper)) "abCdEfgH"
something = runParser ((satisfy isUpper)) "AbCdEfgH"


doSomething :: String -> Parser a -> [a]
doSomething str p = case f of Nothing -> []
                              Just(x, rem) -> x ++ doSomething rem p
    where f = runParser (makeList <$> p) str




oneOrMore :: Parser a -> Parser [a]
oneOrMore p = undefined

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
