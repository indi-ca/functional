{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char (isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Hint: To parse one or more occurrences of p,
-- run p once and then parse zero or more occurrences of p.
-- To parse zero or more occurrences of p, try parsing one or more;
-- if that fails, return the empty list.


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- First, spaces should parse a consecutive list of zero or more whitespace characters
-- (use the isSpace function from the standard Data.Char module).

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- Next, ident should parse an identifier,
-- which for our purposes will be an alphabetic character (use isAlpha) followed by zero or more alphanumeric characters (use isAlphaNum).
-- In other words, an identifier can be any nonempty sequence of letters and digits,
-- except that it may not start with a digit.

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)



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
