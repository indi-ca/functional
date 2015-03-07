module RPM where


import Control.Applicative ((<*>), (*>), (<*), (<$>), (<$))

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

--import Text.Parsec.Numbers


sample = "mslync-29.5-42.i686.rpm"


data Release = Release {
    major :: Int,
    minor :: Int
} deriving Show

data RPM = RPM {
    name :: String,
    release :: Release,
    index :: Int,
    architecture :: String
} deriving Show



---- A release most probably could have more than major and minor numbers
--parseRelease :: Parser Release
--parseRelease = do
--    d1 <- many digit
--    char '.'
--    d2 <- many digit
--    return $ (d1, d2)
    --return $ Release (read d1 :: Int) (read d2 :: Int)


something :: Parser (Char, Char)
something = do
    char 'a'
    n <- letter
    string "->"
    e <- letter
    return $ (n, e)


-- Learning section
-- well, i cannot seem to put anything that is not a string
stringParser :: Parsec String st String
stringParser = many anyChar


wordParser :: Parsec String st String
wordParser = many $ noneOf [' ']

secondWordParser :: Parsec String st String
secondWordParser = wordParser *> (char ' ') *> wordParser


twoWordsParser :: Parsec String st [String]
twoWordsParser = listify <$> wordParser <*> ((char ' ') *> wordParser)
    where listify a b = [a, b]


dogCatParser:: Parsec String st String
dogCatParser = (string "dog") <|> (string "cat")


-- Some need for the try combinator
-- Because I try cat, and it fails because it is not camel
-- It has a common prefix
-- It started to match the camel parser,
-- but consumed the "ca" characters
camelCatParser:: Parsec String st String
camelCatParser = (string "camel") <|> (string "cat")


-- so, what to do?
-- the try combinator does not consume any input if it fails
camelCatTryParser:: Parsec String st String
camelCatTryParser = try (string "camel") <|> (string "cat")


numberParser :: Parsec String str Int
numberParser = read <$> (many $ oneOf "0123456789")


-- This doesn't really work for me
test' p = parse
--test = parseTest something "ab->x"

--test = parseTest dogCatParser "dog"
test'' = parseTest numberParser


