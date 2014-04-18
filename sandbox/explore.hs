

-- shape is type
-- and there are two ways of constructing shapes
-- and one uses the data constructor Circle with takes one parameter Int
-- it can exist in only one of these forms
-- so look for context

-- This is not going to work with the function
data Shape = Circle | Rectangle
--data Shape = Circle Int | Rectangle Int

-- CIRCLE IS NOT A TYPE
-- it is a data constructor


-- it is possible to do pattern matching on it
-- it is matching on how we constructed it
isCircle :: Shape -> Bool
isCircle Circle = True
isCircle Rectangle = False



-- perhaps, to use the Alpha constructor,
-- then I need to provide a function


f1 :: Int -> String
f1 1 = "Bob"



-- curlies area for record syntax
data Person =
    Alpha {
        firstName :: String,
        func_f :: Int -> String
    }
    |
    Beta {
        secondName :: String
    }



-- OK, so
-- We have some Types
-- They can exist in different forms
-- Notice how I can use data constructors
-- I cannot have two data constructors with the same
-- A function is produced, which does this:
-- firstName :: Person -> String
-- why is this function useful?

-- I can construct a Person
-- This is how one does it
-- let p = Beta { secondName = "foo "}

-- it is kind of ODD that I cannot use firstName twice....
--data Parser p  = DataConstructor2 {
--    firstName :: Int
--}
