

-- How do I give these aliases?

data Square = Square Integer deriving (Show)
data Circle = Circle Integer deriving (Show)
data Triangle = Triangle Integer deriving (Show)


class Shape a where

    area :: a -> Float


instance Shape Square where
    area (Square x) = fromIntegral x * fromIntegral x



square_one = Square 8
