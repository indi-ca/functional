

-- How do I give these aliases?

data Square = Square Integer deriving (Show)
data Circle = Circle Integer deriving (Show)
data Triangle = Triangle Integer deriving (Show)


class Shape a where

    area :: a -> Float


instance Shape Square where
    area (Square x) = fromIntegral x * fromIntegral x

instance Shape Circle where
    area (Circle r) = 2 * pi * fromIntegral (r * r)

instance Shape Triangle where
    area (Triangle x) = fromIntegral (x * x) / 2

square_one = Square 5
circle_one = Circle 5
triangle_one = Triangle 5
