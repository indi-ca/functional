import Data.List (sortBy)
import Data.Ord (comparing)

-- How do I give these aliases?
-- What I want is a Type synonym
-- Like this

type Radius = Integer

data Square = Square Integer deriving (Eq, Ord, Show)
data Circle = Circle Radius deriving (Eq, Ord, Show)
data Triangle = Triangle Integer deriving (Eq, Ord, Show)


-- a default implementation seems impossible
class Shape a where
    area :: a -> Float

-- Can do stuff like this
class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a


instance Shape Square where
    area (Square x) = fromIntegral x * fromIntegral x

instance Shape Circle where
    area (Circle r) = 2 * pi * fromIntegral (r * r)

instance Shape Triangle where
    area (Triangle x) = fromIntegral (x * x) / 2

-- Instances of collections can also be defined
instance Shape a => Shape [a] where
    area = sum . map area

instance (Shape a, Shape b) => Shape (a, b) where
    area (a, b) = area a + area b

-- What about either or
--type SquareOrCircle = Either Square Circle
instance (Shape a, Shape b) => Shape (Either a b) where
    area (Left a) = area a
    area (Right b) = area b



square_one = Square 5
circle_one = Circle 5
triangle_one = Triangle 5

shapes = [Square 5, Square 15, Square 3]

-- So, now, how can I operate on a type that I don't know
-- Suppose I have another function
findArea :: (Shape a) => a -> Float
findArea shape = area shape


sortByArea :: Shape a => [a] -> [a]
sortByArea = sortBy (comparing area)






