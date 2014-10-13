import Data.Maybe


type Name = String

data Employee = Employee { name :: Name, phone :: String }
    deriving Show





maybeEmp :: Maybe Name -> Maybe String -> Maybe Employee
maybeEmp Nothing _ = Nothing
maybeEmp _ Nothing = Nothing
maybeEmp (Just name) (Just phone) = Just (Employee name phone)


-- This function ignores g, because it is encapsulated in maybeEmp
converter :: (Name -> String -> Employee) -> (Maybe Name -> Maybe String -> Maybe Employee)
converter g = maybeEmp


--listEmp :: (Name -> String -> Employee) -> ([Name] -> [String] -> [Employee])
--listEmp

