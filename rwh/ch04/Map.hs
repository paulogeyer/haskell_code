import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []

square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square []     = []

square2 xs = map squareOne xs
  where squareOne x = x * x

upperCase2 xs = map toUpper xs

myMap :: (a -> b) -> [a] -> [b]

myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []
