{- File for functions that don't really belong anywhere else in the project.
 - Author: Victor Zamora
 -}
module Util where

-- Natural subtraction
natSub :: Integer -> Integer -> Integer
natSub x y
  | sub < 0 = 0
  | otherwise = sub
  where sub = x-y

-- Removes duplicated elements in a sorted list.
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups [x] = [x]
removeDups (x:y:l) = if x == y
                     then removeDups (y:l)
                     else x:(removeDups (y:l))

-- First element of a 3-tuple
fst :: (a, b, c) -> a
fst (x, _, _) = x

-- Second element of a 3-tuple
snd :: (a, b, c) -> b
snd (_, x, _ ) = x

-- Returns the third element in a 3-tuple.
thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- Returns the size of a String in bytes.
getSizeInBytes :: String -> Int
getSizeInBytes s = 8 * (length s)

