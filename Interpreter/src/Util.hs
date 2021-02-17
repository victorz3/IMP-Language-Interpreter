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
