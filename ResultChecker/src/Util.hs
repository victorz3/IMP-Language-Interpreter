{- File for functions that don't really belong anywhere else in the project.
 - Author: Victor Zamora
 -}
module Util where

import System.Random
import Data.Sort

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

-- Gets the elements with indices indicated by the first list.  
getElems :: [Int] -> [a] -> [a]
getElems l1 l2 = getElemsAux 0 (removeDups (sort l1)) l2

{- Auxiliary function for getElems. This function saves a counter of how many elements
 - were removed from the second list. -}
getElemsAux :: Int -> [Int] -> [a] -> [a]
getElemsAux _ [] _ = []
getElemsAux _ _ [] = []
getElemsAux n (x:l1) (y:l2) = if (x-n == 0)
                              then y:(getElemsAux (n+1) l1 l2)
                              else getElemsAux (n+1) (x:l1) l2

-- Random sample of a list
randomSample :: Int -> Int -> [a] -> [a]
randomSample _ 0 _ = []
randomSample _ _ [] = []
randomSample seed n l = let gen = mkStdGen seed
                            indices = randomRs (0, length(l) -1) gen
                        in getElems (take n indices) l
