{- File for functions that don't really belong anywhere else in the project.
 - Author: Victor Zamora
 -}
module Util where

import Data.Sort
import Math.NumberTheory.Logarithms
import System.Random

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

{- Random sample of a list.
 - First argument is the seed to be used by pseudo-random number generator.
 - Second argument the number of elements to get. Depending on the values returned by
 - the RNG, the sample may be smaller than the number of specified elements.
 - Third argument is the list to get the random sample from. -}
randomSample :: Int -> Int -> [a] -> [a]
randomSample _ 0 _ = []
randomSample _ _ [] = []
randomSample seed n l = let gen = mkStdGen seed
                            indices = randomRs (0, length(l) -1) gen
                        in getElems (take n indices) l

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

-- | 'sizeOf' returns the size of an 'Integer'.
sizeOf :: Integer -> Int
sizeOf 0 = 1
sizeOf i = 1 + (integerLog2 i)
