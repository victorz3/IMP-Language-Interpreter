{- |
Module:      Util
Description: Module for functions that don't really belong anywhere else.
Maintainer:  agua@ciencias.unam.mx
-}
module Util where

import Math.NumberTheory.Logarithms

-- | 'natSub' is for doing natural subtraction.
natSub :: Integer -> Integer -> Integer
natSub x y
  | sub < 0 = 0
  | otherwise = sub
  where sub = x-y

-- | 'removeDups removes duplicated elements in a sorted list.
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups [x] = [x]
removeDups (x:y:l) = if x == y
                     then removeDups (y:l)
                     else x:(removeDups (y:l))

-- | 'fst' returns the first element of a 3-tuple.
fst :: (a, b, c) -> a
fst (x, _, _) = x

-- | 'snd' returns the second element of a 3-tuple.
snd :: (a, b, c) -> b
snd (_, x, _ ) = x

-- | 'thrd' returns the third element of a 3-tuple.
thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- | 'getSizeInBytes' returns the size of a 'String' in bytes.
getSizeInBytes :: String -> Int
getSizeInBytes s = 8 * (length s)

-- | 'sizeOf' returns the size of an 'Integer'.
sizeOf :: Integer -> Int
sizeOf i = 1 + (integerLog2 i)
