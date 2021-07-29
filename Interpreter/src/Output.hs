{- |
Module:      Output
Description: This library is for transforming a number into an output
             binary 'String'.
Mantainer:   agua@ciencias.unam.mx
-}

module Output where

import Data.Char (intToDigit)
import Data.Maybe
import Math.NumberTheory.Logarithms
import Numeric (showIntAtBase)

{- | 'natIntoString' takes a natural number "n" and obtains the nth binary
     string in canonical order.
     This function returns a 'Tuple' where the first element is the binary
     number corresponding to the string, and the second parameter is the
     number of bits to use in the binary number's representation as a
     'String'.
     For example, if the first element of the tuple is 2 and the second
     element is 8, we would be representing 10 (2 in binary) in 8 bits,
     which would give us the 'String' "00000010".
     If the parameter passed to this function is negative, the tuple
     '(-1, 0)' is returned, indicating an error.
-}

natIntoString :: Integer -> (Integer, Int)
natIntoString 0 = (0, 1)
natIntoString n = if n > 0
                  then let l = integerLog2 (n+2)
                       in let c = n - (2^l) + 2
                          in (c, l)
                    else (-1, 0)

{- | 'getStringFromTuple' transforms the tuple returned by 'natIntoString'
     into a 'String' composed of 0's and 1's.
     When the error tuple ('(-1, 0)' by our convention) is passed to this
     function, the 'String' "err" is returned, indicating an error. In
     practice, this indicates that the program did not halt.
-}
getStringFromTuple :: (Integer, Int) -> String
getStringFromTuple (n, m)
  | m <= 0 = "err"
  | n == 0 = replicate m '0'
  | otherwise = replicate (m - (integerLog2 n) - 1) '0' ++ stringn
  where stringn = showIntAtBase 2 intToDigit n ""

{- | 'concatOutput' is implemented as an alternative function for getting
     a program's output. The function concatenates all binary
     representations of the numbers in a state 'List'.

-}
concatOutput :: ([(Int, Integer)], Int) -> String
concatOutput ([], _) = "Ɛ"
concatOutput s = if (fromMaybe 0 (lookup 0 (fst s))) < 0
                 then "err"
                 else concatOutputAux s


concatOutputAux :: ([(Int, Integer)], Int) -> String
concatOutputAux ([], _) = ""
concatOutputAux ((x:xs), size) = (showIntAtBase 2 intToDigit (snd x) "") ++
                                 (concatOutputAux (xs, size))
