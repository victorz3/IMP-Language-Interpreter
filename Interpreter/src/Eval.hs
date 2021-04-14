{- |
Module:      Eval
Description: This library is for evaluating programs in our language (IMP).
Mantainer:   agua@ciencias.unam.mx
-}

module Eval where

import Data.Char (intToDigit)
import Data.Maybe
import Language
import Math.NumberTheory.Logarithms
import Numeric (showIntAtBase)
import qualified Util

{- | 'maxSteps' contains the default value for the maximum number of steps
     that the interpreter should run for.
-}
maxSteps = 10000

-- | 'State' represents a memory state using lists.
type State = [(Int, Integer)]

{- | 'getValue' gets the value in a given location of the 'State'.
     If the value is not in the 'State', '0' is returned.
-}
getValue :: State -> Int -> Integer
getValue l i = fromMaybe 0 (lookup i l)
                 
-- | 'replace' replaces a value within a 'State'.
replace :: Int -> State -> Integer -> State
replace n [] i = [(n, i)]
replace n ((a, b):xs) i = if n == a
                          then ((n, i):xs)
                          else ((a, b):(replace n xs i))

{- | 'simplBoolExp' simplifies easy-to-simplify 'BoolExp's. For example, it
     simplifies ('Equals' X X) to 'T'.
-}
simplBoolExp :: BoolExp -> BoolExp
simplBoolExp (Equals x y) = if x == y
                            then T
                            else (Equals x y)
simplBoolExp (Lessthan x y) = if x == y
                              then F
                              else (Lessthan x y)
simplBoolExp (Not b)
  | b' == T = F
  | b' == F = T
  | otherwise = (Not b')
  where
    b' = simplBoolExp b
simplBoolExp (Or b1 b2)
  | b1' == T = T
  | b2' == T = T
  | b1' == F = b2'
  | b2' == F = b1'
  | otherwise = (Or b1' b2')
  where
    b1' = simplBoolExp b1
    b2' = simplBoolExp b2
simplBoolExp (And b1 b2)
  | b1' == F = F
  | b2' == F = F
  | b1' == T = b2'
  | b2' == T = b1'
  | otherwise = (And b1' b2')
  where
    b1' = simplBoolExp b1
    b2' = simplBoolExp b2
simplBoolExp x = x

{- | 'simplBoolProgram' simplifies some 'BoolExp's within a 'Program' using
     the 'simplBoolExp' function. Then it simplifies 'If' and 'While'
     expressions according to the results of the previous simplification.
-} 
simplBoolProgram :: Program -> Program
simplBoolProgram (Concat p1 p2) = if p1' == NoHalt || p2' == NoHalt
                                  then NoHalt
                                  else (Concat p1' p2')
  where p1' = simplBoolProgram p1
        p2' = simplBoolProgram p2
simplBoolProgram (If b p1 p2)
  | b' == T = p1'
  | b' == F = p2'
  | otherwise = (If b' p1' p2')
  where b' = simplBoolExp b
        p1' = simplBoolProgram p1
        p2' = simplBoolProgram p2
simplBoolProgram (While b p)
  | b' == T = NoHalt
  | b' == F = Skip
  | otherwise = (While b' p')
  where b' = simplBoolExp b
        p' = simplBoolProgram p
simplBoolProgram x = x

{- | 'eval' evaluates a 'Program' without taking the halting program into
     account. Therefore, calls to 'eval' are unsafe and prone to loop
     indefinitely.
-}
eval :: Program -> State -> State
eval Skip s = s
eval (Assign (Loc x) a) s = replace x s (evalArit a s)
eval (Concat p1 p2) s = let sNew = (eval p1 s)
                        in (eval p2 sNew)
eval (If b p1 p2) s = if (evalBool b s)
                      then (eval p1 s)
                      else (eval p2 s)
eval (While b p) s = if (evalBool b s)
                     then let sNew = (eval p s)
                          in (eval (While b p) sNew)
                     else s

{- | 'evalArit' evaluates an 'Arit' without taking the halting problem into
     account.
-}
evalArit :: Arit -> State -> Integer
evalArit (In n) _ = n
evalArit (Mem (Loc x)) s = (getValue s x)   
evalArit (Plus a1 a2) s = (evalArit a1 s) + (evalArit a2 s)
evalArit (Minus a1 a2) s = (evalArit a1 s) - (evalArit a2 s)
evalArit (Times a1 a2) s = (evalArit a1 s) * (evalArit a2 s)
 
{- | 'evalBool' evaluates a boolean expression without taking the halting
     problem into account. -}
evalBool :: BoolExp -> State -> Bool
evalBool T _ = True
evalBool F _ = False
evalBool (Equals a1 a2) s = (evalArit a1 s) == (evalArit a2 s)
evalBool (Lessthan a1 a2) s = (evalArit a1 s) < (evalArit a2 s)
evalBool (Not b) s = (not (evalBool b s))
evalBool (Or b1 b2) s = ((evalBool b1 s) || (evalBool b2 s))
evalBool (And b1 b2) s = ((evalBool b1 s) && (evalBool b2 s))
 
{- | 'evalWH' evaluates a program with a halting parameter. The halting
     parameter indicates the maximum number of steps the program can take.
     If the halting parameter "runs out" (number of steps exceeded), a '-1'
     is saved on location '0', indicating an error.
-}
evalWH :: Program -> State -> Int -> (State, Int)
evalWH p s halt = if (halt <= 0)
                  then (replace 0 s (-1), 0)
                  else (evalWHStep p s halt)

{- | 'evalWHStep' evaluates one step and then checks the halting condition
     again.
-}
evalWHStep :: Program -> State -> Int -> (State, Int)
evalWHStep Skip s halt = (s, halt-1)
evalWHStep (Assign (Loc x) a) s halt = let p = (evalAritWH a s halt)
                                       in (replace x s (fst p), (snd p) -1)
evalWHStep (Concat p1 p2) s halt = let p = (evalWH p1 s halt)
                                   in (evalWH p2 (fst p) (snd p))
evalWHStep (If b p1 p2) s halt = let p = (evalBoolWH b s halt)
                                 in if (fst p)
                                    then (evalWH p1 s ((snd p) - 1))
                                    else (evalWH p2 s ((snd p) - 1))
evalWHStep (While b p) s halt = let p1 = (evalBoolWH b s halt)
                                in if (fst p1)
                                   then
                                     let p2 = (evalWH p s ((snd p1) - 1))
                                     in (evalWH (While b p)
                                         (fst p2) (snd p2))
                                   else (s, (snd p1) - 1)

 
{- | 'evalAritWH' evaluates an 'Arit' taking into account the halting
     parameter.
-}
evalAritWH :: Arit -> State -> Int -> (Integer, Int)
evalAritWH (In n) _ halt = (n, halt)
evalAritWH (Mem (Loc x)) s halt = ((getValue s x), halt-1)   
evalAritWH (Plus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                 in let p2 = (evalAritWH a2 s (snd p1))
                                    in ((fst p1) + (fst p2), (snd p2) -1)
evalAritWH (Minus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in (Util.natSub (fst p1) (fst p2),
                                         (snd p2) -1)
evalAritWH (Times a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in ((fst p1) * (fst p2), (snd p2) -1)

{- | 'evalBoolWH' evaluates a 'BoolExp' taking the halting parameter into
     account.
-}
evalBoolWH :: BoolExp -> State -> Int -> (Bool, Int)
evalBoolWH T _ halt = (True, halt)
evalBoolWH F _ halt = (False, halt)
evalBoolWH (Equals a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                   in let p2 = (evalAritWH a2 s (snd p1))
                                      in ((fst p1) == (fst p2),
                                          (snd p2) - 1)
evalBoolWH (Lessthan a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                     in let p2 = (evalAritWH a2 s (snd p1))
                                        in ((fst p1) < (fst p2),
                                            (snd p2) - 1)
evalBoolWH (Not b) s halt = let p = (evalBoolWH b s halt)
                            in (not (fst p), (snd p) -1) 
evalBoolWH (Or b1 b2) s halt = let p1 = (evalBoolWH b1 s halt)
                               in if (fst p1)
                                  then (True, (snd p1) -1)
                                  else (evalBoolWH b2 s ((snd p1) - 1))
evalBoolWH (And b1 b2) s halt = let p1 = (evalBoolWH b1 s halt)
                                in if (not (fst p1))
                                   then (False, (snd p1) -1)
                                   else (evalBoolWH b2 s ((snd p1) - 1))

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
 
{- | 'getReturnValue' takes a 'State', obtains the value "i" of the 0th
     register, and returns the ith binary string in canonical order, where
     canonical order is the total order that orders first by length and
     then by lexicographical order.
     The first ten binary strings in canonical order are:
     0, 1, 00, 01, 10, 11, 000, 001, 010, 011.
-}
getReturnValue :: State -> String
getReturnValue s = getStringFromTuple $ natIntoString $ getValue s 0 

                                                      
{- | 'executeProgram' takes a 'Program' a number of steps to run (if this
     number ends up being '0' or less, a default value indicated by the
     constant 'maxSteps' is used), and an output function (which is to
     mean, a function that takes a memory 'State' and returns a 'String')
     and returns a 'String' with the program's output or the 'String' "err"
     if the program didn't halt in the required number of steps.
-}
executeProgram :: Program -> Int -> (State -> String) -> String
executeProgram p halt f = let halt' = if halt <= 0
                                      then maxSteps
                                      else halt
                          in f $ fst $ evalWH p [] halt'

{- | 'execListPrograms' executes a 'List' of 'Program' using the function
     'executeProgram'.
-}
execListPrograms :: [(Program, Int)] -> (State -> String) -> [String]
execListPrograms lop outputF = map
                               (\t -> executeProgram (fst t) (snd t)
                                 outputF)
                               lop              
