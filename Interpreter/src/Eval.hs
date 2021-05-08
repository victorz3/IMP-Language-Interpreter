{- |
Module:       Eval
Description:  This library is for evaluating programs in our language (IMP).
Maintainer:   agua@ciencias.unam.mx
-}

module Eval where

import Data.Maybe
import Language
import Output
import qualified Util

{- | 'maxSteps' contains the default value for the maximum number of steps
     that the interpreter should run for.
-}
maxSteps = 10000

-- | 'State' represents a memory state using lists.
type State = [(Int, Integer)]

-- | 'emptyState' returns an empty 'State'
emptyState = []

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
     This function returns a tuple where the first element is the returning
     'State', the second element is the halting parameter, and the third element
     is the number of steps taken.
-}
evalWH :: Program -> State -> Int -> Int -> (State, Int, Int)
evalWH p s halt steps = if (halt <= 0)
                  then (replace 0 s (-1), 0, steps)
                  else (evalWHStep p s halt steps)

{- | 'evalWHStep' evaluates one step and then checks the halting condition
     again.
     This is an auxiliary function that returns a 3-tuple where the first
     element is the computed 'State', the second element is the halting
     parameter, and the third element is the number of steps taken.
-}
evalWHStep :: Program -> State -> Int -> Int -> (State, Int, Int)
evalWHStep Skip s halt steps = (s, halt - 1, steps + 1)
evalWHStep (Assign (Loc x) a) s halt steps = let p = (evalAritWH
                                                      a
                                                      s
                                                      halt
                                                      steps)
                                             in (replace x s (Util.fst p),
                                                 (Util.snd p) -1,
                                                 (Util.thrd p) + 1)
evalWHStep (Concat p1 p2) s halt steps = let p = (evalWH p1 s halt steps)
                                         in (evalWH p2
                                             (Util.fst p)
                                             (Util.snd p)
                                             (Util.thrd p))
evalWHStep (If b p1 p2) s halt steps = let p = (evalBoolWH b s halt steps)
                                       in if (Util.fst p)
                                          then (evalWH
                                                 p1
                                                 s
                                                 ((Util.snd p) - 1)
                                                 ((Util.thrd p) + 1))
                                          else (evalWH
                                                 p2
                                                 s
                                                 ((Util.snd p) - 1)
                                                 ((Util.thrd p) + 1))
evalWHStep (While b p) s halt steps = let p1 = (evalBoolWH b s halt steps)
                                      in if (Util.fst p1)
                                         then
                                           let p2 = (evalWH
                                                      p
                                                      s
                                                      ((Util.snd p1) - 1)
                                                      ((Util.thrd p1) + 1))
                                           in (evalWH
                                               (While b p)
                                               (Util.fst p2)
                                               (Util.snd p2)
                                               (Util.thrd p2))
                                         else (s,
                                               (Util.snd p1) - 1,
                                               (Util.thrd p1) + 1)
                                              
{- | 'evalAritWH' evaluates an 'Arit' taking into account the halting
     parameter and keeping a counter of steps executed.
-}
evalAritWH :: Arit -> State -> Int -> Int -> (Integer, Int, Int)
evalAritWH (In n) _ halt steps = (n, halt, steps)
evalAritWH (Mem (Loc x)) s halt steps = ((getValue s x), halt-1, steps + 1)   
evalAritWH (Plus a1 a2) s halt steps = let p1 = (evalAritWH a1 s halt steps)
                                       in let p2 = (evalAritWH
                                                     a2
                                                     s
                                                     (Util.snd p1)
                                                     (Util.thrd p1))
                                          in ((Util.fst p1) + (Util.fst p2),
                                              (Util.snd p2) -1,
                                              (Util.thrd p2) + 1)
evalAritWH (Minus a1 a2) s halt steps = let p1 = (evalAritWH a1 s halt steps)
                                        in let p2 = (evalAritWH
                                                      a2
                                                      s
                                                      (Util.snd p1)
                                                      (Util.thrd p1))
                                           in (Util.natSub
                                                (Util.fst p1)
                                                (Util.fst p2),
                                               (Util.snd p2) -1,
                                               (Util.thrd p2) + 1)
evalAritWH (Times a1 a2) s halt steps = let p1 = (evalAritWH a1 s halt steps)
                                        in let p2 = (evalAritWH
                                                      a2
                                                      s
                                                      (Util.snd p1)
                                                      (Util.thrd p1))
                                     in ((Util.fst p1) * (Util.fst p2),
                                         (Util.snd p2) -1,
                                         (Util.thrd p2) + 1)

{- | 'evalBoolWH' evaluates a 'BoolExp' taking the halting parameter into
     account and keeping a counter of steps executed.
-}
evalBoolWH :: BoolExp -> State -> Int -> Int -> (Bool, Int, Int)
evalBoolWH T _ halt steps = (True, halt, steps)
evalBoolWH F _ halt steps = (False, halt, steps)
evalBoolWH (Equals a1 a2) s halt steps = let p1 = (evalAritWH a1 s halt steps)
                                         in let p2 = (evalAritWH
                                                       a2
                                                       s
                                                       (Util.snd p1)
                                                       (Util.thrd p1))
                                      in ((Util.fst p1) == (Util.fst p2),
                                          (Util.snd p2) - 1,
                                          (Util.thrd p2) + 1)
evalBoolWH (Lessthan a1 a2) s halt steps = let p1 = (evalAritWH
                                                      a1
                                                      s
                                                      halt
                                                      steps)
                                           in let p2 = (evalAritWH
                                                         a2
                                                         s
                                                         (Util.snd p1)
                                                         (Util.thrd p1))
                                        in ((Util.fst p1) < (Util.fst p2),
                                            (Util.snd p2) - 1,
                                            (Util.thrd p2) + 1)
evalBoolWH (Not b) s halt steps = let p = (evalBoolWH b s halt steps)
                                  in (not (Util.fst p),
                                      (Util.snd p) - 1,
                                      (Util.thrd p) + 1) 
evalBoolWH (Or b1 b2) s halt steps = let p1 = (evalBoolWH b1 s halt steps)
                                     in if (Util.fst p1)
                                        then (True,
                                              (Util.snd p1) - 1,
                                              (Util.thrd p1) + 1)
                                        else (evalBoolWH
                                              b2
                                              s
                                              ((Util.snd p1) - 1)
                                              ((Util.thrd p1) + 1))
evalBoolWH (And b1 b2) s halt steps = let p1 = (evalBoolWH b1 s halt steps)
                                      in if (not (Util.fst p1))
                                         then (False,
                                               (Util.snd p1) -1,
                                               (Util.thrd p1) + 1)
                                         else (evalBoolWH
                                                b2
                                                s
                                                ((Util.snd p1) - 1)
                                                ((Util.thrd p1) + 1))
  
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
                          in f $ Util.fst $ evalWH p emptyState halt' 0

{- | 'uExecuteProgram' is an unsafe version of 'executeProgram' that
     doesn't take into account the halting parameter.
-}
uExecuteProgram :: Program -> (State -> String) -> String
uExecuteProgram p f = f $ eval p emptyState

{- | 'execListPrograms' executes a 'List' of 'Program' using the function
     'executeProgram'.
-}
execListPrograms :: [(Program, Int)] -> (State -> String) -> [String]
execListPrograms lop outputF = map
                               (\t -> executeProgram (fst t) (snd t)
                                 outputF)
                               lop              

{- | 'getSepsHalt' takes a program and a halting parameter and it returns the
     number of steps the program took to halt.
-}
getStepsHalt :: Program -> Int -> Int
getStepsHalt p halt = let halt' = if halt <= 0
                                  then maxSteps
                                  else halt
                      in Util.thrd $ evalWH p emptyState halt' 0
