-- Abstract syntax for while programs
-- Author: Victor Zamora
module Language where

import Numeric.Natural

type State = [(Natural, Integer)]

data Program = Skip
             | Assign Loc Arit
             | Concat Program Program
             | If BoolExp Program Program
             | While BoolExp Program deriving (Show)

data Arit = In Integer
          | Mem Loc
          | Plus Arit Arit
          | Minus Arit Arit
          | Times Arit Arit deriving (Show)

data BoolExp = T
             | F
             | Equals Arit Arit
             | Lessthan Arit Arit
             | Not BoolExp
             | Or BoolExp BoolExp
             | And BoolExp BoolExp deriving (Show)

data Loc = Loc Natural deriving (Show)

-- Function to get value in a location.
getValue :: State -> Natural -> Integer
getValue [] _ = 0
getValue ((i, j):rest) x = if x == i then j
                         else getValue rest x 

-- Replaces a value within a state
replace :: Natural -> State -> Integer -> State
replace n [] i = [(n, i)]
replace n ((a, b):xs) i = if n == a then ((n, i):xs)
                          else ((a, b):(replace n xs i))

--Defining a simple eval function
eval :: Program -> State -> State
eval Skip s = s
eval (Assign (Loc x) a) s = replace x s (evalArit a s)
eval (Concat p1 p2) s = let sNew = (eval p1 s)
                        in (eval p2 sNew)
eval (If b p1 p2) s = if (evalBool b s) then (eval p1 s)
                      else (eval p2 s)
eval (While b p) s = if (evalBool b s) then let sNew = (eval p s)
                                          in (eval (While b p) sNew)
                     else s
  
-- Evaluates an arithmetic expression
evalArit :: Arit -> State -> Integer
evalArit (In n) _ = n
evalArit (Mem (Loc x)) s = (getValue s x)   
evalArit (Plus a1 a2) s = (evalArit a1 s) + (evalArit a2 s)
evalArit (Minus a1 a2) s = (evalArit a1 s) - (evalArit a2 s)
evalArit (Times a1 a2) s = (evalArit a1 s) * (evalArit a2 s)


-- Evaluates a boolean expression
evalBool :: BoolExp -> State -> Bool
evalBool T _ = True
evalBool F _ = False
evalBool (Equals a1 a2) s = (evalArit a1 s) == (evalArit a2 s)
evalBool (Lessthan a1 a2) s = (evalArit a1 s) < (evalArit a2 s)
evalBool (Not b) s = (not (evalBool b s))
evalBool (Or b1 b2) s = ((evalBool b1 s) || (evalBool b2 s))
evalBool (And b1 b2) s = ((evalBool b1 s) && (evalBool b2 s))
