-- Evaluator for our while language expressions.
-- Author: Victor Zamora
import Language
import Parser hiding (main)
import Numeric.Natural
import Text.ParserCombinators.Parsec hiding (State)


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

--Gets the return value from the position x[0]
getReturnValue0 :: State -> Integer
getReturnValue0 s = getValue s 0

--Executes the program and returns a value.
--This function receives a function that gets the return value from the state.
executeProgram :: Program -> (State -> Integer) -> Integer
executeProgram p f = f $ eval p []

main =
  do c <- getContents
     case parse program "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> (print (executeProgram r getReturnValue0))

          
       
