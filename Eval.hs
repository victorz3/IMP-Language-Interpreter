-- Evaluator for our while language expressions.
-- Author: Victor Zamora
import Language
import Parser hiding (main)
import Text.ParserCombinators.Parsec hiding (State)

--A memory state using lists
type StateL = [(Int, Integer)]

-- Function to get value in a location.
getValue :: StateL -> Int -> Integer
getValue l i = fromMaybe 0 (lookup i l)
                 
-- Replaces a value within a state
replace :: Int -> StateL -> Integer -> StateL
replace n [] i = [(n, i)]
replace n ((a, b):xs) i = if n == a then ((n, i):xs)
                          else ((a, b):(replace n xs i))

--Defining a simple eval function
eval :: Program -> StateL -> StateL
eval Skip s = s
eval (Assign (Loc x) a) s = replace x s (evalArit a s)
eval (Concat p1 p2) s = let sNew = (eval p1 s)
                        in (eval p2 sNew)
eval (If b p1 p2) s = if (evalBool b s) then (eval p1 s)
                      else (eval p2 s)
eval (While b p) s = if (evalBool b s) then let sNew = (eval p s)
                                          in (eval (While b p) sNew)
                     else s

--Evaluation function with optional halting parameter.
evalWH :: Program -> StateL -> Int -> (StateL, Int)
evalWH p s halt = if (halt <= 0)
                      then (s, 0)
                      else (evalWHAux p s halt)

--Auxiliary function that evaluates one step, then checks halting condition again.
evalWHAux :: Program -> StateL -> Int -> (StateL, Int)
evalWHAux Skip s halt = (s, halt-1)
evalWHAux (Assign (Loc x) a) s halt = let p = (evalAritWH a s halt)
                                            in (replace x s (fst p), (snd p) -1)
evalWHAux (Concat p1 p2) s halt = let p = (evalWH p1 s halt)
                                        in (evalWH p2 (fst p) (snd p))
evalWHAux (If b p1 p2) s halt = let p = (evalBoolWH b s halt)
                                in if (fst p)
                                   then (evalWH p1 s ((snd p) - 1))
                                   else (evalWH p2 s ((snd p) - 1))
evalWHAux (While b p) s halt = let p1 = (evalBoolWH b s halt)
                               in if (fst p1)
                                  then let p2 = (evalWH p s ((snd p1) - 1))
                                       in (evalWH (While b p) (fst p2) (snd p2))
                                  else (s, (snd p1) - 1)

 
-- Evaluates an arithmetic expression
evalArit :: Arit -> StateL -> Integer
evalArit (In n) _ = n
evalArit (Mem (Loc x)) s = (getValue s x)   
evalArit (Plus a1 a2) s = (evalArit a1 s) + (evalArit a2 s)
evalArit (Minus a1 a2) s = (evalArit a1 s) - (evalArit a2 s)
evalArit (Times a1 a2) s = (evalArit a1 s) * (evalArit a2 s)

-- Evaluates an arithmetic expression with a halting parameter.
evalAritWH :: Arit -> StateL -> Int -> (Integer, Int)
evalAritWH (In n) _ halt = (n, halt)
evalAritWH (Mem (Loc x)) s halt = ((getValue s x), halt-1)   
evalAritWH (Plus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                 in let p2 = (evalAritWH a2 s (snd p1))
                                    in ((fst p1) + (fst p2), (snd p2) -1)
evalAritWH (Minus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in ((fst p1) - (fst p2), (snd p2) -1)
evalAritWH (Times a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in ((fst p1) * (fst p2), (snd p2) -1)




-- Evaluates a boolean expression
evalBool :: BoolExp -> StateL -> Bool
evalBool T _ = True
evalBool F _ = False
evalBool (Equals a1 a2) s = (evalArit a1 s) == (evalArit a2 s)
evalBool (Lessthan a1 a2) s = (evalArit a1 s) < (evalArit a2 s)
evalBool (Not b) s = (not (evalBool b s))
evalBool (Or b1 b2) s = ((evalBool b1 s) || (evalBool b2 s))
evalBool (And b1 b2) s = ((evalBool b1 s) && (evalBool b2 s))

-- Evaluates a boolean expression with halting parameter.
evalBoolWH :: BoolExp -> StateL -> Int -> (Bool, Int)
evalBoolWH T _ halt = (True, halt)
evalBoolWH F _ halt = (False, halt)
evalBoolWH (Equals a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                   in let p2 = (evalAritWH a2 s (snd p1))
                                      in ((fst p1) == (fst p2), (snd p2) - 1)
evalBoolWH (Lessthan a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                     in let p2 = (evalAritWH a2 s (snd p1))
                                        in ((fst p1) < (fst p2), (snd p2) - 1)
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


--Gets the return value from the position x[i]
getReturnValue :: Int -> StateL -> Integer
getReturnValue i s = getValue s i 

--Executes the program and returns a value.
--This function receives a function that gets the return value from the state.
executeProgram :: Program -> (StateL -> Integer) -> Integer
executeProgram p f = f $ eval p []

-- Executes the program with the option to halt if, after a number of steps,
-- the program hasn't halted.
executeProgramWH :: Program -> Int -> (StateL -> Integer) -> Integer
executeProgramWH p halt f = f $ fst $ evalWH p [] halt

-- Main for parsing and executing with lists.
-- main =
--   do c <- getContents
--      case parse program "(stdin)" c of
--             Left e -> do putStrLn "Error parsing input:"
--                          print e
--             Right r -> do
--               v <- Vec.replicate 2 0
--               evalA r v
--               print (Vec.read v 0)
--               return ()

-- Versión con arreglos                     
-- main :: IO ()
-- main =
--     do c <- getContents
--        case parse program "(stdin)" c of
--             Left e -> do putStrLn "Error parsing input:"
--                          print e
--             Right r -> print (getIntegerReturnValue r)


-- Versión sin arreglos.
main :: IO ()
main =
    do c <- getContents
       case parse program "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> print (executeProgram r (getReturnValue 0))

-- Versión sin y con detención en 5 pasos.
-- main :: IO ()
-- main =
--     do c <- getContents
--        case parse program "(stdin)" c of
--             Left e -> do putStrLn "Error parsing input:"
--                          print e
--             Right r -> print (executeProgramWH r 16 (getReturnValue 1))
