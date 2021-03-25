-- Evaluator for our while language expressions.
-- Author: Victor Zamora
import Language
import Parser hiding (main)
import qualified Util
import Data.Maybe
import System.IO
import Math.NumberTheory.Logarithms
import Text.ParserCombinators.Parsec hiding (State)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Maximum number of steps to execute for each program.
maxSteps = 10000

-- File containing programs to run.
programsFile = "programs.txt"

-- Folder containing program files.
programFilesLoc = "programs/"

-- File to write program outputs.
outputs = "outputs.txt"

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

-- Simplifies a boolean expression
simplBoolExp :: BoolExp -> BoolExp
simplBoolExp (Equals x y) = if x == y then T else (Equals x y)
simplBoolExp (Lessthan x y) = if x == y then F else (Lessthan x y)
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

{- Simplifies booleans in a program, attempting to detect programs
 - that don't halt.
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
  
--Evaluation function with optional halting parameter.
evalWH :: Program -> StateL -> Int -> (StateL, Int)
evalWH p s halt = if (halt <= 0)
                      then (replace 0 s (-1), 0)
                      else (evalWHAux p s halt)

{- Auxiliary function that evaluates one step, then checks halting
 - condition again. -}
evalWHAux :: Program -> StateL -> Int -> (StateL, Int)
evalWHAux Skip s halt = (s, halt-1)
evalWHAux (Assign (Loc x) a) s halt = let p = (evalAritWH a s halt)
                                      in (replace x s (fst p),
                                          (snd p) -1)
evalWHAux (Concat p1 p2) s halt = let p = (evalWH p1 s halt)
                                        in (evalWH p2 (fst p) (snd p))
evalWHAux (If b p1 p2) s halt = let p = (evalBoolWH b s halt)
                                in if (fst p)
                                   then (evalWH p1 s ((snd p) - 1))
                                   else (evalWH p2 s ((snd p) - 1))
evalWHAux (While b p) s halt = let p1 = (evalBoolWH b s halt)
                               in if (fst p1)
                                  then
                                    let p2 =
                                          (evalWH p s ((snd p1) - 1))
                                    in (evalWH (While b p) (fst p2)
                                        (snd p2))
                                  else (s, (snd p1) - 1)

 
-- Evaluates an arithmetic expression with a halting parameter.
evalAritWH :: Arit -> StateL -> Int -> (Integer, Int)
evalAritWH (In n) _ halt = (n, halt)
evalAritWH (Mem (Loc x)) s halt = ((getValue s x), halt-1)   
evalAritWH (Plus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                 in let p2 = (evalAritWH a2 s (snd p1))
                                    in ((fst p1) + (fst p2),
                                        (snd p2) -1)
evalAritWH (Minus a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in (Util.natSub (fst p1) (fst p2), (snd p2) -1)
evalAritWH (Times a1 a2) s halt = let p1 = (evalAritWH a1 s halt)
                                  in let p2 = (evalAritWH a2 s (snd p1))
                                     in ((fst p1) * (fst p2), (snd p2) -1)

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
getReturnValue :: StateL -> String
getReturnValue s = getStringFromTuple $ getTupleFromNat $ getValue s 0 

{- Returns the ith String in canonical order.
   This function returns a tuple where the first element is
   the numeric output and the second element is the number
   of bits required to represent said number as a string -}
getTupleFromNat :: Integer -> (Integer, Int)
getTupleFromNat 0 = (0, 1)
getTupleFromNat n = if n > 0
                    then let l = integerLog2 (n+2)
                         in let c = n - (2^l) + 2
                            in (c, l)
                    -- Si el número es negativo, tenemos un error.
                    else (-1, 0)

{- Transforms a tuple of Integer and Int into a String.
 - If the tuple is (n, m), the string will b n in base 2
 - using m digits.
 -}
getStringFromTuple :: (Integer, Int) -> String
getStringFromTuple (n, m)
  | m <= 0 = "err"
  | n == 0 = replicate m '0'
  | otherwise = replicate (m - (integerLog2 n) - 1) '0' ++ stringn
  where stringn = showIntAtBase 2 intToDigit n ""
                                                      
{- Executes the program with the option to halt if, after a number of steps,
 - the program hasn't halted.
 - First parameter is the program.
 - Second parameter is the maximum number of steps to execute. If the number is 0, we
 - default to the value established by maxSteps.
 - Third parameter is a function that returns the program's output. -}
executeProgram :: Program -> Int -> (StateL -> String) -> String
executeProgram p halt f = let halt' = if halt <= 0
                                      then maxSteps
                                      else halt
                          in f $ fst $ evalWH p [] halt'

executeListOfPrograms :: [(Program, Int)] -> (StateL -> String) -> [String]
executeListOfPrograms lop outputFunc = map
                                       (\t -> executeProgram (fst t) (snd t)
                                        outputFunc) lop



openAndExecuteProgram :: String -> IO ()
openAndExecuteProgram programName =
  do
    programHandle <- openFile
                     (programFilesLoc ++ programName ++ ".while")
                     ReadMode
    contents <- hGetContents programHandle
    case parse numberedProgramWithHalt "(stdin)" contents of
      Left e -> error "Error parsing input: " -- ++ (show e)
      Right r -> do
        appendFile outputs $ (executeProgram (Util.thrd r) (Util.snd r) getReturnValue) ++ "\n"
        

-- Versión sin arreglos.
main :: IO ()
main = do hanP <- openFile programsFile ReadMode
          c <- hGetContents hanP
          let programs = lines c
          --outputHandle <- openFile outputs AppendMode
          test <- mapM openAndExecuteProgram programs
          print "Es el final"

                   -- case parse numberedProgramWithHalt "(stdin)" c of
          --   Left e -> do putStrLn "Error parsing input:"
          --                print e
          --   Right r -> do
          --     let res = executeProgram (Util.thrd r) (Util.snd r) getReturnValue
              
              
