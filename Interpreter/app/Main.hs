{- | Description: Main program that calls the interpreter on a set of
                  programs specified by a file.
     Maintainer:  agua@ciencias.unam.mx
-}

import qualified Control.Monad.Parallel as P
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA
import qualified Eval as E
import qualified Language
import qualified Output
import Parser
import qualified ProgramHandler
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.Read
import qualified Util

-- | 'dataFolder' is the folder where our program data is contained.
dataFolder = "../Data/"
 
-- | 'programsFolder' is the folder where the programs to run are contained.
programsFolder = dataFolder ++ "programs/"

-- | 'programsFile is the file containing the names of the programs to run.
programsFile = dataFolder ++ "programs3.txt"

{- | 'outputs' is the name of the file where the outputs of the programs
     will be written.
-}
outputs = dataFolder ++ "outputs3.txt"

-- | 'hash' is the file to contain the MD5 of the outputs file. 
hashF = dataFolder ++ "hash3.txt"

{- | 'openGetProgramResult' opens a program, executes it, and returns its
     resulting 'String' wrapped in the 'IO' monad.
     The resulting 'String' is of the form:
     program# result steps_taken
-}
openGetProgramResult :: String -> (E.State -> String) -> IO String
openGetProgramResult p resFunction= do
  putStrLn ("Program: " ++ p)
  contents <- ProgramHandler.openProgram (programsFolder ++ p)
  let halt = head $ lines contents
  let haltN = readMaybe halt :: Maybe Int
  let hP = case haltN of
        Just x -> x
        Nothing -> 0
  case parse optionalHPProgram "" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let result = E.executeProgram r hP resFunction  
      return (p ++ " " ++
        (fst result) ++ " " ++ (show (snd result)) ++ "\n")
  
{- | 'openGetProgramResult2' does the same as 'openGetProgramResult' but it uses
     files with the format: #program\n#steps\nprogram
-}
openGetProgramResult2 :: String -> IO String
openGetProgramResult2 p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      let steps = Util.snd r
      let result = E.executeProgram program steps E.getReturnValue 
      return ((show (Util.fst r)) ++ " " ++
               (fst result) ++ " " ++ (show (Language.lenP program)) ++
               " " ++ (show (snd result)) ++ "\n")

{- | 'openGetProgramSteps' opens a program, executes it, the number of steps it
     took for the program to finish, wrapped in the IO monad. 
-} 
openGetProgramSteps :: String -> IO Int
openGetProgramSteps p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      let steps = Util.snd r
      return $ E.getStepsHalt program steps      
    
{- | 'uOpenGetProgramResult' is an unsafe version of 'openGetProgramResult'
     that doesn't take into account the halting parameter.
-} 
uOpenGetProgramResult :: String -> IO String
uOpenGetProgramResult p = do
  contents <- ProgramHandler.openProgram (programsFolder ++ p)
  case parse program "" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      return (E.uExecuteProgram r E.getReturnValue)
  
{- | 'openExecuteAppendProgram' opens and executes a program file with the
     following format:
     - 1st line is an integer stating the program number.
     - 2nd line is an integer stating maximum number of steps to execute.
     - 3rd line and beyond is the program itself, according to the IMP
       grammar.
     After executing the program, this function writes the program's
     output in 'outputs'. The format for the output is the same as the one
     in 'openGetProgramResult'.
-}
openExecuteAppendProgram :: String -> IO ()
openExecuteAppendProgram programName = do
  result <- openGetProgramResult programName E.getReturnValue--Output.concatOutpu
  appendFile outputs $ result

{- | 'openGetListResults' opens a list of programs an returns an 'IO [String]'
     with with the results of the execution.
-}
openGetListResults :: [String] -> (E.State -> String) -> IO [String]
openGetListResults [] _ = do
  return [] 
openGetListResults (p:rest) f = do
  resP <- openGetProgramResult p f
  resL <- openGetListResults rest f
  return (resP:resL)
  
{-# DEPRECATED openExecuteListPrograms "Use openGetListResults and writeOutputs instead" #-}
openExecuteListPrograms :: [String] -> IO ()
openExecuteListPrograms [] = do return ()
openExecuteListPrograms (p:r) = do
  openExecuteAppendProgram p
  openExecuteListPrograms r  

{- | 'pOpenExecuteListPrograms' is the parallel version of
     'openExecuteListPrograms'
-} 
pOpenExecuteListPrograms :: [String] -> IO [String]
pOpenExecuteListPrograms l = do
  list <- P.mapM (\p -> openGetProgramResult p E.getReturnValue) l 
  return list


pOpenExecuteListPrograms2 :: [String] -> Int -> IO [String]
pOpenExecuteListPrograms2 l m = do
  let lMax = take m l
  let lRest = drop m l
  list <- P.mapM (\p -> openGetProgramResult p E.getReturnValue) lMax
  writeOutputs list
  pOpenExecuteListPrograms2 lRest m

writeOutputs :: [String] -> IO ()
writeOutputs [] = do return ()
writeOutputs (o:l) = do
  appendFile outputs o 
  writeOutputs l
  
main :: IO ()
main = do
  args <- getArgs
  hanP <- openFile programsFile ReadMode
  c <- hGetContents hanP
  let programs = lines c
  results <-
    if (length args) == 0 || head args /= "-par" 
    then openGetListResults programs E.getReturnValue
    else pOpenExecuteListPrograms programs
  writeOutputs results
  -- Compute hash
  bResults <- LB.readFile outputs
  let hash = sha256 bResults
  appendFile hashF (showDigest hash)
  putStrLn "Everything is fine :)"
