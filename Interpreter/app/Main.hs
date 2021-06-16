{- | Description: Main program that calls the interpreter on a set of
                  programs specified by a file.
     Maintainer:  agua@ciencias.unam.mx
-}

import Codec.Archive.Zip
import qualified Control.Monad.Parallel as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU
import Data.Digest.Pure.MD5
import qualified Eval
import qualified Language
import Parser
import qualified ProgramHandler
import System.IO
import Text.ParserCombinators.Parsec
import Text.Read
import qualified Util

-- | 'dataFolder' is the folder where our program data is contained.
dataFolder = "../Data/"

-- | 'programsFolder' is the folder where the programs to run are contained.
programsFolder = dataFolder ++ "programs/"

-- | 'programsFile is the file containing the names of the programs to run.
programsFile = dataFolder ++ "programs.txt"

{- | 'outputs' is the name of the file where the outputs of the programs
     will be written.
-}
outputs = dataFolder ++ "outputs.txt"

-- | 'outputsZip' is the name for the zip file containing the program's outputs.
outputsZip = dataFolder ++ "outputs2.zip"

-- | 'hash' is the file to contain the MD5 of the outputs file. 
hashF = "hash.txt"


{- | 'openGetProgramResult' opens a program, executes it, and returns its
     resulting 'String' wrapped in the 'IO' monad.
     The resulting 'String' is of the form:
     program# result len(program) steps_taken
-}
openGetProgramResult :: String -> IO String
openGetProgramResult p = do
  contents <- ProgramHandler.openProgram (programsFolder ++ p)
  let halt = head $ lines contents
  let haltN = readMaybe halt :: Maybe Int
  let hP = case haltN of
        Just x -> x
        Nothing -> 0
  case parse optionalHPProgram "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let result = Eval.executeProgram r hP Eval.getReturnValue
      return (p ++ " " ++
        (fst result) ++ " " ++ (show (Language.lenP r)) ++
        " " ++ (show (snd result)) ++ "\n")
  
{- | 'openGetProgramResult2' does the same as 'openGetProgramResult' but it uses
     files with the format: #program\n#steps\nprogram
-}
openGetProgramResult2 :: String -> IO String
openGetProgramResult2 p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      let steps = Util.snd r
      let result = Eval.executeProgram program steps Eval.getReturnValue 
      return ((show (Util.fst r)) ++ " " ++
               (fst result) ++ " " ++ (show (Language.lenP program)) ++
               " " ++ (show (snd result)) ++ "\n")

{- | 'openGetProgramSteps' opens a program, executes it, the number of steps it
     took for the program to finish, wrapped in the IO monad. 
-} 
openGetProgramSteps :: String -> IO Int
openGetProgramSteps p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      let steps = Util.snd r
      return $ Eval.getStepsHalt program steps      
    
{- | 'uOpenGetProgramResult' is an unsafe version of 'openGetProgramResult'
     that doesn't take into account the halting parameter.
-} 
uOpenGetProgramResult :: String -> IO String
uOpenGetProgramResult p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      return ((show (Util.fst r)) ++ " " ++
               (Eval.uExecuteProgram program Eval.getReturnValue) ++
               " " ++ (show (Language.lenP program)) ++ "\n")
  
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
  result <- openGetProgramResult programName
  appendFile outputs $ result

{- | 'openGetListResults' opens a list of programs an returns a 'String' with the
     results of the execution.
-}
openGetListResults :: [String] -> IO [String]
openGetListResults [] = do
  return []
openGetListResults (p:rest) = do
  resP <- openGetProgramResult p
  resL <- openGetListResults rest
  return (resP:resL)

  --   return ""
-- openGetListResults (p:rest) = do
--   resP <- openGetProgramResult p
--   resL <- openGetListResults rest
--   return (resP ++ resL)

  
{- | 'openExecuteListPrograms' reads a list of program names, opens and executes
     each program in the list.
     This function is sequential.
-}
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
  list <- P.mapM openGetProgramResult l
  return list

writeOutputs :: [String] -> IO ()
writeOutputs [] = do return ()
writeOutputs (o:l) = do
  appendFile outputs o 
  writeOutputs l
  
main :: IO ()
main = do
  hanP <- openFile programsFile ReadMode
  c <- hGetContents hanP
  let programs = lines c
  --results <- openGetListResults programs
  results <- pOpenExecuteListPrograms programs
  writeOutputs results
  -- Compute hash
 -- let bResults = BU.fromString results
  --let hash = md5 (LB.fromStrict bResults)
  -- Create zip
  -- s1 <- mkEntrySelector outputs
  -- createArchive outputsZip (addEntry Deflate bResults s1)
  -- s2 <- mkEntrySelector hashF
  -- withArchive outputsZip (addEntry Deflate (md5DigestBytes hash) s2) 
  putStrLn "Everything is fine :)"
