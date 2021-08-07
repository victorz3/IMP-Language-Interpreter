{- | Description: This program checks that results sent by our
     collaborators are correct.
     Maintainer: agua@ciencias.unam.mx
-}

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA
import qualified Eval
import qualified ProgramHandler as H
import Data.Sort
import Data.Time.Clock
import Language
import Parser
import System.IO
import System.Random
import Text.ParserCombinators.Parsec
import Text.Read
import qualified Util

-- | 'dataFolder' is the folder where our program data is contained.
dataFolder = "../Data/"

-- | 'programsFolder' is the folder where the programs to run are contained.
programsFolder = dataFolder ++ "programs/"

-- | Name of file with the hash code.
hashFile = dataFolder ++ "hash.txt"

-- | Name of file containing the results of execution.
results = dataFolder ++ "outputs.txt"

-- | File containing numbers for executed programs.
programs = dataFolder ++ "programs.txt"

-- | Number of files to cross-check
checks = 3

{- | 'openGetProgramResult' opens a program, executes it, and returns its
     resulting 'String' wrapped in the 'I2O' monad.
     The resulting 'String' is of the form:
     program# result len(program) steps_taken
-}
openGetProgramResult :: String -> IO String
openGetProgramResult p = do
  contents <- H.openProgram (programsFolder ++ p)
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
        (fst result) ++ " " ++ (show (lenP r)) ++
        " " ++ (show (snd result)) ++ "\n")

{- | 'findResult' finds the result of a specific program within
     a '[String]' containing lines with various programs'
     results.
     It has two parameters. The first parameter contains the
     lines with results and the second parameter contains the
     number of the specific program we're looking for.
-}
findResult :: [String] -> String -> String
findResult [] p = error ("No entry for program " ++ p)
findResult (s:l) p = let w = words s in
                       if p == head w
                       -- Last element of the line is the
                       -- program's result
                       then head $ tail $ w
                       else findResult l p
  
-- | Gets a program's result from the 'results' file.
getProgramResult :: String -> IO String
getProgramResult p = do
  rHandle <- openFile results ReadMode
  contents <- hGetContents rHandle
  let l = lines contents
  return $ findResult l p

{- | 'checkProgramResult' checks whether the result of a program matchs the
     one saved in the outputs file.
     First parameter is the program.
     Second parameter is the expected result.
     This function returns 'true' if the execution of the program returns
     the expected result and 'false' otherwise.
-}
checkProgramResult :: String -> String -> IO Bool
checkProgramResult p r = do
  result <- getProgramResult p
  return (result == r)

{- | 'openExecuteCheck' opens a program file, then executes it with the
     interpreter and finally checks that the result is the same as the one
     in out results file.
-}
openExecuteCheck :: String -> IO Bool 
openExecuteCheck program = do
  result <- openGetProgramResult program
  let output = H.getOutput result
  rv <- checkProgramResult program output
  return rv

{- | 'isOutputFileValid' checks whether the specified outputs file is
     valid. This involves two checks:
     - MD5Sum of file is equal to the one received (integrity check)
     - A random sample of the outputs is valid
     Checking the second condition involves running a random sample of
     programs. The number of programs run is specified by the variable
     'checks'
-}
isOutputFileValid :: IO Bool
isOutputFileValid = do
  -- First, get value of hash.
  hash <- readFile hashFile
  -- Now read file bytes.
  fileContent <- LB.readFile results 
  -- Compute SHA256
  let hash2 =  showDigest $ sha256 fileContent
  -- Check if hashes match
  let b = hash /= hash2
  if b then (return False)
    else do
    -- Now execute checks.
    -- First get the list of executed programs. 
    handle2 <- openFile programs ReadMode
    prog <- hGetContents handle2
    let programs = lines prog
    -- Get seed from current time.
    time <- getCurrentTime >>= return . utctDayTime
    let seed = floor $ toRational $ time
    let sample = Util.randomSample seed checks programs
    print sample
    -- Now, check every program from the sample.
    resultsMatch <- mapM (openExecuteCheck) sample
    return ((foldr (&&) True resultsMatch) == True)

  
main :: IO ()
main = do
  valid <- isOutputFileValid
  if valid 
    then print "Results are correct!"
    else error "Results are incorrect!"
  
