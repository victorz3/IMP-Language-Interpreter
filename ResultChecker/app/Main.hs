{- This program checks that results obtained by our collaborators are
 - correct.
 - Author: Victor Zamora -}

-- System libraries.
import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5
import System.Random
import Data.Sort
import Data.Time.Clock

-- Our libraries.
import qualified Util
import qualified ProgramHandler

-- Name of file with the hash code.
hashFile = "hash.txt"
-- Name of file containing the results of execution.
results = "outputs.txt"
-- File containing numbers for executed programs.
programs = "programs.txt"
-- Number of files to cross-check
checks = 5

{- | 'findResult' finds the result of a specific program within
     a '[String]' containing lines with various programs'
     results.
     It has two parameters. The first parameter contains the
     lines with results and the second parameter contains the
     number of the specific program we're looking for.
-}
findResult :: [String] -> String -> String
findResult [] p = error ("No entry for program" ++ p)
findResult (s:l) p = let w = words s in
                       if p == head w
                       -- Last element of the line is the
                       -- program's result
                       then head $ tail $ w
                       else findResult l p
  
-- ^Gets a program's result from the 'results' file.
getProgramResult :: String -> IO String
getProgramResult p = do
-- First, look for the program's number.
-- Open program.
  program <- ProgramHandler.openProgram p
-- Parse the program.
  let num = head $ lines $ program
  rHandle <- openFile results ReadMode
  contents <- hGetContents rHandle
  let l = lines contents
  return $ findResult l p

{- | 'checkProgramResult' checks whether the result of a program matchs the one saved in the outputs file.
     First parameter is the program.
     Second parameter is the expected result.
     This function returns 'true' if the execution of the program returns the expected result and 'false'
     otherwise.
-}
checkProgramResult :: String -> String -> IO Bool
checkProgramResult p r = do
  result <- getProgramResult p
  return (result == r)

main :: IO ()
main = do
  r <- getProgramResult "3"
  print r
  iguales <- (checkProgramResult "3" r)
  print iguales
  -- -- First, get value of hash. This step might not be necessary
  -- -- depending on how hash is received.
  -- handle1 <- openFile hashFile ReadMode
  -- c <- hGetContents handle1
  -- let hash = take ((length c) - 1) c
  -- -- Now read file bytes.
  -- fileContent <- LB.readFile results 
  -- -- Compute md5
  -- let hash2 = md5 fileContent
  -- -- Check if hashes match
  -- when (hash /= show hash2) (error "File has been corrupted")
  -- -- Now execute checks.
  -- -- First get the list of executed programs. 
  -- handle2 <- openFile programs ReadMode
  -- prog <- hGetContents handle2
  -- let programs = lines prog
  -- -- Get seed from current time.
  -- time <- getCurrentTime >>= return . utctDayTime
  -- let seed = floor $ toRational $ time
  -- let sample = Util.randomSample seed checks programs
  -- print sample
  -- -- Now, check every program from the sample.
  
  
