{- This program checks that results obtained by our collaborators are
 - correct.
 - Author: Victor Zamora -}

import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5
import System.Random
import Data.Sort
import Data.Time.Clock
import Util

-- Name of file with the hash code.
hashFile = "hash.txt"
-- Name of file containing the results of execution.
filename = "results.txt"
-- File containing numbers for executed programs.
programs = "programs.txt"
-- Number of files to cross-check
checks = 5

{- Checks the result of a program.
 - First parameter is the program.
 - Second parameter is the expected result.
 - This function returns true if the execution of the program returns the expected
 - result and false otherwise. -}
checkProgramResult :: String -> String -> Bool
checkProgramResult p r = r == ()


main :: IO ()
main = do
  {- First, get value of hash. This step might not be necessary depending
   - on how hash is received. -}
  handle1 <- openFile hashFile ReadMode
  c <- hGetContents handle1
  let hash = take ((length c) - 1) c
  -- Now read file bytes.
  fileContent <- LB.readFile filename 
  -- Compute md5
  let hash2 = md5 fileContent
  -- Check if hashes match
  when (hash /= show hash2) (error "File has been corrupted")
  {- Now execute checks
   - First get the list of executed programs. -}
  handle2 <- openFile programs ReadMode
  prog <- hGetContents handle2
  let programs = lines prog
  -- Get seed from current time.
  time <- getCurrentTime >>= return . utctDayTime
  let seed = floor $ toRational $ time
  let sample = randomSample seed checks programs
  -- Now, check every program from the sample.
  
  
