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
dataFolder = "../Data/Testing/LotsOfAccesses/"

-- | 'programsFolder' is the folder where the programs to run are contained.
programsFolder = dataFolder ++ ""

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

    
{- | 'uOpenGetProgramResult' is an unsafe version of 'openGetProgramResult'
     that doesn't take into account the halting parameter.
-} 
uOpenGetProgramResult :: String -> IO String
uOpenGetProgramResult p = do
  contents <- ProgramHandler.openProgram (programsFolder ++ p)
  case parse program "" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      return (Eval.uExecuteProgram r Eval.getReturnValue)
  
  
writeOutputs :: [String] -> IO ()
writeOutputs [] = do return ()
writeOutputs (o:l) = do
  appendFile outputs o 
  writeOutputs l
  
main :: IO ()
main = do
  result <- uOpenGetProgramResult "prueba100000"
  putStrLn result
  
  -- hanP <- openFile programsFile ReadMode
 --  c <- hGetContents hanP
 --  let programs = lines c
 --  --results <- openGetListResults programs
 --  results <- pOpenExecuteListPrograms programs
 --  writeOutputs results
 --  -- Compute hash
 -- -- let bResults = BU.fromString results
 --  --let hash = md5 (LB.fromStrict bResults)
 --  -- Create zip
 --  -- s1 <- mkEntrySelector outputs
 --  -- createArchive outputsZip (addEntry Deflate bResults s1)
 --  -- s2 <- mkEntrySelector hashF
 --  -- withArchive outputsZip (addEntry Deflate (md5DigestBytes hash) s2) 
 --  putStrLn "Everything is fine :)"
