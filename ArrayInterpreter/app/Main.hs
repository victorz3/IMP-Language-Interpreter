module Main where

import Arrays
import Control.Monad.ST
import Language
import Parser hiding (main)
import qualified ProgramHandler
import Text.ParserCombinators.Parsec
import Util

{- | 'openGetProgramResult' opens a program, executes it, and returns its
     resulting 'String' wrapped in the 'ST' monad.
     The resulting 'String' is of the form:
     program# result len(program)
-}
openGetProgramResult :: String -> IO String
openGetProgramResult p = do
  contents <- ProgramHandler.openProgram p
  case parse program "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = r
      let memory = programMemory program
      print memory
      let result = runST $ Arrays.executeProgramM program memory
      return $ result ++ " " ++ (show (Language.lenP program)) ++ "\n"

main :: IO ()
main = do
  res <- openGetProgramResult "100000"
  --print res
  return ()

    --outputHandle <- openFile outputs AppendMode
