{- | Description: Main program that calls the interpreter on a set of
                  programs specified by a file.
     Maintainer:  agua@ciencias.unam.mx
-}

import qualified Eval
import qualified Language
import Parser hiding (main)
import qualified ProgramHandler
import System.IO
import Text.ParserCombinators.Parsec
import qualified Util


-- | 'programsFile is the file containing the names of the programs to run.
programsFile = "programs.txt"

{- | 'outputs' is the name of the file where the outputs of the programs
     will be written.
-}
outputs = "outputs.txt"


{- | 'openGetProgramResult' opens a program, executes it, and returns its
     resulting 'String' wrapped in the 'IO' monad.
     The resulting 'String' is of the form:
     program# result len(program)
-}
openGetProgramResult :: String -> IO String
openGetProgramResult p = do
  contents <- ProgramHandler.openProgram p
  case parse numberedProgramHalt "(stdin)" contents of
    Left e -> error ("Error parsing program " ++ p ++ " : " ++ (show e))
    Right r -> do
      let program = Util.thrd r
      let steps = Util.snd r
      return ((show (Util.fst r)) ++ " " ++
               (Eval.executeProgram program steps Eval.getReturnValue) ++
               " " ++ (show (Language.lenP program)) ++ "\n")

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
   
main :: IO ()
main = do
  res <- uOpenGetProgramResult "100000"
--  print res
  return ()
  -- hanP <- openFile programsFile ReadMode
          -- c <- hGetContents hanP
          -- let programs = lines c 
          -- --outputHandle <- openFile outputs AppendMode
          -- l <- mapM openExecuteAppendProgram programs
          -- return ()      
  
-- main :: IO ()
-- main = do
--   c <- getContents
--   case parse program "(stdin)" c of
--     Left e -> do putStrLn "Error parsing input:"
--                  print e
--     Right r -> print (getIntegerReturnValue r)
