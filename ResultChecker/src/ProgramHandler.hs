{- |
Module:      ProgramHandler 
Description: This library is for opening and handling program
              files in our language (while).
Mantainer:   agua@ciencias.unam.mx
-}
module ProgramHandler where

import System.IO

-- Folder containing program files.
folder = "programs/"
-- File extension for our programs.
extension = ".while"

{- | The 'openProgram' function opens a program file by adding
     the correct extension and looking in the correct folder.
     The extension and folder are saved in constants 'folder' and
     'extension'.
-}
openProgram :: String -> IO String
openProgram p = do
  programHandle <- openFile
                   (folder ++ p ++ extension) ReadMode
  contents <- hGetContents programHandle
  return contents

{- | 'getOutput' gets the output out of a line of the form:
     program# program_output len(program)
-}
getOutput :: String -> String
getOutput line = let w = words line in
                   head $ tail $ w
