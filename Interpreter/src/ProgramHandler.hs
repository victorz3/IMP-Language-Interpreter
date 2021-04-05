{- |
Module:      ProgramHandler 
Description: This library is for opening and handling program
              files in our language (while).
Mantainer:   agua@ciencias.unam.mx
-}
module ProgramHandler where

import System.IO

-- Folder containing program files.
programFilesLoc = "programs/"
-- File extension for our programs.
extension = ".while"


{- | The 'openProgram' function opens a program file by adding
     the correct extension and looking in the correct folder.
     The extension and folder are saved in constans 'folder' and
     'extension'.
-}
openProgram :: String -> IO String
openProgram p = do
  programHandle <- openFile
                   (programFilesLoc ++ p ++ extension) ReadMode
  contents <- hGetContents programHandle
  return contents
