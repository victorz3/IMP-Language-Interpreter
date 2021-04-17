module Main where

import Language
import Parser hiding (main)
import System.IO
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  hanP <- openFile "ejemplo.imp" ReadMode
  c <- hGetContents hanP
  let programs = lines c
  case parse program "(stdin)" c of
         Left e -> do
           putStrLn "Error parsing input:"
           print e
         Right r -> print (programMemory r)

        --outputHandle <- openFile outputs AppendMode
