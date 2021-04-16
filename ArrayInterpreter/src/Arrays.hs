-- Evaluator for our while language expressions.
-- This version uses arrays instead of lists.
-- Author: Victor Zamora
import Language
import Parser hiding (main)
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad.ST
import qualified Data.Vector.Mutable as Vec

--Gets value in array location.
getValueA :: Vec.MVector s Integer -> Int -> ST s Integer
getValueA v i = do
  value <- Vec.read v i
  return value

--Replaces a value in an array location.
replaceA :: Int -> Vec.MVector s Integer -> Integer -> ST s ()
replaceA i v j = do
  Vec.write v i j 

-- Evaluation function which uses array locations.
evalA :: Program -> Vec.MVector s Integer -> ST s ()
evalA Skip _ = do
  return ()
evalA (Assign (Loc x) a) v = do
  expr <- evalAritA a v
  replaceA x v expr 
  return ()
evalA (Concat p1 p2) v = do
  evalA p1 v
  evalA p2 v
  return ()
evalA (If b p1 p2) v = do
  boo <- evalBoolA b v
  case boo of
    True -> evalA p1 v
    False -> evalA p2 v
  return ()
evalA (While b p) v = do
  boo <- evalBoolA b v
  case boo of
    True -> evalA (Concat p (While b p)) v
    otherwise -> return ()
  return ()    
  
-- Evaluates an arithmetic expression using a vector as memory
evalAritA :: Arit -> Vec.MVector s Integer -> ST s Integer
evalAritA (In n) _ = do
  return n
evalAritA (Mem (Loc x)) v = do
  val <- getValueA v x
  return val
evalAritA (Plus a1 a2) v = do
  e1 <- evalAritA a1 v
  e2 <- evalAritA a2 v
  return (e1 + e2)
evalAritA (Minus a1 a2) v = do
  e1 <- evalAritA a1 v
  e2 <- evalAritA a2 v
  return (e1 - e2)
evalAritA (Times a1 a2) v = do
  e1 <- evalAritA a1 v
  e2 <- evalAritA a2 v
  return (e1 * e2)

-- Evaluates a boolean expression using arrays.
evalBoolA :: BoolExp -> Vec.MVector s Integer -> ST s Bool
evalBoolA T _ = do
  return True
evalBoolA F _ = do
  return False
evalBoolA (Equals a1 a2) v = do
  e1 <- evalAritA a1 v
  e2 <- evalAritA a2 v
  return (e1 == e2)
evalBoolA (Lessthan a1 a2) v = do
  e1 <- evalAritA a1 v
  e2 <- evalAritA a2 v
  return (e1 < e2)
evalBoolA (Not a) v = do
  b1 <- evalBoolA a v
  return (not b1)
evalBoolA (Or a1 a2) v = do
  b1 <- evalBoolA a1 v
  b2 <- evalBoolA a2 v
  return (b1 || b2)
evalBoolA (And a1 a2) v = do
  b1 <- evalBoolA a1 v
  b2 <- evalBoolA a2 v
  return (b1 && b2)

-- Gets the return value of a program using arrays
-- Using constant array size for now, but eventually will have to allocate.
getReturnValue0A :: Program -> ST s Integer
getReturnValue0A p = do
  v <- Vec.replicate 100000 0
  evalA p v
  Vec.read v 0

--Removes ST monad from program's return value (when using arrays)
getIntegerReturnValue :: Program -> Integer 
getIntegerReturnValue p = runST $ getReturnValue0A p

-- Versión con arreglos                     
main :: IO ()
main =
    do c <- getContents
       case parse program "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> print (getIntegerReturnValue r)
