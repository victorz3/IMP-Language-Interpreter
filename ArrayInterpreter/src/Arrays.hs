{- |
Module:      Arrays
Description: This library is for evaluating programs in our language (IMP)
             using arrays for the interpreter's memory.
Mantainer:   agua@ciencias.unam.mx
-}

module Arrays where

import Control.Monad.ST
import qualified Data.Vector.Mutable as Vec
import Language
import Output

{- | 'getValueA' takes a 'MVector' and an 'Int' and returns the 'MVector''s
     value in the posicion specified by the 'Int'. This function is
     unsafe, meaning it does not check that the value actually exists
     before trying to read it.
-}
getValueA :: Vec.MVector s Integer -> Int -> ST s Integer
getValueA v i = do
  value <- Vec.read v i
  return value

{- | 'replaceA' takes a 'MVector', an'Int', and an 'Integer' and replaces
     the value at the position specified by the second parameter for the
     third parameter. This function is unsafe, meaning it does not check
     that the value actually exists before trying to replace it.
-}
replaceA :: Int -> Vec.MVector s Integer -> Integer -> ST s ()
replaceA i v j = do
  Vec.write v i j 

{- | 'evalA' does the evaluation of a 'Program' with a 'MVector' used to
     represent the memory.
-}
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
   
{- | 'evalAritA' does the evaluation of an 'Arit' with a 'MVector' used to
     represent the memory.
-}
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

{- | 'evalBoolA' does the evaluation of a 'BoolExp' with a 'MVector' used
     to represent the memory.
-}
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

{- | 'executeProgramM' runs a 'Program' with a memory size specified by the
     'Int' parameter, and returns the program's result ('String'
     corresponding to the value at location '0') wrapped in the 'ST' monad.
-}
executeProgramM :: Program -> Int -> ST s String
executeProgramM p mem = do
  v <- Vec.replicate mem 0
  evalA p v
  result <- Vec.read v 0
  return (stringFromNat result)

{- | 'executeProgram' executes a 'Program' using the 'executeProgramM'
     function but computing memory needed for execution first.
-}
executeProgram :: Program -> ST s String
executeProgram p = executeProgramM p (programMemory p)
