module Lambda where

import Expr
import Data.List


free_vars_helper :: Expr -> [String] -> [String]
free_vars_helper (Variable x) list
  | x `elem` list = []
  | otherwise = [x]
free_vars_helper (Function x body) list = free_vars_helper body (x:list)
free_vars_helper (Application func arg) list =
  (free_vars_helper func list) ++ (free_vars_helper arg list)

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars expr = nub (free_vars_helper expr [])
    
iterateStrings :: [String] -> Expr -> String
iterateStrings (x:xs) expr = 
  if x `elem` (free_vars expr) then iterateStrings xs expr
                              else x

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce e1 x e2 = case e1 of 
  Variable a -> if a == x then e2 else e1
  Function y a -> if x == y then e1 else if y `elem` (free_vars e2) 
    then (reduce (Function replace_string (reduce a y (Variable replace_string))) x e2) 
    else (Function y (reduce a x e2 )) 
  Application a b -> Application (reduce a x e2) (reduce b x e2) 
  where replace_string = iterateStrings ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"] e1
   
-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN x = case x of 
  Application e1 e2 -> case e1 of 
    Function y e -> reduce e y e2 
    Application e3 e4 -> Application (stepN e1) e2
    Variable a -> Application e1 (stepN e2)
  Variable a -> x
  Function y e -> Function y (stepN e)


-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN expr = case stepN expr of
  e | e == expr -> expr
    | otherwise -> reduceN e

reduceAllN :: Expr -> [Expr]
reduceAllN expr = case stepN expr of
  e | e == expr -> [expr]
    | otherwise -> expr : reduceAllN e

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA x = case x of
  Application e1 e2 -> case e1 of
    Variable a -> Application e1 (stepA e2)
    Function y e -> case e2 of
      Application e3 e4  -> Application e1 (stepA e2)
      Function z f -> reduce e y e2
      Variable a -> reduce e y e2
    Application e5 e6 -> Application (stepA e1) e2
  Function y e -> Function y (stepA e)
  Variable a -> x

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = case stepA expr of
  e | e == expr -> expr
    | otherwise -> reduceA e


reduceAllA :: Expr -> [Expr]
reduceAllA expr = case stepA expr of
  e | e == expr -> [expr]
    | otherwise -> expr : reduceAllA e


-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros x y = case y of 
              Macro a -> case (lookup a x) of
                    Nothing -> y
                    Just b -> evalMacros x b
              Function a b -> Function a (evalMacros x b) 
              Application a b -> Application (evalMacros x a) (evalMacros x b)
              Variable a -> Variable a

evalOneCode :: (Expr -> Expr) -> [Code] -> [(String, Expr)] -> [Expr]
evalOneCode function listC listE = case listC of
          [] -> []
          (x:xs) -> case x of
                    Assign a b -> evalOneCode function xs ((a, b):listE)
                    Evaluate a -> (function (evalMacros listE a)) : evalOneCode function xs listE
-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode function list = evalOneCode function list []
