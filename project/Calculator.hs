{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

type Name = String
type Env = [(Name, Integer)]

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Var Name
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          deriving Show

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Var v) = case lookup v env of
                    Just v -> v
                    Nothing -> error "runtime error"
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | '-' term exprCont |epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | '/' factor termCont | '%' factor termCont | epsilon

-- factor ::= variable | natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> 
               do char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                <|> 
                do char '/'
                   f <- factor  
                   termCont (Div acc f)
                <|> 
                do char '%'
                   f <- factor  
                   termCont (Mod acc f)
                <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
         <|>
         do v <- variable
            return (Var v)
         <|>
         do char '('
            e <- expr
            char ')'
            return e
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser String
variable = do xs <- many1 (satisfy isLetter)
              return xs

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator [] (lines txt)  -- if env parameter /= [] program works with vars defined here!

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator env []  = return ()
calculator env (l:ls) = do 
                            let (output, nextEnv) = execute env l
                            putStrLn output
                            calculator nextEnv ls  

-- | evaluate a single expression
execute :: Env -> String -> (String, Env)
execute env txt 
    = case parse expr txt of
        [ (tree, "") ] ->  (show (eval env tree), env)
        _ -> ("parse error; try again", env)