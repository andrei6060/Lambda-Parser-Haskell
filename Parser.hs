module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    mp >>= f = 
        Parser $ \s ->
            case parse mp s of
                Nothing -> Nothing
                Just(x, s') -> parse (f x) s'
    return x = Parser $ \s -> Just(x, s)

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser (\s -> case parse p1 s of 
                                Nothing -> parse p2 s
                                ok -> ok)

--- type declaration over ---

-- TODO 2.1. parse a expression
failParser :: Parser a
failParser = Parser (\s -> Nothing)
--app func brack variable
charParser :: Char -> Parser Char
charParser c = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just(c, xs) else Nothing)
        
charParser2 :: Parser Char
charParser2 = Parser (\s ->
    case s of
        []-> Nothing
        (x:xs) -> Just(x, xs))


predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just(x, xs) else Nothing)

macroParser :: Parser Expr
macroParser = do
    spaceParser
    charParser '$'
    x <- variableParser2
    return $ Macro x

 
variableParser :: Parser Expr
variableParser = do
    spaceParser
    x <- predicateParser (isAlpha)
    return $ Variable [x]

variableParser2 :: Parser String
variableParser2 = do
    x <- some (predicateParser (isAlpha))
    return $ x

functionParser :: Parser Expr
functionParser = do
    spaceParser
    charParser '\\'
    x <- variableParser2
    charParser '.'
    --
    e <-  functionParser <|> betweenParanteze <|> variableParser <|> macroParser
    return $ Function x e

parantezaParser :: Parser Char
parantezaParser = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == '(' then Just('(', xs) else Nothing)

paranteza2Parser :: Parser Char
paranteza2Parser = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == ')' then Just(')', xs) else Nothing)



spaceParser :: Parser Char
spaceParser = Parser (\s ->
    case s of
        [] -> Just(' ', [])
        (x:xs) -> if x == ' ' then Just(' ', xs) else Just(' ', x:xs))


spaceParser2 :: Parser Char
spaceParser2 = Parser (\s ->
    case s of
        [] -> Nothing
        (x:xs) -> if x == ' ' then Just(' ', xs) else Nothing)




applicationParser :: Parser Expr
applicationParser = do
    spaceParser
    x <- some(functionParser <|> variableParser <|> betweenParanteze <|> macroParser)
    spaceParser
    return $ foldl1(\e1 e2 -> Application e1 e2) x

betweenParanteze :: Parser Expr
betweenParanteze = do
    spaceParser
    parantezaParser
    x <- expr_parser
    paranteza2Parser
    return x

expr_parser :: Parser Expr 
expr_parser =  applicationParser <|> functionParser  <|> betweenParanteze  <|> variableParser <|> macroParser

parse_expr :: String -> Expr
parse_expr expr = case parse expr_parser expr of 
    Nothing -> Variable "Error"
    Just(a, _) -> a






codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser 

evaluateParser :: Parser Code
evaluateParser = do
    spaceParser
    expr <- expr_parser
    return (Evaluate expr)

assignParser :: Parser Code
assignParser = do
    spaceParser
    name <- variableParser2
    many spaceParser2
    charParser '='
    many spaceParser2
    expr <- expr_parser
    return (Assign name expr)

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code code = case parse codeParser code of
  Nothing -> error "Error"
  Just (x, _) -> x
    