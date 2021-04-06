module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             deriving Show

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = [first] ++ rest
              return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

parseNumber :: Parser LispVal   
parseNumber = do
                x <- many1 digit
                return $ Number $ read x
--parseNumber = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do
                x <- many1 digit
                char '.'
                y <- many1 digit
                let atom = (x ++ "." ++ y)
                return $ Float $ read atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> parseNumber