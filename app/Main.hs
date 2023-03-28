{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where

import Data.Char
import Control.Applicative  
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    p <- parseFile (head args) jsonValue
    print p

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fn p = do
    input <- readFile fn
    return (snd <$> runParser p input)

data JsonValue = JsonNull
    | JsonBool Bool
    | JsonNumber Integer -- No support floats...
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

-- No proper error reporting for now
newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    } 

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x) 
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)
 
instance Alternative Parser where
    empty = Parser $ const Nothing

    (Parser p1) <|> (Parser p2) = 
        Parser $ \input -> p1 input <|> p2 input

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where
        f :: String -> JsonValue
        f "true" = JsonBool True
        f "false" = JsonBool False
        -- This should never happen...monkaW
        f _ = undefined

jsonNull :: Parser JsonValue 
jsonNull = const JsonNull <$> stringP "null"

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where
        f ds = JsonNumber $ read ds
-- No scape support for now...
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

wsP :: Parser String
wsP = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep e = (:) <$> e <*> many (sep *> e) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> elements <* charP ']')
    where
        elements = sepBy sep jsonValue
        sep = wsP *> charP ',' <* wsP

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> wsP *>
                            sepBy (wsP *> charP ',' <* wsP) pair
                            <* wsP <* charP '{')
    where
        pair = (\key _ value -> (key, value)) <$> stringLiteral
                            <*> (wsP *> charP ':' *> wsP)
                            <*> jsonValue

notNull :: Parser [a] -> Parser [a]
notNull p = Parser $ \input ->
    do
        (input', xs) <- runParser p input
        if null xs
            then Nothing
            else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
        in Just (rest, token)

charP :: Char -> Parser Char
charP x = Parser f
    where
        f [] = Nothing
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject 
