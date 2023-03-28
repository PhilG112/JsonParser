{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

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


jsonNull :: Parser JsonValue
jsonNull = undefined 

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
jsonValue = undefined
