import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.List

type latex = String

emphasisSymbol :: Parser Char
emphasisSymbol = char '*'

italicizedChar = noneOf "*"

italics = do emphasisSymbol
             content <- many1 italicizedChar
             emphasisSymbol
             return content

bodyText = italics <|> many1 (noneOf "*")

htexFile = many bodyText

readInput :: String -> [Latex]
readInput input = case parse htexFile "" input of
    Left err  -> ["No match " ++ show err]
    Right val -> fmap ("Match found:\n"++) val

main = do 
    contents <- readFile "E:/Google Drive/Code/latexprecompiler/input.htex"
    putStrLn $ intercalate "\n" (readInput contents)

