import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.List

type Latex = String

emphasisSymbol :: Parser Char
emphasisSymbol = char '*'

boldSymbol :: Parser String
boldSymbol = string "**"

emphacizedChar :: Parser Char
emphacizedChar = noneOf "*"

boldChar :: Parser Char
boldChar = try (do char '*'
                   noneOf "*")
       <|> noneOf "*"
       <?> "Didn't find bold." 

textChar :: Parser Char
textChar = noneOf "*("

linkChar :: Parser Char
linkChar = noneOf "]"

linkDescriptionChar :: Parser Char
linkDescriptionChar = noneOf ")"

emphasis = do emphasisSymbol
              content <- many1 emphacizedChar
              emphasisSymbol
              return content

bold = do boldSymbol
          content <- many1 boldChar
          boldSymbol
          return content

link = do char '('
          description <- many1 linkDescriptionChar
          string ")["
          link <- many1 linkChar
          char ']'
          return (description ++ " " ++ link)

bodyText = try (link) <|> try (bold) <|> try (emphasis) <|> many1 (textChar)

htexFile = many bodyText

readInput :: String -> [Latex]
readInput input = case parse htexFile "" input of
    Left err  -> ["No match " ++ show err]
    Right val -> val

main = do 
    contents <- readFile "E:\\Google Drive\\Code\\latexpreprocessor\\input.htex"
    putStrLn $ intercalate "\n" (readInput contents)

