import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.List

type Latex = String

emphasisSymbol :: Parser Char
emphasisSymbol = char '*'

boldSymbol :: Parser String
boldSymbol = string "**"

beginBoldEmphasisSymbol :: Parser String
beginBoldEmphasisSymbol = string "*_"

endBoldEmphasisSymbol :: Parser String
endBoldEmphasisSymbol = string "_*"

emphacizedChar :: Parser Char
emphacizedChar = noneOf "*"

boldEmphasizedChar :: Parser Char
boldEmphasizedChar = try (do char '_'
                             noneOf "*")
                 <|> noneOf "_"
                 <?> "Didn't find bold emph."

boldChar :: Parser Char
boldChar = try (do char '*'
                   noneOf "*")
       <|> noneOf "*"
       <?> "Didn't find bold." 

boldEmphasis = do beginBoldEmphasisSymbol
                  content <- many1 boldEmphasizedChar
                  endBoldEmphasisSymbol
                  return content

emphasis = do emphasisSymbol
              content <- many1 emphacizedChar
              emphasisSymbol
              return content

bold = do boldSymbol
          content <- many1 boldChar
          boldSymbol
          return content

bodyText = try (boldEmphasis) <|> try (bold) <|> try (emphasis) <|> many1 (noneOf "*")

htexFile = many bodyText

readInput :: String -> [Latex]
readInput input = case parse htexFile "" input of
    Left err  -> ["No match " ++ show err]
    Right val -> val

main = do 
    contents <- readFile "E:\\Google Drive\\Code\\latexpreprocessor\\input.htex"
    putStrLn $ intercalate "\n" (readInput contents)

