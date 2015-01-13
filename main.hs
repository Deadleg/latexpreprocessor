type Latex = String

documentClass = "\\documentclass{article}\n"
beginDocument = "\\begin{document}\n"
endDocument = "\\end{document}"

constructLatex :: String -> Latex
constructLatex input = documentClass ++ beginDocument ++ input ++ "\n" ++ endDocument

parse :: String -> IO Latex
parse input = do
    return $ constructLatex input

main = do 
    contents <- readFile "E:/Google Drive/Code/latexprecompiler/input.htex"
    outputLines <- parse contents
    writeFile "E:/Google Drive/Code/latexprecompiler/output.tex" outputLines
