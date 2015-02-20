{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module MathProcessor
( multiply
, pow
, frac
, plus
, Expression(Var, Func, Func2, Derivative)
, displayEquation
) where

import Data.Data
import Data.List.Split
import System.IO
import qualified Data.HashMap.Strict as Map

type LatexEquation = String

class ExpressionOps a where
    multiply :: a -> a -> a
    pow :: a -> a -> a
    frac :: a -> a -> a
    plus :: a -> a -> a

data Expression = Var String
                | Func Expression
                | Func2 Expression Expression
                | Derivative Expression Expression
                deriving (Data, Typeable)

instance ExpressionOps Expression where
    multiply exp1 exp2 = Func2 exp1 exp2
    pow exp1 exp2      = Func2 exp1 exp2
    frac exp1 exp2     = Func2 exp1 exp2
    plus exp1 exp2     = Func2 exp1 exp2

generateKeyValuePairs :: String -> [(String , String)]
generateKeyValuePairs input = map (\x -> (head x, last x)) (map (splitOn ",") (lines input))

constructLatex :: Expression -> Map.HashMap String String -> LatexEquation
constructLatex expr map = case expr of
                        Var name             -> show $ case Map.lookup name map of
                                                    Just x   -> x
                                                    Nothing  -> "Nothing"
                        Func var             -> (show $ toConstr (expr)) ++ (constructLatex var map)
                        Func2 var1 var2      -> (show $ toConstr (expr)) ++ (constructLatex var1 map) ++ (constructLatex var2 map)
                        Derivative var1 var2 -> (show $ toConstr (expr)) ++ (constructLatex var1 map) ++ (constructLatex var2 map)

-- Recursively call displayEquation for non-Var Expressions
displayEquation :: Expression -> FilePath -> IO (String)
displayEquation expr filePath = do
                            contents <- readFile filePath
                            return $ constructLatex expr (Map.fromList (generateKeyValuePairs contents))
                            
                            
