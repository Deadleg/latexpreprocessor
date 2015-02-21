{-# LANGUAGE DeriveDataTypeable #-}

module MathProcessor
( Expression(..)
, ExpressionType(..)
, construct
, displayEquation
) where

import Data.Data
import Data.List.Split
import System.IO
import qualified Data.HashMap.Strict as Map

type LatexEquation = String

data ExpressionType = Var 
                    | UnaryFunc  Expression (Map.HashMap String String -> Expression -> (String -> String))
                    | BinaryFunc Expression Expression (Map.HashMap String String -> Expression -> Expression -> (String -> String))
                    | MultiFunc  [Expression] (Map.HashMap String String -> [Expression] -> (String -> String))

data Expression = Expression { getKey    :: String
                             , exprType  :: ExpressionType } 

class LtxExpr a where
    getValue  :: Map.HashMap String String -> a -> String
    construct :: Map.HashMap String String -> a -> String

instance LtxExpr Expression where
    getValue map expr  = case Map.lookup (getKey expr) map of
                              Just x  -> x
                              Nothing -> "Nothing"
    construct map expr = case exprType expr of
                              Var                    -> getValue map expr
                              UnaryFunc exp f        -> f map exp $ getValue map expr
                              BinaryFunc exp1 exp2 f -> f map exp1 exp2 $ getValue map expr
                              MultiFunc xs f         -> f map xs $ getValue map expr
    
generateKeyValuePairs :: String -> [(String , String)]
generateKeyValuePairs input = map (\x -> (head x, last x)) (map (splitOn ",") (lines input))

constructLatex :: Map.HashMap String String -> Expression -> LatexEquation
constructLatex map expr = construct map expr

displayEquation :: Expression -> FilePath -> IO (String)
displayEquation expr filePath = do
                            contents <- readFile filePath
                            return $ constructLatex (Map.fromList (generateKeyValuePairs contents)) expr
                            
