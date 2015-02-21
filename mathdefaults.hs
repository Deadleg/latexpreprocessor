module MathDefaults
( constructInfix
, constructUnary
, constructMulti
, constructDerivative
) where

import MathProcessor
import qualified Data.HashMap.Strict as Map

constructInfix :: Map.HashMap String String -> Expression -> Expression -> (String -> String)
constructInfix map expr1 expr2 = \x -> (construct map expr1) ++ " " ++ x ++ " " ++ (construct map expr2)

constructDerivative :: Map.HashMap String String -> Expression -> Expression -> (String -> String)
constructDerivative map expr1 expr2 = \_ -> "\\frac{d" ++ (construct map expr1) ++ "}{d" ++ (construct map expr2) ++ "}"

constructUnary :: Map.HashMap String String -> Expression -> (String -> String)
constructUnary map expr = \x -> (construct map expr) ++ " " ++ x

constructMulti :: Map.HashMap String String -> [Expression] -> (String -> String)
constructMulti map exprs = \x -> x ++ "(" ++ (init (foldl (\acc x -> acc ++ (construct map x) ++ ",") "" exprs))  ++ ")"
