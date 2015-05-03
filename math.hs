{-# LANGUAGE TemplateHaskell #-}
import MathProcessor
import Data.Data
import MathDefaults

gamma        = Expression { getKey="gamma"    , exprType=Var }
lambda       = Expression { getKey="lambda"   , exprType=Var }
epsilon      = Expression { getKey="epsilon"  , exprType=Var }
x            = Expression { getKey="x"        , exprType=Var }
theta        = Expression { getKey="plus"    , exprType=BinaryFunc gamma lambda constructInfix }
leftEquation = Expression { getKey="function1", exprType=MultiFunc [theta,gamma,x,epsilon] constructMulti }

main = do
        latex <- displayEquation leftEquation "vars.txt"
        putStrLn $ latex

