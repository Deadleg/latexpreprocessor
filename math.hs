{-# LANGUAGE TemplateHaskell #-}
import MathProcessor
import Data.Data
import Data.Typeable
import MathDefaults

gamma = Expression { getKey="gamma", exprType=Var }
lambda = Expression { getKey="lambda", exprType=Var }
epsilon = Expression { getKey="epsilon", exprType=Var }
x = Expression { getKey="x", exprType=Var }
theta = Expression { getKey="theta", exprType=BinaryFunc gamma lambda constructInfix }

leftEquation = Expression { getKey="fasda", exprType=MultiFunc [theta,gamma,x,epsilon] constructMulti }

--rightEquation = (l)

--equationOne :: (Expression, Expression)
--equationOne = (leftEquation, rightEquation)

main = do
        latex <- displayEquation leftEquation "vars.txt"
        putStrLn $ latex

