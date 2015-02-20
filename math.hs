{-# LANGUAGE TemplateHaskell #-}
import MathProcessor
import Data.Data
import Data.Typeable

gamma = Var "gamma"
lambda = Var "lambda"
epsilon = Var "epsilon"
x = Var "x"
theta = Func gamma

leftEquation = Derivative theta gamma
rightEquation = (lambda `pow` (x `plus` epsilon)) `multiply` (Derivative (Derivative theta gamma) gamma)

equationOne :: (Expression, Expression)
equationOne = (leftEquation, rightEquation)

main = do
        latex <- displayEquation rightEquation "vars.txt"
        putStrLn $ latex

