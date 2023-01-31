module Main (main) where

import System.Environment (getArgs)
import Parser.SMTLibParser (parseSMTFile)
import Parser.ExtractWordEquation (asserts, variables, parseAssert, constructEquation, constructVariables, renameVariables, termToSequence)
import NielsenTransformation (Equation((:=:)), nielsen)
import Brzozowski.NielsenTransformable ()

main :: IO ()
main = do
    [path] <- getArgs
    smt <- parseSMTFile path

    let vs = variables smt
    let as = asserts smt
    let constraints = map parseAssert as
    let vars = constructVariables vs constraints (renameVariables vs)
    let (t1, t2) = constructEquation constraints
    let e = termToSequence vars t1 :=: termToSequence vars t2

    putStrLn (if nielsen e then "sat" else "unsat")
