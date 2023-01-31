module Parser.ExtractWordEquation where

import Prelude hiding (lookup)
import Data.Map (Map, empty, fromList, insertWith, lookup, mapWithKey, union, (!))
import Parser.SMTLibParser (Expression (..))
import Brzozowski.Regex (Regex (..))
import NielsenTransformation (Variable (..), Sequence, Terminal (Terminal), ToSymbol (toSymbol))
import Data.Maybe (fromMaybe)

asserts :: [Expression] -> [[Expression]]
asserts es = es >>= extractAssert
    where
        extractAssert :: Expression -> [[Expression]]
        extractAssert (Parenthesized [Atom "assert", Parenthesized assertion]) = [assertion]
        extractAssert (Parenthesized (Atom "assert" : _)) = error "assert is a unary function"
        extractAssert _ = []

variables :: [Expression] -> [String]
variables es = es >>= extractStringVariable
    where
        extractStringVariable :: Expression -> [String]
        extractStringVariable (Parenthesized [Atom "declare-const", Atom x, Atom "String"]) = [x]
        extractStringVariable (Parenthesized [Atom "declare-const", _, Atom t]) = error (t ++ " variables not supported")
        extractStringVariable (Parenthesized (Atom "declare-const" : _)) = error "parse error"
        extractStringVariable (Parenthesized [Atom "declare-fun", Atom x, Parenthesized [], Atom "String"]) = [x]
        extractStringVariable (Parenthesized [Atom "declare-fun", _, Parenthesized [], Atom t]) = error (t ++ " variables not supported")
        extractStringVariable (Parenthesized (Atom "declare-fun" : _)) = error "parse error"
        extractStringVariable _ = []

-- renames variable to have names with length 1
renameVariables :: [String] -> Map String Char
renameVariables vs = fromList (map helper shortVariables ++ zip longVariables ['ア'..])
    where
        shortVariables = filter (\x -> length x == 1) vs
        longVariables = filter (\x -> length x /= 1) vs
        helper x = (x, head x)

data Constraint = Equation Term Term | RegexConstraint String Regex
    deriving Show

parseAssert :: [Expression] -> Constraint
parseAssert [Atom "str.in_re", Atom x, Parenthesized re] = RegexConstraint x (parseRegex re)
parseAssert [Atom "not", Parenthesized [Atom "str.in_re", Atom x, Parenthesized re]] = RegexConstraint x (Not (parseRegex re))
parseAssert [Atom "=", t1, t2] = Equation (parseTerm t1) (parseTerm t2)
parseAssert (Atom "<": _) = error "string comparison is not supported"
parseAssert (Atom ">": _) = error "string comparison is not supported"
parseAssert (Atom "<=": _) = error "string comparison is not supported"
parseAssert (Atom ">=": _) = error "string comparison is not supported"
parseAssert [Atom "=", Atom "true", Parenthesized a] = parseAssert a
parseAssert [Atom "or", Parenthesized a] = parseAssert a
parseAssert [Atom "not", Parenthesized [Atom "=", Atom x, StringLiteral s]] = RegexConstraint x (Not (strToRe s))
parseAssert [Atom "not", Parenthesized [Atom "=", _, _]] = error "inequality is not supported"
parseAssert _ = error "parse error"

parseRegex :: [Expression] -> Regex
parseRegex [Atom "str.to_re", StringLiteral s] = strToRe s
parseRegex [Atom "re.none"] = Phi
parseRegex [Atom "re.++", Parenthesized p, Parenthesized q] = Concatenation (parseRegex p) (parseRegex q)
parseRegex [Atom "re.*", Parenthesized p] = Iterate (parseRegex p)
parseRegex [Atom "re.+", Parenthesized p] = Concatenation r (Iterate r)
    where r = parseRegex p
parseRegex [Atom "re.intersection", Parenthesized p, Parenthesized q] = And (parseRegex p) (parseRegex q)
parseRegex [Atom "re.union", Parenthesized p, Parenthesized q] = Or (parseRegex p) (parseRegex q)
parseRegex [Atom "re.comp", Parenthesized p] = Not (parseRegex p)
parseRegex [Atom "re.range", StringLiteral [a], StringLiteral [z]] = foldr Or Phi (map Symbol [a .. z])
parseRegex [Atom "re.*", Atom "re.allchar"] = Iterate (Not Phi)
parseRegex _ = error "parse error"

strToRe :: String -> Regex
strToRe as = mconcat (map Symbol as)

data TermSymbol = Variable' String | String String
    deriving Show

type Term = [TermSymbol]

parseTerm :: Expression -> Term
parseTerm (Atom x) = [Variable' x]
parseTerm (StringLiteral s) = [String s]
parseTerm (Parenthesized (Atom "str.++" : ts)) = mconcat (map parseTerm ts)
parseTerm _ = error "parse error"

constructVariables :: [String] -> [Constraint] -> Map String Char -> Map String (Variable Regex)
constructVariables allVariables cs nameLookup = mapWithKey (\x r -> Variable (nameLookup !? x) r) variables `union` wildcards
    where
        constructVariables' [] = empty
        constructVariables' (RegexConstraint v r : vs) = insertWith And v r (constructVariables' vs)
        constructVariables' (Equation _ _ : vs) = constructVariables' vs
        variables = constructVariables' cs
        wildcards = fromList (zip allVariables (map (\x -> Variable (nameLookup !? x) (Not Phi)) allVariables))

constructEquation :: [Constraint] -> (Term, Term)
constructEquation cs = if length equations /= 1 then error "can only handle one equation" else head equations
    where
        constructEquation' [] = []
        constructEquation' (RegexConstraint _ _ : cs) = constructEquation' cs
        constructEquation' (Equation t1 t2 : cs) = (t1, t2) : constructEquation' cs

        equations = constructEquation' cs

termToSequence :: Map String (Variable Regex) -> Term -> Sequence Regex
termToSequence _ [] = []
termToSequence variables (Variable' x : ts) = toSymbol (variables !? x) : termToSequence variables ts
termToSequence variables (String s : ts) = map (toSymbol . Terminal) s++ termToSequence variables ts

(!?) :: Map String b -> String -> b
(!?) m k = fromMaybe (error ("key " ++ k ++ " not found")) (lookup k m)
