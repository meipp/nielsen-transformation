{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances,
    AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NielsenTransformation where

import Data.Bifunctor (first, second)
import Data.Either (Either (..))
import Data.Function (on)
import Data.List (groupBy, intercalate, nub, sortOn)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceShow)
import Prelude hiding (Either (..))
import Color

newtype Terminal = Terminal Char
    deriving (Eq, Show)

data Variable r = Variable Char r
    deriving (Eq, Ord, Show)

type Symbol r = Either Terminal (Variable r)

type Sequence r = [Symbol r]

data Equation r = Sequence r :=: Sequence r
    deriving Eq

-- instance Eq r => Eq (Equation r) where
--     (α :=: β) == (α' :=: β') = α == α' && β == β'

infix 3 :=:

class CanBeEmpty a where
    ε :: a

instance CanBeEmpty (Sequence r) where
    ε = []

class ToSymbol a r where
    toSymbol :: a -> Symbol r

instance ToSymbol Terminal r where
    toSymbol = Left

instance ToSymbol (Variable r) r where
    toSymbol = Right

instance ToSymbol (Symbol r) r where
    toSymbol = id

class ToSequence a r where
    toSequence :: a -> Sequence r

instance ToSequence Terminal r where
    toSequence = return . toSymbol

instance ToSequence (Variable r) r where
    toSequence = return . toSymbol

instance ToSequence (Symbol r) r where
    toSequence = return

instance ToSequence (Sequence r) r where
    toSequence = id

(·) :: (ToSequence a r, ToSequence b r) => a -> b -> Sequence r
α · β = toSequence α ++ toSequence β

data Trace r = Start (Equation r) | Rewrite (Trace r) (RewriteOperation r) (Equation r)

-- This loses some solutions
-- if we would just derive Eq the Trace history would play into (==), blowing up the nub call in nielsenTransformation
instance Eq a => Eq (Trace a) where
    (==) = (==) `on` value

data Side = Left' | Right' deriving (Eq, Show)

data RewriteOperation r = DeleteTerminalPrefix
                        | DeleteVariablePrefix
                        | VariableIsEmpty Side (Variable r) (Replacement r)
                        | VariableStartsWithTerminal Side (Variable r) Terminal (Replacement r)
                        | VariableStartsWithVariable Side (Variable r) (Variable r) (Replacement r)
                        deriving (Eq, Show)

data Replacement r = Replacement (Symbol r) (Sequence r)
    deriving (Eq, Show)

value :: Trace r -> Equation r
value (Start x) = x
value (Rewrite _ _ x) = x

backtrace :: Trace r -> [(Equation r, RewriteOperation r, Equation r)]
backtrace (Start _) = []
backtrace (Rewrite r o x) = (value r, o, x) : backtrace r

type RewriteRule r = Equation r -> [(Equation r, RewriteOperation r)]

rewriteTrace :: RewriteRule a -> Trace a -> [Trace a]
rewriteTrace rule t = map newTrace (rule (value t))
    where newTrace (e, o) = Rewrite t o e

rewriteTraces :: RewriteRule a -> [Trace a] -> [Trace a]
rewriteTraces rule xs = xs >>= rewriteTrace rule

joinRewriteRules :: [RewriteRule a] -> RewriteRule a
joinRewriteRules rs x = rs >>= ($ x)

class Swap a where
    swap :: a -> a

-- TODO: remove
instance (Swap a, Swap b) => Swap (a, b) where
    swap (a, b) = (swap a, swap b)

instance Swap (Equation r) where
    swap (α :=: β) = (β :=: α)

instance Swap Side where
    swap Left' = Right'
    swap Right' = Left'

instance Swap (RewriteOperation r) where
    swap DeleteTerminalPrefix = DeleteTerminalPrefix
    swap DeleteVariablePrefix = DeleteVariablePrefix
    swap (VariableIsEmpty side x replacement) = VariableIsEmpty (swap side) x replacement
    swap (VariableStartsWithTerminal side x a replacement) = VariableStartsWithTerminal (swap side) x a replacement
    swap (VariableStartsWithVariable side x y replacement) = VariableStartsWithVariable (swap side) x y replacement

onBothSides :: RewriteRule r -> RewriteRule r
onBothSides r e = r e ++ map swap (r (swap e))
    -- where swap (α :=: β) = (β :=: α)
-- onBothSides = id
-- onBothSides = error "onBothSides is not defined"

with_aa :: ((Terminal, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation r)]) -> RewriteRule r
with_aa f ((Left a):α :=: (Left b):β)
    | a == b = f (a, (α, β))
    | otherwise = []
with_aa _ _ = []

with_xx :: Eq r => ((Variable r, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation r)]) -> RewriteRule r
with_xx f ((Right x):α :=: (Right y):β)
    | x == y = f (x, (α, β))
    | otherwise = []
with_xx _ _ = []

with_xε :: (((Variable r, Sequence r), ((), Sequence r)) -> [(Equation r, RewriteOperation r)]) -> RewriteRule r
with_xε f ((Right x):α :=: β)
    -- TODO: only if x nullable
    | True = f ((x, α), ((), β))
with_xε _ _ = []

with_xa :: (((Variable r, Sequence r), (Terminal, Sequence r)) -> [(Equation r, RewriteOperation r)]) -> RewriteRule r
with_xa f ((Right x):α :=: (Left a):β)
    -- TODO: only if x can start with a
    | True = f ((x, α), (a, β))
with_xa _ _ = []

with_xy :: Eq r => (((Variable r, Sequence r), (Variable r, Sequence r)) -> [(Equation r, RewriteOperation r)]) -> RewriteRule r
with_xy f ((Right x):α :=: (Right y):β)
    -- TODO: only if x can be satisfied
    -- TODO: only if y can be satisfied
    | x /= y = f ((x, α), (y, β))
    | otherwise = []
with_xy _ _ = []

class NielsenTransformable r where
    -- rule 1
    aa :: (Terminal, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation r)]
    xx :: (Variable r, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation r)]

    -- rule 2
    xε :: ((Variable r, Sequence r), ((), Sequence r)) -> [(Equation r, RewriteOperation r)]

    -- rule 3
    xa :: ((Variable r, Sequence r), (Terminal, Sequence r)) -> [(Equation r, RewriteOperation r)]

    -- rule 4
    xy :: ((Variable r, Sequence r), (Variable r, Sequence r)) -> [(Equation r, RewriteOperation r)]

    -- prerequisites
    nullable :: Symbol r -> Bool
    satisfiable :: Symbol r -> Bool
    showSymbol :: Symbol r -> String

replaceSymbol :: Eq r => Symbol r -> Sequence r -> Symbol r -> Sequence r
replaceSymbol x y x'
    | x == x' = y
    | otherwise = [x']

replace :: (Eq r, ToSymbol a r) => a -> Sequence r -> Sequence r -> Sequence r
replace x = replaceMap id id (toSymbol x)

replaceMap :: (Eq a) => (a -> b) -> (a -> b) -> a -> [a] -> [a] -> [b]
replaceMap onReplace onNoReplace x replacement xs = xs >>= (\x' -> if x' == x then map onReplace replacement else map onNoReplace [x'])

showSequence :: NielsenTransformable r => Sequence r -> String
showSequence [] = "ε"
showSequence α  = α >>= showSymbol

showEquation :: NielsenTransformable r => Equation r -> String
showEquation (α :=: β) = showSequence α ++ " = " ++ showSequence β

showEquations :: NielsenTransformable r => [Equation r] -> String
showEquations es = "[" ++ intercalate ", " (map showEquation es) ++ "]"

nielsenTransformation :: (NielsenTransformable r, Eq r) => [Trace r] -> [Trace r]
nielsenTransformation [] = []
nielsenTransformation ts = trace (showEquations es) (filter (\t -> value t == (ε :=: ε)) ts `listOr` nielsenTransformation (nub (rewriteTraces rewriteRule ts)))
    where es = map value ts
          rewriteRule = joinRewriteRules [
                with_aa aa,
                with_xx xx,
                onBothSides (with_xε xε),
                onBothSides (with_xa xa),
                onBothSides (with_xy xy)
            ]

listOr :: [a] -> [a] -> [a]
listOr [] ys = ys
listOr xs _  = xs

nielsen :: (NielsenTransformable r, Eq r, Ord r, Show r) => Equation r -> Bool
nielsen e = case nielsenTransformation [Start e] of
    []    -> False
    (t:_) -> trace ("trace: " ++ showRewrites bt) $
            --  traceShow (extractSolutionVariablePrefixes operations) $
             trace ("solution: " ++ showIndentedList (\(Variable v _, s) -> v : " = " ++ showSequence s) (extractSolution operations)) $
             True
        where bt = (reverse (backtrace t))
              operations = map (\(_, o, _) -> o) bt

showRewrite :: (Eq r, Show r, NielsenTransformable r) => (Equation r, RewriteOperation r, Equation r) -> String
showRewrite (α :=: β, o@DeleteTerminalPrefix, e2)
    = --show o ++ " | " ++
        colorPrefix red α ++ " = " ++ colorPrefix red β ++ " -> " ++ showEquation e2
showRewrite (α :=: β, o@DeleteVariablePrefix, e2)
    = --show o ++ " | " ++
        colorPrefix red α ++ " = " ++ colorPrefix red β ++ " -> " ++ showEquation e2
showRewrite (α :=: β, o@(VariableIsEmpty _ x (Replacement _ _)), e2)
    = show o ++ " | " ++
        colorOccurrences x red α ++ " = " ++ colorOccurrences x red β ++ " -> " ++ showEquation e2
-- showRewrite (α :=: β, o@(VariableStartsWithTerminal _ x _ replacement), _)
--     = show o ++ " | " ++
--         colorOccurrencesExceptFirst x yellow α ++ " = " ++ colorOccurrencesExceptFirst x yellow β
--     ++ " -> " ++ colorReplacedOccurrencesExceptFirst replacement yellow α  ++ " = " ++ colorReplacedOccurrencesExceptFirst replacement yellow β
-- showRewrite (α :=: β, o@(VariableStartsWithVariable _ x _ replacement), _)
--     = show o ++ " | " ++
--         colorOccurrencesExceptFirst x yellow α ++ " = " ++ colorOccurrencesExceptFirst x yellow β
--     ++ " -> " ++ colorReplacedOccurrencesExceptFirst replacement yellow α  ++ " = " ++ colorReplacedOccurrencesExceptFirst replacement yellow β
showRewrite (e1, o, e2) = showEquation e1 ++ " [" ++ show o ++ "] " ++ showEquation e2

showRewrites :: (Eq r, Show r, NielsenTransformable r) => [(Equation r, RewriteOperation r, Equation r)] -> String
showRewrites es = "[\n    " ++ intercalate "\n    " (map showRewrite es) ++ "\n]"

extractSolutionVariablePrefixes :: [RewriteOperation r] -> [(Variable r, Sequence r)]
extractSolutionVariablePrefixes = mapMaybe variablePrefix

extractSolution :: Ord r => [RewriteOperation r] -> [(Variable r, Sequence r)]
extractSolution os = variables
    where prefixes = extractSolutionVariablePrefixes os
          variables = map (second concat) $ groups prefixes

groups :: Ord a => [(a, b)] -> [(a, [b])]
groups xs = map (first head) $ map unzip $ groupOn fst $ sortOn fst xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

-- showSolutions :: [(Char, String)] -> String
-- showSolutions solutions = map showSolution
showSolution :: (Char, String) -> String
showSolution (x, "")  = x : " = ε"
showSolution (x, "ε") = x : " = ε"
showSolution (x, s)   = x : " = " ++ filter (/= 'ε') s

showIndentedList :: (a -> String) -> [a] -> String
showIndentedList _ [] = "[]"
showIndentedList f xs = intercalate "\n" (["["] ++ map ("    " ++) (map f xs) ++ ["]"])

variablePrefix :: RewriteOperation r -> Maybe (Variable r, Sequence r)
variablePrefix DeleteTerminalPrefix = Nothing
variablePrefix DeleteVariablePrefix = Nothing
variablePrefix (VariableIsEmpty side x _) = Just (x, ε)
variablePrefix (VariableStartsWithTerminal side x a _) = Just (x, toSequence a)
variablePrefix (VariableStartsWithVariable side x y _) = Just (x, toSequence y)

colorPrefix :: NielsenTransformable r => (String -> String) -> Sequence r -> String
colorPrefix _ []     = error "Sequence cannot be empty"
colorPrefix f (x:xs) = concat (f (showSymbol x) : map showSymbol xs)

colorOccurrences :: (Eq r, NielsenTransformable r, ToSymbol a r) => a -> (String -> String) -> Sequence r -> String
colorOccurrences _ _ [] = "ε"
colorOccurrences x f s = concat (map (\y -> if toSymbol x == y then f (showSymbol y) else showSymbol y) s)

colorOccurrencesExceptFirst :: (Eq r, NielsenTransformable r, ToSymbol a r) => a -> (String -> String) -> Sequence r -> String
colorOccurrencesExceptFirst _ _ [] = "ε"
colorOccurrencesExceptFirst x f (s:ss) = showSymbol s ++ concat (map (\y -> if toSymbol x == y then f (showSymbol y) else showSymbol y) ss)

colorReplacedOccurrencesExceptFirst :: (Eq r, NielsenTransformable r) => Replacement r -> (String -> String) -> Sequence r -> String
colorReplacedOccurrencesExceptFirst _ _ [] = "ε"
colorReplacedOccurrencesExceptFirst (Replacement x ys) f (s:ss) = showSymbol s ++ concat (ss >>= (\x' -> if x == x' then map f (map showSymbol ys) else [showSymbol x']))
