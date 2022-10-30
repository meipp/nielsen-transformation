{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances,
    AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NielsenTransformation where

import Data.Either (Either (..))
import Data.Function (on)
import Data.List (intercalate, nub)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceShow)
import Prelude hiding (Either (..))

newtype Terminal = Terminal Char
    deriving Eq

data Variable r = Variable Char r
    deriving Eq

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

class ToSequence a r where
    toSequence :: a -> Sequence r

instance ToSequence (Symbol r) r where
    toSequence = return

instance ToSequence (Sequence r) r where
    toSequence = id

(·) :: (ToSequence a r, ToSequence b r) => a -> b -> Sequence r
α · β = toSequence α ++ toSequence β

data Trace a = Start a | Rewrite (Trace a) RewriteOperation a

-- This loses some solutions
-- if we would just derive Eq the Trace history would play into (==), blowing up the nub call in nielsenTransformation
instance Eq a => Eq (Trace a) where
    (==) = (==) `on` value

data Side = Left' | Right' deriving (Eq, Show)

data RewriteOperation = DeleteTerminalPrefix
                      | DeleteVariablePrefix
                      | VariableIsEmpty Side Char
                      | VariableStartsWithTerminal Side Char Char
                      | VariableStartsWithVariable Side Char Char
                      deriving (Eq, Show)

value :: Trace a -> a
value (Start x) = x
value (Rewrite _ _ x) = x

backtrace :: Trace a -> [(a, RewriteOperation, a)]
backtrace (Start _) = []
backtrace (Rewrite r o x) = (value r, o, x) : backtrace r

type RewriteRule a = a -> [(a, RewriteOperation)]

rewriteTrace :: [RewriteRule a] -> Trace a -> [Trace a]
rewriteTrace rules t = map (\(e, o) -> Rewrite t o e) (rules >>= (\r -> r (value t)))

rewriteTraces :: [RewriteRule a] -> [Trace a] -> [Trace a]
rewriteTraces rules xs = xs >>= rewriteTrace rules

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

instance Swap RewriteOperation where
    swap DeleteTerminalPrefix = DeleteTerminalPrefix
    swap DeleteVariablePrefix = DeleteVariablePrefix
    swap (VariableIsEmpty side x) = VariableIsEmpty (swap side) x
    swap (VariableStartsWithTerminal side x a) = VariableStartsWithTerminal (swap side) x a
    swap (VariableStartsWithVariable side x y) = VariableStartsWithVariable (swap side) x y

onBothSides :: RewriteRule (Equation r) -> RewriteRule (Equation r)
onBothSides r e = r e ++ map swap (r (swap e))
    -- where swap (α :=: β) = (β :=: α)
-- onBothSides = id
-- onBothSides = error "onBothSides is not defined"

with_aa :: ((Terminal, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation)]) -> RewriteRule (Equation r)
with_aa f ((Left a):α :=: (Left b):β)
    | a == b = f (a, (α, β))
    | otherwise = []
with_aa _ _ = []

with_xx :: Eq r => ((Variable r, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation)]) -> RewriteRule (Equation r)
with_xx f ((Right x):α :=: (Right y):β)
    | x == y = f (x, (α, β))
    | otherwise = []
with_xx _ _ = []

with_xε :: (((Variable r, Sequence r), ((), Sequence r)) -> [(Equation r, RewriteOperation)]) -> RewriteRule (Equation r)
with_xε f ((Right x):α :=: β)
    -- TODO: only if x nullable
    | True = f ((x, α), ((), β))
with_xε _ _ = []

with_xa :: (((Variable r, Sequence r), (Terminal, Sequence r)) -> [(Equation r, RewriteOperation)]) -> RewriteRule (Equation r)
with_xa f ((Right x):α :=: (Left a):β)
    -- TODO: only if x can start with a
    | True = f ((x, α), (a, β))
with_xa _ _ = []

with_xy :: Eq r => (((Variable r, Sequence r), (Variable r, Sequence r)) -> [(Equation r, RewriteOperation)]) -> RewriteRule (Equation r)
with_xy f ((Right x):α :=: (Right y):β)
    -- TODO: only if x can be satisfied
    -- TODO: only if y can be satisfied
    | x /= y = f ((x, α), (y, β))
    | otherwise = []
with_xy _ _ = []

class NielsenTransformable r where
    -- rule 1
    aa :: (Terminal, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation)]
    xx :: (Variable r, (Sequence r, Sequence r)) -> [(Equation r, RewriteOperation)]

    -- rule 2
    xε :: ((Variable r, Sequence r), ((), Sequence r)) -> [(Equation r, RewriteOperation)]

    -- rule 3
    xa :: ((Variable r, Sequence r), (Terminal, Sequence r)) -> [(Equation r, RewriteOperation)]

    -- rule 4
    xy :: ((Variable r, Sequence r), (Variable r, Sequence r)) -> [(Equation r, RewriteOperation)]

    -- prerequisites
    nullable :: Symbol r -> Bool
    satisfiable :: Symbol r -> Bool
    showSymbol :: Symbol r -> String

replaceSymbol :: Eq r => Symbol r -> Sequence r -> Symbol r -> Sequence r
replaceSymbol x y x'
    | x == x' = y
    | otherwise = [x']

replace :: Eq r => Symbol r -> Sequence r -> Sequence r -> Sequence r
replace x y xs = xs >>= replaceSymbol x y


showSequence :: NielsenTransformable r => Sequence r -> String
showSequence [] = "ε"
showSequence α  = α >>= showSymbol

showEquation :: NielsenTransformable r => Equation r -> String
showEquation (α :=: β) = showSequence α ++ " = " ++ showSequence β

showEquations :: NielsenTransformable r => [Equation r] -> String
showEquations es = "[" ++ intercalate ", " (map showEquation es) ++ "]"

nielsenTransformation :: (NielsenTransformable r, Eq r) => [Trace (Equation r)] -> [Trace (Equation r)]
nielsenTransformation [] = []
nielsenTransformation ts = trace (showEquations es) (filter (\t -> value t == (ε :=: ε)) ts `listOr` nielsenTransformation (nub (rewriteTraces rewriteRules ts)))
    where es = map value ts
          rewriteRules = [
                with_aa aa,
                with_xx xx,
                onBothSides (with_xε xε),
                onBothSides (with_xa xa),
                onBothSides (with_xy xy)
            ]

listOr :: [a] -> [a] -> [a]
listOr [] ys = ys
listOr xs _  = xs

nielsen :: (NielsenTransformable r, Eq r) => Equation r -> Bool
nielsen e = case nielsenTransformation [Start e] of
    []    -> False
    (t:_) -> trace ("trace: " ++ showRewrites bt) (traceShow (extractSolutionVariablePrefixes (map (\(_, o, _) -> o) bt)) True)
        where bt = (reverse (backtrace t))

showRewrite :: NielsenTransformable r => (Equation r, RewriteOperation, Equation r) -> String
showRewrite (e1, o, e2) = showEquation e1 ++ " [" ++ show o ++ "] " ++ showEquation e2

showRewrites :: NielsenTransformable r => [(Equation r, RewriteOperation, Equation r)] -> String
showRewrites es = "[\n    " ++ intercalate "\n    " (map showRewrite es) ++ "\n]"

extractSolutionVariablePrefixes :: [RewriteOperation] -> [(Char, Char)]
extractSolutionVariablePrefixes = mapMaybe variablePrefix


variablePrefix :: RewriteOperation -> Maybe (Char, Char)
variablePrefix DeleteTerminalPrefix = Nothing
variablePrefix DeleteVariablePrefix = Nothing
variablePrefix (VariableIsEmpty side x) = Just (x, 'ε')
variablePrefix (VariableStartsWithTerminal side x a) = Just (x, a)
variablePrefix (VariableStartsWithVariable side x y) = Just (x, y)
