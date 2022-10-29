{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances,
    AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NielsenTransformation where

import Data.Function (on)
import Data.List (intercalate, nub)
import Debug.Trace (trace)
import Prelude hiding (Either (..))

data Symbol r = Terminal Char
              | Variable Char r
              deriving Eq

type Sequence r = [Symbol r]

data Equation r = Sequence r :=: Sequence r
    deriving Eq
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

data Side = Left | Right deriving (Eq, Show)

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
    swap Left = Right
    swap Right = Left

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

class NielsenTransformable r where
    -- rule 1
    aa :: RewriteRule (Equation r)
    xx :: RewriteRule (Equation r)

    -- rule 2
    xε :: RewriteRule (Equation r)

    -- rule 3
    xa :: RewriteRule (Equation r)

    -- rule 4
    xy :: RewriteRule (Equation r)

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
                aa,
                xx,
                onBothSides xε,
                onBothSides xa,
                onBothSides xy
            ]

listOr :: [a] -> [a] -> [a]
listOr [] ys = ys
listOr xs _  = xs

nielsen :: (NielsenTransformable r, Eq r) => Equation r -> Bool
nielsen e = case nielsenTransformation [Start e] of
    []    -> False
    (t:_) -> trace ("trace: " ++ showRewrites (reverse (backtrace t))) True

showRewrite :: NielsenTransformable r => (Equation r, RewriteOperation, Equation r) -> String
showRewrite (e1, o, e2) = showEquation e1 ++ " [" ++ show o ++ "] " ++ showEquation e2

showRewrites :: NielsenTransformable r => [(Equation r, RewriteOperation, Equation r)] -> String
showRewrites es = "[\n    " ++ intercalate "\n    " (map showRewrite es) ++ "\n]"
