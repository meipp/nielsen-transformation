{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    FlexibleInstances,
    AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NielsenTransformation where

import Data.List (intercalate, nub)
import Debug.Trace (trace)

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

data Trace a = Start a | (Trace a) `Before` a deriving Eq

value :: Trace a -> a
value (Start x) = x
value (Before _ x) = x

backtrace :: Trace a -> [a]
backtrace (Start x) = [x]
backtrace (Before t x) = x : backtrace t

type RewriteRule a = a -> [a]

rewriteTrace :: [RewriteRule a] -> Trace a -> [Trace a]
rewriteTrace rules t = map (t `Before`) (rules >>= (\r -> r (value t)))

rewriteTraces :: [RewriteRule a] -> [Trace a] -> [Trace a]
rewriteTraces rules xs = xs >>= rewriteTrace rules

onBothSides :: RewriteRule (Equation r) -> RewriteRule (Equation r)
onBothSides r e = r e ++ map swap (r (swap e))
    where swap (α :=: β) = (β :=: α)

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
    (t:_) -> trace ("trace: " ++ showEquations (reverse (backtrace t))) True
