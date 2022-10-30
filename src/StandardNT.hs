{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation

instance NielsenTransformable () where
    aa ((Left (Terminal a)):α :=: (Left (Terminal b)):β)
        | a == b    = [(α :=: β, DeleteTerminalPrefix)]
        | otherwise = []
    aa _ = []

    xx ((Right (Variable x _)):α :=: (Right (Variable y _)):β)
        | x == y    = [(α :=: β, DeleteVariablePrefix)]
        | otherwise = []
    xx _ = []

    xε (x@(Right (Variable xx _)):α :=: β)
        | nullable x = [(φ α :=: φ β, VariableIsEmpty Left' xx)]
        | otherwise  = []
        where φ = replace x ε
    xε _ = []

    xa (x@(Right (Variable xx _)):α :=: a@(Left (Terminal aa)):β)
        = [(x · φ α :=: φ β, VariableStartsWithTerminal Left' xx aa)]
        where φ = replace x (a·x)
    xa _ = []

    xy (x@(Right (Variable xx _)):α :=: y@(Right (Variable yy _)):β)
        = [(x · φ1 α :=: φ1 β, VariableStartsWithVariable Left' xx yy), (φ2 α :=: y · φ2 β, VariableStartsWithVariable Right' yy xx)]
        where φ1 = replace x (y·x)
              φ2 = replace y (x·y)
    xy _ = []

    nullable (Left (Terminal _))   = False
    nullable (Right (Variable _ _)) = True

    satisfiable (Left (Terminal _)) = True
    satisfiable (Right (Variable _ _)) = True

    showSymbol (Left (Terminal a)) = [a]
    showSymbol (Right (Variable x _)) = [x]

a :: Symbol ()
a = Left (Terminal 'a')
b :: Symbol ()
b = Left (Terminal 'b')
x :: Symbol ()
x = Right (Variable 'x' ())
y :: Symbol ()
y = Right (Variable 'y' ())
