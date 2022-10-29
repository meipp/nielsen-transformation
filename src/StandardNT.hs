{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import Prelude hiding (Either (..))
import NielsenTransformation

instance NielsenTransformable () where
    aa ((Terminal a):α :=: (Terminal b):β)
        | a == b    = [(α :=: β, DeleteTerminalPrefix)]
        | otherwise = []
    aa _ = []

    xx ((Variable x _):α :=: (Variable y _):β)
        | x == y    = [(α :=: β, DeleteVariablePrefix)]
        | otherwise = []
    xx _ = []

    xε (x@(Variable xx _):α :=: β)
        | nullable x = [(φ α :=: φ β, VariableIsEmpty Left xx)]
        | otherwise  = []
        where φ = replace x ε
    xε _ = []

    xa (x@(Variable xx _):α :=: a@(Terminal aa):β)
        = [(x · φ α :=: φ β, VariableStartsWithTerminal Left xx aa)]
        where φ = replace x (a·x)
    xa _ = []

    xy (x@(Variable xx _):α :=: y@(Variable yy _):β)
        = [(x · φ1 α :=: φ1 β, VariableStartsWithVariable Left xx yy), (φ2 α :=: y · φ2 β, VariableStartsWithVariable Right yy xx)]
        where φ1 = replace x (y·x)
              φ2 = replace y (x·y)
    xy _ = []

    nullable (Terminal _)   = False
    nullable (Variable _ _) = True

    satisfiable (Terminal _) = True
    satisfiable (Variable _ _) = True

    showSymbol (Terminal a) = [a]
    showSymbol (Variable x _) = [x]

a :: Symbol ()
a = Terminal 'a'
b :: Symbol ()
b = Terminal 'b'
x :: Symbol ()
x = Variable 'x' ()
y :: Symbol ()
y = Variable 'y' ()
