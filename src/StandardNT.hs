{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import NielsenTransformation

instance NielsenTransformable () where
    aa ((Terminal a):α :=: (Terminal b):β)
        | a == b    = [α :=: β]
        | otherwise = []
    aa _ = []

    xx ((Variable x _):α :=: (Variable y _):β)
        | x == y    = [α :=: β]
        | otherwise = []
    xx _ = []

    xε (x@(Variable _ _):α :=: β)
        | nullable x = [α :=: β]
        | otherwise  = []
    xε _ = []

    xa (x@(Variable _ _):α :=: a@(Terminal _):β)
        = [x · φ α :=: φ β]
        where φ = replace x (a·x)
    xa _ = []

    xy (x@(Variable _ _):α :=: y@(Variable _ _):β)
        = [x · φ1 α :=: φ1 β, φ2 α :=: y · φ2 β]
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
