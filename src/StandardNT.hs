{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation

instance NielsenTransformable () where
    aa (_, (α, β)) = [(α :=: β, DeleteTerminalPrefix)]

    xx (_, (α, β)) = [(α :=: β, DeleteVariablePrefix)]

    xε ((x, α), ((), β)) = [(φ α :=: φ β, VariableIsEmpty Left' x (Replacement (toSymbol x) ε))]
        where φ = replace x ε

    xa ((x, α), (a, β))
        = [(x · φ α :=: φ β, VariableStartsWithTerminal Left' x a (Replacement (toSymbol x) (a · x)))]
        where φ = replace x (a · x)

    xy ((x, α), (y, β))
        = [(x · φ1 α :=: φ1 β, VariableStartsWithVariable Left' x y (Replacement (toSymbol x) (y·x))), (φ2 α :=: y · φ2 β, VariableStartsWithVariable Right' y x (Replacement (toSymbol y) (x·y)))]
        where φ1 = replace x (y·x)
              φ2 = replace y (x·y)

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
