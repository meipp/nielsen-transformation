{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation

instance NielsenTransformable () where
    aa (_, (α, β)) = [(α :=: β, DeleteTerminalPrefix)]

    xx (_, (α, β)) = [(α :=: β, DeleteVariablePrefix)]

    xε ((x@(Variable xx _), α), ((), β)) = [(φ α :=: φ β, VariableIsEmpty Left' xx)]
        where φ = replace x ε

    xa ((x@(Variable xx _), α), (a@(Terminal aa), β))
        = [(x · φ α :=: φ β, VariableStartsWithTerminal Left' xx aa)]
        where φ = replace x (a · x)

    xy ((x@(Variable xx _), α), (y@(Variable yy _), β))
        = [(x · φ1 α :=: φ1 β, VariableStartsWithVariable Left' xx yy), (φ2 α :=: y · φ2 β, VariableStartsWithVariable Right' yy xx)]
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
