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
        = [((Right x :: Symbol ()) · φ α :=: φ β, VariableStartsWithTerminal Left' xx aa)]
        where φ = replace x ((Left a :: Symbol ()) · (Right x :: Symbol ()))

    xy ((x@(Variable xx _), α), (y@(Variable yy _), β))
        = [((Right x :: Symbol ()) · φ1 α :=: φ1 β, VariableStartsWithVariable Left' xx yy), (φ2 α :=: (Right y :: Symbol ()) · φ2 β, VariableStartsWithVariable Right' yy xx)]
        where φ1 = replace x ((Right y :: Symbol ())·(Right x :: Symbol ()))
              φ2 = replace y ((Right x :: Symbol ())·(Right y :: Symbol ()))

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
