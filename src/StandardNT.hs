{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module StandardNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation

instance NielsenTransformable () where
    aa (_, (α, β)) = [DeleteTerminalPrefix]

    xx (_, (α, β)) = [DeleteVariablePrefix]

    xε ((x, α), ((), β)) = [VariableIsEmpty Left' x (Replacement (toSymbol x) ε)]

    xa ((x, α), (a, β))
        = [VariableStartsWithTerminal Left' x a (Replacement (toSymbol x) (a · x))]

    xy ((x, α), (y, β))
        = [VariableStartsWithVariable Left' x y (Replacement (toSymbol x) (y·x)), VariableStartsWithVariable Right' y x (Replacement (toSymbol y) (x·y))]

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
