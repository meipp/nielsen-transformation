{-# OPTIONS_GHC -Wno-orphans #-}

module StandardNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation
import Data.Side

instance NielsenTransformable () where
    aa (_, (_, _)) = [DeleteTerminalPrefix]

    xx (_, (_, _)) = [DeleteVariablePrefix]

    xε ((x, _), ((), _)) = [VariableIsEmpty Left' x (Replacement (toSymbol x) ε)]

    xa ((x, _), (a, _))
        = [VariableStartsWithTerminal Left' x a (Replacement (toSymbol x) (a · x))]

    xy ((x, _), (y, _))
        = [VariableStartsWithVariable Left' x y (Replacement (toSymbol x) (y·x)), VariableStartsWithVariable Right' y x (Replacement (toSymbol y) (x·y))]

    nullable () = True

    satisfiable () = length alphabet /= 0

    showSymbol (Left (Terminal a)) = [a]
    showSymbol (Right (Variable x _)) = [x]
