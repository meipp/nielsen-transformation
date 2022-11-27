{-# OPTIONS_GHC -Wno-orphans #-}

module NoRegex.NielsenTransformable where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation
import Data.Side

instance NielsenTransformable () where
    aa (_, (_, _)) = [DeleteTerminalPrefix]

    xx (_, (_, _)) = [DeleteVariablePrefix]

    xε ((x, _), ((), _)) = [VariableIsEmpty left x (Replacement (toSymbol x) ε)]

    xa ((x, _), (a, _))
        = [VariableStartsWithTerminal left x a (Replacement (toSymbol x) (a · x))]

    xy ((x, _), (y, _))
        = [VariableStartsWithVariable left x y (Replacement (toSymbol x) (y·x)), VariableStartsWithVariable right y x (Replacement (toSymbol y) (x·y))]

    nullable () = True

    satisfiable () = length alphabet /= 0

    showSymbol (Left (Terminal a)) = [a]
    showSymbol (Right (Variable x _)) = [x]
