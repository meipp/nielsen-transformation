{-# OPTIONS_GHC  -Wno-orphans #-}

module BrzozowskiNT where

import Prelude hiding (Either (..))
import Data.Either (Either (..))
import NielsenTransformation
import Brzozowski (Regex (..), deriveSymbol')
import qualified Brzozowski
import Util.Unsure (unsure)
import Data.Side

showRegex :: Regex -> String
showRegex Phi = "Ø"
showRegex Lambda = "ε"
showRegex (Symbol a) = [a]
showRegex (Concatenation p q) = "(" ++ showRegex p ++ "" ++ showRegex q ++ ")"
showRegex (Iterate p) = "(" ++ showRegex p ++ ")*"
showRegex (Not p) = "(" ++ showRegex p ++ ")'"
showRegex (And p q) = "(" ++ showRegex p ++ "&" ++ showRegex q ++ ")"
showRegex (Or p q) = "(" ++ showRegex p ++ "|" ++ showRegex q ++ ")"

instance NielsenTransformable Regex where
    aa (_, (_, _)) = [DeleteTerminalPrefix]

    xx (Variable _ p, (_, _))
        | satisfiable p = [DeleteVariablePrefix]
        | otherwise     = []

    xε ((x@(Variable _ p), _), ((), _))
        | nullable p = [VariableIsEmpty left x (Replacement (toSymbol x) ε)]
        | otherwise  = []

    xa ((x@(Variable xc p), _), (Terminal a, _))
        | satisfiable dap = [VariableStartsWithTerminal left x (Terminal a) (Replacement (toSymbol x) (Terminal a · x'))]
        | otherwise       = []
        where dap = deriveSymbol' a p
              x'  = Variable xc dap

    xy ((x@(Variable xc p), _), ((Variable _ q), _))
        -- TODO
        -- = undefined
        = alphabet >>= helper
        where helper a
                -- TODO is this correct?
                | satisfiable dap && satisfiable daq = [VariableStartsWithTerminal left x ta (Replacement (toSymbol x) (ta · Variable xc dap))]
                | otherwise = []
                where dap = deriveSymbol' a p
                      daq = deriveSymbol' a q
                      ta  = Terminal a
        -- = [VariableStartsWithVariable left x y (Replacement (toSymbol x) (y·x)), VariableStartsWithVariable right y x (Replacement (toSymbol y) (x·y))]

    nullable = Brzozowski.nullable
    satisfiable p = not (Brzozowski.impossible p) && length alphabet /= 0

    showSymbol (Left (Terminal a)) = [a]
    showSymbol (Right (Variable x p)) = [x] ++ "{" ++ showRegex p ++ "}"
