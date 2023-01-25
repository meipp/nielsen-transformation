module Handcrafted.BenchmarkSet where

import NielsenTransformation (Equation((:=:)))
import Definitions.Common (a, b, constrainX, constrainY)
import Brzozowski.Regex (Regex(..))

satNoRegex1 :: Equation ()
satNoRegex1 = [x] :=: [a]
    where x = constrainX ()

satNoRegex2 :: Equation ()
satNoRegex2 = [x, a] :=: [b, y]
    where x = constrainX ()
          y = constrainY ()

unsatNoRegex1 :: Equation ()
unsatNoRegex1 = [x, a] :=: [b, y, x]
    where x = constrainX ()
          y = constrainY ()

satRegex1 :: Equation Regex
satRegex1 = [x] :=: [a, a, a, b]
    where x = constrainX (Concatenation (Iterate (Symbol 'a')) (Symbol 'b'))

satRegex2 :: Equation Regex
satRegex2 = [x, b] :=: [y, x, b]
    where x = constrainX (Concatenation (Symbol 'a') (Iterate (Symbol 'b')))
          y = constrainY (Or (Symbol 'a') (Iterate (Symbol 'b')))

unsatRegex1 :: Equation Regex
unsatRegex1 = [x] :=: [a, a, a]
    where x = constrainX (Concatenation (Iterate (Symbol 'a')) (Symbol 'b'))

unsatRegex2 :: Equation Regex
unsatRegex2 = [x, b, y] :=: [y, x]
    where x = constrainX (Iterate (Or (Symbol 'a') (Symbol 'b')))
          y = constrainY (Concatenation (Iterate (Symbol 'b')) (Symbol 'a'))

unsatRegex3 :: Equation Regex
unsatRegex3 = [x, b] :=: [y, x]
    where x = constrainX (Concatenation (Symbol 'a') (Iterate (Symbol 'b')))
          y = constrainY (Or (Symbol 'a') (Iterate (Symbol 'b')))
