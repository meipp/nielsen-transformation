module Handcrafted.BenchmarkSet where

import NielsenTransformation (Equation((:=:)), Variable (Variable))
import NoRegex.Example (a, b, x, y)
import Brzozowski.Regex
import Brzozowski.Example (a', b')

satNoRegex1 :: Equation ()
satNoRegex1 = [x] :=: [a]

satNoRegex2 :: Equation ()
satNoRegex2 = [x, a] :=: [b, y]

unsatNoRegex1 :: Equation ()
unsatNoRegex1 = [x, a] :=: [b, y, x]

satRegex1 :: Equation Regex
satRegex1 = [x] :=: [a', a', a', b']
    where x = Right (Variable 'x' (Concatenation (Iterate (Symbol 'a')) (Symbol 'b')))

satRegex2 :: Equation Regex
satRegex2 = [x, b'] :=: [y, x, b']
    where x = Right (Variable 'x' (Concatenation (Symbol 'a') (Iterate (Symbol 'b'))))
          y = Right (Variable 'y' (Or (Symbol 'a') (Iterate (Symbol 'b'))))

unsatRegex1 :: Equation Regex
unsatRegex1 = [x] :=: [a', a', a']
    where x = Right (Variable 'x' (Concatenation (Iterate (Symbol 'a')) (Symbol 'b')))

unsatRegex2 :: Equation Regex
unsatRegex2 = [x, b', y] :=: [y, x]
    where x = Right (Variable 'x' (Iterate (Or (Symbol 'a') (Symbol 'b'))))
          y = Right (Variable 'y' (Concatenation (Iterate (Symbol 'b')) (Symbol 'a')))

unsatRegex3 :: Equation Regex
unsatRegex3 = [x, b'] :=: [y, x]
    where x = Right (Variable 'x' (Concatenation (Symbol 'a') (Iterate (Symbol 'b'))))
          y = Right (Variable 'y' (Or (Symbol 'a') (Iterate (Symbol 'b'))))
