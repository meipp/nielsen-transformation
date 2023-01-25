module Brzozowski.Example where

import NielsenTransformation
import Brzozowski.Regex

x' :: Symbol Regex
x' = Right (Variable 'x' (Symbol 'b' `Or` Lambda))
y' :: Symbol Regex
y' = Right (Variable 'y' (Symbol 'a' `Or` Symbol 'b'))
