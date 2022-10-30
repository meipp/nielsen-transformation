module Example where

import NielsenTransformation
import Brzozowski

a :: Symbol ()
a = Left (Terminal 'a')
b :: Symbol ()
b = Left (Terminal 'b')
x :: Symbol ()
x = Right (Variable 'x' ())
y :: Symbol ()
y = Right (Variable 'y' ())

-- a :: Symbol Regex
-- a = Left (Terminal 'a')
-- b :: Symbol Regex
-- b = Left (Terminal 'b')
-- x :: Symbol Regex
-- x = Right (Variable 'x' (Symbol 'b' `Or` Lambda))
-- y :: Symbol Regex
-- y = Right (Variable 'y' (Symbol 'a' `Or` Symbol 'b'))