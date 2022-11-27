module NoRegex.Example where

import NielsenTransformation

a :: Symbol ()
a = Left (Terminal 'a')
b :: Symbol ()
b = Left (Terminal 'b')
x :: Symbol ()
x = Right (Variable 'x' ())
y :: Symbol ()
y = Right (Variable 'y' ())
