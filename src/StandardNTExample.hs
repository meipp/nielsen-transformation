module StandardNTExample where

a :: Symbol ()
a = Left (Terminal 'a')
b :: Symbol ()
b = Left (Terminal 'b')
x :: Symbol ()
x = Right (Variable 'x' ())
y :: Symbol ()
y = Right (Variable 'y' ())
