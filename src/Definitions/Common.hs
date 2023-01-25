module Definitions.Common where

import NielsenTransformation

terminal :: Char -> Symbol r
terminal t = Left (Terminal t)

a :: Symbol r
a = terminal 'a'

b :: Symbol r
b = terminal 'b'

c :: Symbol r
c = terminal 'c'

d :: Symbol r
d = terminal 'd'

constrainedVariable :: Char -> r -> Symbol r
constrainedVariable x r = Right (Variable x r)

constrainX :: r -> Symbol r
constrainX = constrainedVariable 'x'

constrainY :: r -> Symbol r
constrainY = constrainedVariable 'y'

constrainZ :: r -> Symbol r
constrainZ = constrainedVariable 'z'
