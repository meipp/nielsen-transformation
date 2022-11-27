module Data.Side where

type Side = Either () ()

left :: Side
left = Left ()

right :: Side
right = Right ()
