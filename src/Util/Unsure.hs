module Util.Unsure where

pedantic :: Bool
pedantic = False

unsure :: a -> a
unsure x
    | pedantic  = error "I am unsure about this computation"
    | otherwise = x
