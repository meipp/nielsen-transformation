module Util where

without :: Eq a => [a] -> [a] -> [a]
without xs ys = filter (\x -> not (x `elem` ys)) xs
