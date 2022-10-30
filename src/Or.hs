module Or where

import Data.Bifunctor (Bifunctor(bimap))

data Or a b = Left' a | Right' b | Both a b
    deriving (Eq, Ord, Show)

instance Bifunctor Or where
    bimap f _ (Left' x)  = Left' (f x)
    bimap _ g (Right' y) = Right' (g y)
    bimap f g (Both x y) = Both (f x) (g y)

fromEither :: Either a b -> Or a b
fromEither (Left x)  = Left' x
fromEither (Right y) = Right' y

toEither :: Or a b -> [Either a b]
toEither (Left' x)  = [Left x]
toEither (Right' y) = [Right y]
toEither (Both x y) = [Left x, Right y]
