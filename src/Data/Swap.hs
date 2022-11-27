{-# LANGUAGE FlexibleInstances #-}

module Data.Swap where

class Swap a where
    swap :: a -> a

instance Swap (Either a a) where
    swap (Left x) = Right x
    swap (Right x) = Left x
