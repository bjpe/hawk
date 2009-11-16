{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeSynonymInstances #-}
module Data.Stringable where

class Stringable a where
    toString :: a -> String

instance Stringable String where
    toString = id

instance Show a => Stringable a where
    toString = show
