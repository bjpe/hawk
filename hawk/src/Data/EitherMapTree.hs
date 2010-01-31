module Data.EitherMapTree where

import qualified Data.Map as M

data EitherMapTree a b = Node {
    subNodes :: M.Map a (Either [EitherMapTree a b] b)
    }
    deriving (Show)

lookup :: (Ord a) => a -> EitherMapTree a b -> Maybe (Either [EitherMapTree a b] b)
lookup k (Node sn) = M.lookup k sn

fromList :: Ord a => [(a, Either [EitherMapTree a b] b)] -> EitherMapTree a b
fromList = Node . M.fromList
