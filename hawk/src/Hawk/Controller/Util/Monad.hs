module Hawk.Controller.Util.Monad
    ( concatMapM
    , untilJustM
    , liftMaybe
    , ap' )
    where

import Control.Monad (liftM)


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
--foldr (liftM2 (++) . f) $ return []

-- | runs a List of monadic Maybe operations until one return Just
-- If no return Just a default is return
untilJustM :: Monad m => m a -> [m (Maybe a)] -> m a
untilJustM a []     = a
untilJustM a (x:xs) = x >>= maybe (untilJustM a xs) return

liftMaybe :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
liftMaybe = maybe (return Nothing)

-- | Like Control.Monad.ap, but runs first m a and then m (a -> b)
ap' :: Monad m => m (a -> b) -> m a -> m b
ap' f m = do
    m' <- m
    f' <- f
    return $ f' m'

