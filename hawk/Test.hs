module Test where
{-# LANGUAGE Rank2Types #-}
import Control.Monad.Trans

data TestType = TestType
  { tst :: (Conf a, MonadIO m) => m a }

class Conf a where
  get :: MonadIO m => m a

testFunc :: (forall a m. (Conf a, MonadIO m) => m a) -> TestType
testFunc = TestType
