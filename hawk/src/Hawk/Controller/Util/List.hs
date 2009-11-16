module Hawk.Controller.Util.List
    ( splitAll
    , splitWhere
    , lookupFirst
    , replaceOrAdd
    ) where

splitAll :: (a -> Bool) -> [a] -> [[a]]
splitAll _ [] = []
splitAll p xs = f : splitAll p s
  where (f, s) = splitWhere p xs

splitWhere :: (a -> Bool) -> [a] -> ([a], [a])
splitWhere p xs = (f, drop 1 s)
  where (f, s) = break p xs

-- lookupFirst = safeHead . filter p
lookupFirst :: (a -> Bool) -> [a] -> Maybe a
lookupFirst _ [] = Nothing
lookupFirst p (x:xs)
  | p x       = Just x
  | otherwise = lookupFirst p xs

replaceOrAdd :: (a -> a -> Bool) -> a -> [a] -> [a]
replaceOrAdd _ new [] = [new]
replaceOrAdd eq new (x:xs)
  | new `eq` x = new : xs
  | otherwise  = x : replaceOrAdd eq new xs
