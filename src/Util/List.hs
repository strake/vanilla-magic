module Util.List where

sortAndGroupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
sortAndGroupBy cmp = groupBy (\ x y -> cmp x y == EQ) . sortBy cmp
