module Util where

(&) :: (a -> b) -> (b -> c) -> (a -> c)
(&) = flip (.)
