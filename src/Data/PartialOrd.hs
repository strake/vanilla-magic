module Data.PartialOrd where

import Data.Function (on)
import Data.Ord (Down (..))

infix 4 ≤

class PartialOrd a where
    partialCompare :: a -> a -> Maybe Ordering
    partialCompare x y
        | x ≤ y && y ≤ x = Just EQ
        | x ≤ y = Just LT
        | y ≤ x = Just GT
        | otherwise = Nothing

    (≤) :: a -> a -> Bool
    x ≤ y = Just False == ((== GT) <$> partialCompare x y)

instance PartialOrd () where
    partialCompare () () = Just EQ

instance PartialOrd a => PartialOrd (Down a) where
    partialCompare (Down x) (Down y) = partialCompare y x

instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b) where
    (u, x) ≤ (v, y) = u ≤ v && x ≤ y

instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a, b, c) where
    partialCompare = partialCompare `on` \ (a, b, c) -> (a, (b, c))

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d) => PartialOrd (a, b, c, d) where
    partialCompare = partialCompare `on` \ (a, b, c, d) -> (a, (b, (c, d)))

newtype Incomparable a = Incomparable a deriving (Eq)
instance Eq a => PartialOrd (Incomparable a) where (≤) = (==)
