module Data.Mana.Cost1 (Cost1, (∪), acceptableColors, mono, generic) where

import Data.Color
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

newtype Cost1 = Cost1 (Set Color) deriving (Eq, Ord)

instance Show Cost1 where
    show = List.intercalate "/" . fmap show . Set.toList . acceptableColors

infixl 6 ∪
(∪) :: Cost1 -> Cost1 -> Cost1
Cost1 xs ∪ Cost1 ys = Cost1 (Set.union xs ys)

mono :: Color -> Cost1
mono = Cost1 . Set.singleton

generic :: Cost1
generic = Cost1 (Set.fromList [W, U, B, R, G])

acceptableColors :: Cost1 -> Set Color
acceptableColors (Cost1 xs) = xs
