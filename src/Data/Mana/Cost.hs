module Data.Mana.Cost where

import Control.Applicative
import Data.Bool (bool)
import Data.Color
import Data.Foldable
import Data.Function (on)
import Data.Mana.Cost1
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.PartialOrd
import Data.Set (Set)
import qualified Data.Set as Set

newtype Cost = Cost (MultiSet Cost1) deriving (Eq)

instance Show Cost where
    show (Cost c) = bool "" ("{" ++ show (MSet.occur generic c) ++ "}") (liftA2 (||) (any (== generic)) null c) ++
                    concatMap (\ x -> "{" ++ show x ++ "}") (MSet.deleteAll generic c)

instance Monoid Cost where
    mempty = Cost mempty
    Cost x `mappend` Cost y = Cost (x `mappend` y)

instance PartialOrd Cost where
    Cost x ≤ Cost y | MSet.size x > MSet.size y = False | otherwise =
        ((\ x y -> all (`pays` x) (payments y)) `on` Cost . (MSet.\\ MSet.intersection x y)) x y

payments :: Cost -> Set Payment
payments (Cost m) = case MSet.minView m of
    Just (x, n) -> liftS2 MSet.insert (acceptableColors x) (payments (Cost n))
    Nothing -> Set.empty

liftS2 :: (Ord c) => (a -> b -> c) -> Set a -> Set b -> Set c
liftS2 f xs ys = Set.fromList $ liftA2 f (toList xs) (toList ys)

pays :: Payment -> Cost -> Bool
pays p = any (`MSet.isSubsetOf` p) . payments

type Payment = MultiSet Color
