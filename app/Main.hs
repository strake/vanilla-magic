{-# LANGUAGE OverloadedStrings, RecursiveDo, ViewPatterns, PartialTypeSignatures #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString.Lazy (maybeResult, parse)
import Data.Bool
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Color
import Data.Foldable
import Data.Foldable.Unicode
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Mana.Cost as Mana (Cost (..))
import qualified Data.Mana.Cost1 as Mana (Cost1)
import qualified Data.Mana.Cost as Mana.Cost
import qualified Data.Mana.Cost1 as Mana.Cost
import qualified Data.Maybe as List (mapMaybe)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MSet
import Data.Ord (Down (..))
import Data.PartialOrd
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Text.Read (readMaybe)
import Text.Regex.Applicative (RE)
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Text as RE.Text (match)
import qualified Text.Regex.Applicative.Common as RE
import Util
import Util.Applicative

main = do
    sets <- maybeResult . parse json' <$> BS.getContents >>= \ case
        Just (Object o) -> pure $ HM.mapMaybe (\ case Object o -> Just o
                                                      _ -> Nothing) o
        _ -> error "bad input"

    let setCards = HM.mapMaybe (HM.lookup "cards" >=> \ case
                                    Array a -> Just (List.mapMaybe (\ case Object o -> Just o
                                                                           _ -> Nothing) $ toList a)
                                    _ -> Nothing) sets

    mapM_ print . List.sortBy (compare `on` \ (setName, _) ->
                                   ((HM.lookup setName >=> HM.lookup "releaseDate") >>> \ case Just (String t) -> t
                                                                                               _ -> "") sets) . HM.toList $
        List.sortBy (compare `on` \ (_, _, Mana.Cost c, _, _) -> liftA2 (,) MSet.size id c) .
        fmap (\ (Incomparable (a, b), c, Down d, Down e) -> (a, b, c, d, e)) .
        findMins . List.mapMaybe frenchVanillaCreatureStats <$> setCards

frenchVanillaCreatureStats :: Object -> Maybe (Incomparable (Bool, _ Text),
                                               Mana.Cost, Down PT, Down FrenchVanillaFlavor)
frenchVanillaCreatureStats o
  | isCreature o,
    String text <- fromMaybe (String "") $ HM.lookup "text" o,
    Just flavor <- frenchVanillaFlavor text =
      (liftA5 . liftA5) (\ l τs p t c ->
                         (Incomparable (l, τs), c, Down (PT p t), Down flavor))
      (Just . isLegendary)
      (HM.lookup "types" >=> \ case Array ts -> traverse (\ case String s -> Just s
                                                                 _ -> Nothing) ts
                                    _ -> Nothing)
      (readMaybe <=< (\ case String t -> Just (Text.unpack t)
                             _ -> Nothing) <=< HM.lookup "power")
      (readMaybe <=< (\ case String t -> Just (Text.unpack t)
                             _ -> Nothing) <=< HM.lookup "toughness")
      ((\ case Success a -> Just a
               _ -> Nothing) . fromJSON <=< HM.lookup "manaCost") o
  | otherwise = Nothing

isCreature :: Object -> Bool
isCreature o
  | Just (Array ts) <- HM.lookup "types" o,
    any (== "Creature") ts = True
  | otherwise = False

isLegendary :: Object -> Bool
isLegendary o
  | Just (Array ts) <- HM.lookup "supertypes" o,
    any (== "Legendary") ts = True
  | otherwise = False

frenchVanillaFlavor :: Text -> Maybe FrenchVanillaFlavor
frenchVanillaFlavor = fmap FrenchVanillaFlavor . go . Text.toLower
  where go s
          | Text.null s = Just mempty
          | (k, Text.null -> True) <- spanKeyword s = Just (Set.singleton k)
          | (k, t) <- spanKeyword s = bool (Set.insert k <$> (go <=< dropSep) t) Nothing (Text.null t)
        spanKeyword = Text.span (liftA2 (||) isAlphaNum (== ' '))
        dropSep = Text.uncons & \ case Just (x, t) | x == '\n' || x == ',' -> Just (go 0 t)
                                       _ -> Nothing
          where go n s = case Text.uncons s of
                    Just (x, t)
                      | isSpace x -> go n t
                      | '(' <- x -> go (n + 1) t
                      | ')' <- x, n > 0 -> go (n - 1) t
                      | n > 0 -> go n t
                    _ -> s

valentStaticAbilities :: HashMap Text Valence
valentStaticAbilities = HM.fromList
    [("deathtouch", Ben),
     ("defender", Mal),
     ("double strike", Ben),
     ("first strike", Ben),
     ("flash", Ben),
     ("flying", Ben),
     ("haste", Ben),
     ("hexproof", Ben),
     ("indestructible", Ben),
     ("intimidate", Ben),
     ("lifelink", Ben),
     ("menace", Ben),
     ("reach", Ben),
     ("trample", Ben)]

data Valence = Ben | Mal deriving (Eq)

isValent :: Valence -> Text -> Bool
isValent v a = Just v == HM.lookup a valentStaticAbilities

isNeutral :: Text -> Bool
isNeutral a = Nothing == HM.lookup a valentStaticAbilities

newtype FrenchVanillaFlavor = FrenchVanillaFlavor (Set Text)

instance Show FrenchVanillaFlavor where
    show (FrenchVanillaFlavor ts) = "[" ++ List.intercalate ", " (Text.unpack . Text.toTitle <$> toList ts) ++ "]"

instance PartialOrd FrenchVanillaFlavor where
    FrenchVanillaFlavor as ≤ FrenchVanillaFlavor bs =
        (Set.isSubsetOf `on` Set.filter (isValent Ben)) as bs &&
        (Set.isSubsetOf `on` Set.filter (isValent Mal)) bs as &&
        ((==)           `on` Set.filter (isNeutral))    as bs

data PT = PT Word Word

instance Show PT where
    show (PT p t) = show p ++ "/" ++ show t

instance PartialOrd PT where
    PT p1 t1 ≤ PT p2 t2 = p1 <= p2 && t1 <= t2

instance FromJSON Mana.Cost where
    parseJSON = withText "mana cost" $ maybe (fail "mana cost") pure . RE.Text.match re
      where re :: RE Char Mana.Cost
            re = RE.reFoldl RE.Greedy (<>) mempty (RE.sym '{' *> (Mana.Cost <$> re1) <* RE.sym '}')

            re1 :: RE Char (MultiSet Mana.Cost1)
            re1 = flip (MSet.insertMany Mana.Cost.generic) mempty <$> RE.decimal <|>
                  MSet.singleton <$> foldl1SepBy1 (RE.sym '/') (Mana.Cost.∪) (Mana.Cost.mono <$> reColor)

            reColor :: RE Char Color
            reColor = W <$ RE.sym 'W' <|>
                      U <$ RE.sym 'U' <|>
                      B <$ RE.sym 'B' <|>
                      R <$ RE.sym 'R' <|>
                      G <$ RE.sym 'G'

foldl1SepBy1 :: RE s b -> (a -> a -> a) -> RE s a -> RE s a
foldl1SepBy1 sep f x = liftA2 (\ x -> \ case [] -> x
                                             xs -> List.foldl' f x xs) x (many (sep *> x))

findMinsBy :: Foldable f => (a -> a -> Maybe Ordering) -> f a -> [a]
findMinsBy f = foldr (\ x -> join $ bool (x:) id . any (≤ x)) []
  where x ≤ y = Just False == ((== GT) <$> f x y)

findMins = findMinsBy partialCompare
