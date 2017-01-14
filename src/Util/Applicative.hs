module Util.Applicative where

liftA4 :: Applicative p => (a -> b -> c -> d -> e) -> p a -> p b -> p c -> p d -> p e
liftA4 f w x y z = pure f <*> w <*> x <*> y <*> z

liftA5 :: Applicative p => (a -> b -> c -> d -> e -> f) -> p a -> p b -> p c -> p d -> p e -> p f
liftA5 f v w x y z = pure f <*> v <*> w <*> x <*> y <*> z
