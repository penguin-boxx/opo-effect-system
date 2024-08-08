module Test.Utils where

domain :: (Enum a, Bounded a) => [a]
domain = [minBound .. maxBound]

instance Show (a -> b) where
  show _ = "<fun>"

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  xs == ys = map xs domain == map ys domain
