module PP
  ( module Text.PrettyPrint.GenericPretty
  , OutShow (..)
  ) where

import Data.Functor.Identity
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.GenericPretty

-- | Wrapper to make `GenericPretty.Out` from `Show`.
newtype OutShow a = OutShow { getOutShow :: a }
  deriving newtype (Show, Read, Eq, Ord, Enum, Bounded, Num, Semigroup, Monoid)
  deriving stock (Generic, Functor)
  deriving Applicative via Identity
  deriving Monad via Identity

instance Show a => Out (OutShow a) where
  doc = PP.text . show . getOutShow
  docPrec = const doc
