module Test.Prelude
  ( module Data.Coerce
  , module Data.Proxy
  , module Test.HUnit
  , module Test.Lazy
  , module Test.Properties
  , module Test.QuickCheck
  , module Test.Run
  , module Test.Utils
  , module MetaUtils
  ) where

import Data.Coerce (Coercible)
import Data.Proxy
import Test.HUnit (Test (..), assertFailure, assertEqual, assertBool)
import Test.QuickCheck (Arbitrary (..), (===), (.&&.), (==>))
import Test.Run
import Test.Lazy
import Test.Properties
import Test.Utils
import MetaUtils
