module TestUtils
 ( module TestUtils
 , module Data.Coerce
 , module Data.Proxy
 , module Test.HUnit
 , module Test.QuickCheck
 , module Utils
 ) where

import Control.Exception
import Data.Coerce (Coercible)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Proxy
import System.Environment
import Test.HUnit (Test (..), assertFailure)
import Test.HUnit qualified as HU
import Test.QuickCheck (Arbitrary (..), (===), (.&&.), (==>))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Classes.Base qualified as QC
import Test.QuickCheck.Gen qualified as QC
import Utils

type NamedTests = [(String, Test)]

testMain :: NamedTests -> IO ()
testMain tests = do
  activeTestNames <- concatMap words <$> getArgs
  putStrLn $ "\nExecuting: " <> showNames activeTestNames
  let activeTests = filterTests activeTestNames tests
  HU.runTestTTAndExit activeTests
  where
    showNames names
      | null names = "all"
      | otherwise = List.intercalate ", " names

filterTests :: [String] -> NamedTests -> Test
filterTests names tests = TestList
  [test | (name, test) <- tests, name `elem` names || null names]

propertyToTestIO :: QC.Testable prop => String -> IO prop -> Test
propertyToTestIO description property = TestCase $
  property >>= QC.quickCheckWithResult args >>= \case
    QC.Failure { theException = Just exception } ->
      case fromException @TodoException exception of
        Just _ -> throwIO exception
        Nothing -> assertFailure $ "exception occured: " <> show exception
    QC.Failure { theException = Nothing, .. } ->
      assertFailure $ "expected " <> description <> ", but\n" <> output
    _ -> pure ()
  where
    args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 100 }

propertyToTest :: QC.Testable prop => String -> prop -> Test
propertyToTest description = propertyToTestIO description . pure

lawsToTest :: QC.Laws -> Test
lawsToTest QC.Laws {..} = TestList $ uncurry mkTest <$> lawsProperties
  where
    mkTest name = propertyToTest (lawsTypeclass <> "." <> name <> " satisfied")

nameTests :: Int -> [Test] -> NamedTests
nameTests iBlock = zipWith mkNamedTest [1 :: Int ..]
  where
    mkNamedTest iTask test = (show iBlock <> "." <> show iTask, test)

data TooEagerException = TooEagerException
instance Exception TooEagerException
instance Show TooEagerException where
  show TooEagerException = "Форсировали ненужные данные"

infinite :: forall a. a
infinite = throw TooEagerException

domain :: (Enum a, Bounded a) => [a]
domain = [minBound .. maxBound]

deriving newtype instance Arbitrary a => Arbitrary (Id a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Done f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Todo f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Dispatcher l f a)

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = NE.fromList . QC.getNonEmpty <$> arbitrary

instance Show (a -> b) where
  show _ = "<fun>"

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  xs == ys = map xs domain == map ys domain
