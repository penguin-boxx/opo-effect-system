import Control.Exception
import MetaUtils
import Test.HUnit (Test (..), assertFailure, assertEqual)
import Test.HUnit qualified as HU

data Data a = Ok !a | NotOk deriving Show

--todoImpl ''Data ''Eq
todoImpl ''Data ''Functor
todoImpl ''Data ''Applicative
todoImpl ''Data ''Monad
todoImpl ''Data ''Foldable
todoImpl ''Data ''Traversable

main :: IO ()
main = HU.runTestTTAndExit $ TestList
  [ testPP
  , testMacro
  ]

testPP :: Test
testPP = TestCase $ assertEqual "out show" "Ok 42" $ pretty $ OutShow $ Ok (42 :: Int)

testMacro :: Test
testMacro = TestList
  [ TestCase $ assertThrows $ fmap (+1) value
  , TestCase $ assertThrows $ value >>= \x -> value >>= \y -> pure (x + y)
  ]
  where
    value = Ok 42 :: Data Int

    assertThrows :: a -> IO ()
    assertThrows comp = try (evaluate comp) >>= \case
      Left e -> case fromException @TodoException e of
        Just _ -> pure ()
        Nothing -> throw e
      Right _ -> assertFailure "Expected todo exception, got result :|"
