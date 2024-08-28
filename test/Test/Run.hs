module Test.Run (NamedTests, testMain, nameTests) where

import Control.Monad (forM)
import Data.List qualified as List
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (..), Counts (..))
import Test.HUnit.Base qualified as HU

type NamedTests = [(String, Test)]
data TestStatus = TestPassed | TestPartial Float | TestFailed deriving Eq
data TestResult = TestResult { testName :: String, testStatus :: TestStatus }
type TestReport = [TestResult]

instance Semigroup Counts where
  c <> c' = Counts
    { errors = errors c + errors c'
    , failures = failures c + failures c'
    , tried = tried c + tried c'
    , cases = cases c + cases c'
    }

instance Monoid Counts where
  mempty = Counts 0 0 0 0

testMain :: NamedTests -> IO ()
testMain tests = do
  testFilters <- getTestFilters
  putStrLn $ "Executing: " <>
    if null testFilters then "all tests" else List.intercalate ", " testFilters
  (counts, report) <- runTests $ filterTests testFilters tests
  lookupEnv "HASKELL_TEST_REPORT" >>= maybe (pure ()) \filePath ->
    appendFile filePath $ showMachine report
  putStrLn $ "\n" <> showCounts counts <> "\n"
  if statusFromCounts counts == TestPassed then exitSuccess else exitFailure
  where
    getTestFilters :: IO [String]
    getTestFilters = concatMap words <$> getArgs

    filterTests :: [String] -> NamedTests -> NamedTests
    filterTests names = filter \(name, _) -> null names || name `elem `names

nameTests :: Int -> [Test] -> NamedTests
nameTests iBlock = map wrapToTestLabel . zipWith mkNamedTest [1 :: Int ..]
  where
    wrapToTestLabel (label, test) = (label, TestLabel label test)
    mkNamedTest iTask test = (show iBlock <> "." <> show iTask, test)

statusFromCounts :: Counts -> TestStatus
statusFromCounts Counts {..}
  | errors == 0 && failures == 0 = TestPassed
  | errors + failures < tried = TestPartial $
      1 - fromIntegral (errors + failures) / fromIntegral tried
  | otherwise = TestFailed

showCounts :: Counts -> String
showCounts Counts {..} = concat
  [ "Cases: ", show cases
  , "  Tried: ", show tried
  , "  Errors: ", show errors
  , "  Failures: ", show failures
  ]

showMachine :: TestReport -> String
showMachine = List.intercalate "\n" . map showResult
  where
    showResult TestResult {..} = testName <> "=" <> case testStatus of
      TestPassed -> "DONE"
      TestPartial percent -> "PARTIAL " <> show percent
      TestFailed -> "FAILED"

runTests :: NamedTests -> IO (Counts, TestReport)
runTests tests = collectReport <$> forM tests \(name, test) -> do
  putStrLn $ "Running test \"" <> name <> "\":"
  (counts, _) <- HU.performTest reportStart reportError reportFailure () test
  let result = TestResult { testName = name, testStatus = statusFromCounts counts }
  reportSummary counts
  pure (counts, result)
  where
    reportStart _ _ = pure () -- per test case
    reportError loc msg =
      -- Hack to distinguish not implemented cases
      let isTodo = "Not implemented" `List.isSubsequenceOf` msg in
      let prefix = if isTodo then "[TODO] " else "[ERROR] " in
      reportProblem prefix loc msg
    reportFailure = reportProblem "[FAILURE] "
    reportProblem prefix _ msg HU.State{..} () = putStr $ padLines 4 $
      prefix <> showPath path <> " " <> msg <> if '\n' `elem` msg then "\n" else ""
    reportSummary counts = putStr $ padLines 4 $ case statusFromCounts counts of
      TestPassed -> "Done :)"
      TestPartial percent -> "In progress, " <> show (floor $ percent * 100) <> "% tests remain :|"
      TestFailed -> "Nothing here :("

padLines :: Int -> String -> String
padLines nSpaces = unlines . map (replicate nSpaces ' ' ++) . lines

collectReport :: [(Counts, TestResult)] -> (Counts, TestReport)
collectReport = sequenceA

showPath :: HU.Path -> String
showPath = List.intercalate ":" . reverse . map \case
  HU.ListItem n -> show n
  HU.Label label -> label
