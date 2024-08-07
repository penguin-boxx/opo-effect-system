module Test.HUnit.TextAdvanced where

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (void, unless)
import Test.HUnit.Base
import System.Environment (lookupEnv)

type NamedTests = [(String, Test)]
data TestResult = Fulfilled | PartialDone Float | ZeroDone
  deriving (Show, Eq)
type TestSummary = (String, TestResult)
type TestReport = [TestSummary]

padStartLines :: String -> String -> String
padStartLines pad = unlines .  map (pad <>) . lines

padToCell :: String -> String
padToCell = go 20
  where
    go n []
      | n <= 0 = ""
      | otherwise = replicate n ' '
    go n (x:xs)
      | n <= 0 = ""
      | otherwise = x : go (n - 1) xs

showPath :: Path -> String
showPath [] = ""
showPath nodes = foldl1 f (map showNode nodes)
  where
    f b a = a ++ ":" ++ b
    showNode (ListItem n) = show n
    showNode (Label label) = safe label (show label)
    safe s ss = if ':' `elem` s || "\"" ++ s ++ "\"" /= ss then ss else s

instance Semigroup Counts where
  c <> c' = Counts 
    { errors = errors c + errors c'
    , failures = failures c + failures c'
    , tried = tried c + tried c'
    , cases = cases c + cases c'
    }

instance Monoid Counts where
  mempty = Counts 0 0 0 0

collectTestResult :: [(Counts, TestSummary)] -> (Counts, [TestSummary])
collectTestResult = sequenceA

testResultByCounts :: Counts -> TestResult
testResultByCounts (Counts {..})
  | (errors == 0) && (failures == 0) = Fulfilled
  | errors + failures < tried = PartialDone $ 
      fromIntegral (errors + failures) / fromIntegral tried
  | otherwise = ZeroDone

isSuccessByCounts :: Counts -> Bool
isSuccessByCounts = (== Fulfilled) . testResultByCounts

showCounts :: Counts -> String
showCounts Counts{ cases = cases', tried = tried',
                   errors = errors', failures = failures' } =
  "Cases: " ++ show cases' ++ "  Tried: " ++ show tried' ++
  "  Errors: " ++ show errors' ++ "  Failures: " ++ show failures'

putSummaryMark :: TestSummary -> IO ()
putSummaryMark (name, report) = putStrLn $ "    TEST \"" <> name <> "\" " <> mkMark report
  where
    mkMark Fulfilled = "DONE ✅"
    mkMark (PartialDone p) = "HAS FAILURES 🤔 " <> show p <> "%"
    mkMark ZeroDone = "FAILED ⛔"

runTestMR :: (String, Test) -> IO (Counts, TestSummary)
runTestMR (name, test) = do
    putStrLn $ "RUNNING TEST \"" <> name <> "\":"
    (counts', _) <- performTest reportStart reportError reportFailure () test
    let summary = (name, testResultByCounts counts')
    putSummaryMark summary
    pure (counts', summary)
  where
    reportStart _ _ = pure ()
    reportError = reportProblem "[ERROR] "
    reportFailure = reportProblem "[FAILURE] "
    reportProblem tp _ msg ss _ = putStrLn $ padStartLines "    " $
      tp <> showPath (path ss) 
      <> "\n= DIAGNOSTICS ===>\n" 
      <> msg <>
      "\n<==="

showTestReportToHuman :: TestReport -> String
showTestReportToHuman [] = ""
showTestReportToHuman ((name, result):rest) = 
  name <> " = " <> mkTextStatus result <> "\n" <> showTestReportToHuman rest
  where
    mkTextStatus Fulfilled = "✅"
    mkTextStatus (PartialDone _) = "🤔"
    mkTextStatus ZeroDone = "⛔"

showTestReportToMachine :: TestReport -> String
showTestReportToMachine [] = ""
showTestReportToMachine ((name, result):rest) =
  padToCell name <> "= " <> mkTextStatus result <> "\n" 
  <> showTestReportToMachine rest
  where
    mkTextStatus Fulfilled = "DONE"
    mkTextStatus (PartialDone p) = "PARTIAL " <> show p
    mkTextStatus ZeroDone = "FAILED"

exportReportIsSpecified :: TestReport -> IO ()
exportReportIsSpecified report = do
  mv <- lookupEnv "HASKELL_TEST_REPORT"
  case mv of
    Nothing -> pure ()
    Just file -> writeFile file (showTestReportToMachine report)


runTestMRAndExit :: NamedTests -> IO ()
runTestMRAndExit tests = do
  results <- traverse runTestMR tests
  let (counts', report) = collectTestResult results
  let isSuccess = isSuccessByCounts counts'
  putStrLn "\n===== TOTAL =====\n"
  unless isSuccess $ putStrLn $ showTestReportToHuman report
  putStrLn $ showCounts counts'
  exportReportIsSpecified report
  if isSuccess
    then exitSuccess
    else exitFailure
