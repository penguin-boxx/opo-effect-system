import SpecBlock1 qualified
import SpecBlock2 qualified
import SpecBlock3 qualified
import Test.Prelude

main :: IO ()
main = testMain $ SpecBlock1.tests ++ SpecBlock2.tests ++ SpecBlock3.tests
