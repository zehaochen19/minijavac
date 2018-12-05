import           ParserSpec
import           Test.Hspec
import           TypeCheckSpec
import           JSONSpec

main :: IO ()
main = hspec $ do
  parserSpecs
  typeCheckSpecs
  jsonSpec
