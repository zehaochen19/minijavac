--import ParserSpec
import Test.Hspec
import TypeCheckSpec

main :: IO ()
main = hspec $ do typeCheckSpecs
