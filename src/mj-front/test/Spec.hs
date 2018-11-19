import ParserSpec
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    identifierPSpec
    expressionPSpec
    varDecPSpec
    statementPSpec
    methodDecPSpec
    mainClassDecPSpec
    classDecPSpec
    miniJavaPSpec
