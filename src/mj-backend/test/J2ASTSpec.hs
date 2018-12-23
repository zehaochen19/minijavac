module J2ASTSpec where


import           MiniJava.Json2AST                 as J
import           Test.Hspec
import qualified Data.ByteString.Lazy          as BS


jsonSpec :: Spec
jsonSpec = describe "recover from json" $ do
  it "should recover a LinkedList program" $ do
    ast <- J.astFromSrc "test/cases/LinkedList.json"
    print ast
    
