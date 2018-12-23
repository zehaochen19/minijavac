module JSONSpec where


import           MiniJava.JSON                 as J
import           MiniJava.Config
import           Test.Hspec
import qualified Data.ByteString.Lazy          as BS

jsonSpec :: Spec
jsonSpec = describe "compileToJSON" $ do
  it "should compile a LinkedList program" $ do
    json <- J.compileToJSON "test/cases/LinkedList.java" $ Config False
    BS.writeFile "LinkedList.json" json
    --print json
    json `shouldSatisfy` (\res -> BS.length res > 0)
  it "should compile a BinaryTree program" $ do
    json <- J.compileToJSON "test/cases/BinaryTree.java" $ Config False
    BS.writeFile "BinaryTree.json" json
    --print json
    json `shouldSatisfy` (\res -> BS.length res > 0)
