module ParserSpec where

import Data.Either
import MiniJava.Parser
import MiniJava.Symbol
import Test.Hspec
import Text.Megaparsec

identifierPSpec :: Spec
identifierPSpec =
  describe "Identifier parser should" $ do
    it "parse variables" $
      parse identifierP "" "x123" `shouldBe` (Right $ Identifier "x123")
    it "not parse identifiers started with number" $
      parse identifierP "" "123x" `shouldSatisfy` isLeft
    it "parse class name" $
      parse identifierP "" "BinaryTree" `shouldBe`
      (Right $ Identifier "BinaryTree")
    it "not parse reserved word" $ do
      parse identifierP "" "class" `shouldSatisfy` isLeft
      parse identifierP "" "int" `shouldSatisfy` isLeft
    it "parse a identifier started with underscore" $
      parse identifierP "" "_func__" `shouldBe` (Right $ Identifier "_func__")
