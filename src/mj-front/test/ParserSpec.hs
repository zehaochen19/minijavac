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

expressionPSpec :: Spec
expressionPSpec =
  describe "Expression parser should" $ do
    it "parse booleans" $ do
      parse expressionP "" "true" `shouldBe` Right ETrue
      parse expressionP "" "false" `shouldBe` Right EFalse
    it "parse arithmetic expressions" $ do
      parse expressionP "" "1+1" `shouldBe`
        (Right $ EBinary BPlus (EInt 1) (EInt 1))
      parse expressionP "" "1 -1" `shouldBe`
        (Right $ EBinary BMinus (EInt 1) (EInt 1))
      parse expressionP "" "1 * 3" `shouldBe`
        (Right $ EBinary BMult (EInt 1) (EInt 3))
      parse expressionP "" "1239*     3   " `shouldBe`
        (Right $ EBinary BMult (EInt 1239) (EInt 3))
    it "parse a new object" $
      parse expressionP "" "new SomeClass()" `shouldBe`
      (Right $ ENewObj (Identifier "SomeClass"))
    it "parse array indexing" $
      parse expressionP "" "arr[3]" `shouldBe`
      (Right $ EArrayIndex (EId (Identifier "arr")) (EInt 3))
    it "parse a method application" $
      parse expressionP "" "obj.func(1, foo)" `shouldBe`
      (Right $
       EMethodApp
         (EId $ Identifier "obj")
         (Identifier "func")
         [EInt 1, EId $ Identifier "foo"])
    it "parse a new array" $
      parse expressionP "" "new int[3+4]" `shouldBe`
      (Right $ ENewIntArr (EBinary BPlus (EInt 3) (EInt 4)))
    it "parse a not-expression" $
      parse expressionP "" "!true" `shouldBe` (Right $ ENot ETrue)
    it "parse a length of array" $
      parse expressionP "" "arr.length" `shouldBe`
      (Right $ EArrayLength (EId (Identifier "arr")))
    it "parse a expression inside parentheses" $
      parse expressionP "" "!((3 + 4) * 2 < (5 + count))" `shouldBe`
      (Right $
       ENot
         (EParen
            (EBinary
               BLT
               (EBinary
                  BMult
                  (EParen (EBinary BPlus (EInt 3) (EInt 4)))
                  (EInt 2))
               (EParen (EBinary BPlus (EInt 5) (EId (Identifier "count")))))))
