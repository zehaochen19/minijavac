{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Data.Either
import qualified Data.Text.IO as TIO
import MiniJava.Parser
import MiniJava.Symbol
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ

parserSpecs :: Spec
parserSpecs = do
  identifierPSpec
  expressionPSpec
  varDecPSpec
  statementPSpec
  methodDecPSpec
  mainClassDecPSpec
  classDecPSpec
  miniJavaPSpec

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

statementPSpec :: Spec
statementPSpec =
  describe "Statement parser should parse" $ do
    it "a print statement" $
      parse statementP "" "System.out.println(1 + 2);" `shouldBe`
      (Right $ SPrint (EBinary BPlus (EInt 1) (EInt 2)))
    it "a if assignment" $
      parse statementP "" "a = 4;" `shouldBe`
      (Right $ SAssignId (Identifier "a") (EInt 4))
    it "an array assignment" $
      parse statementP "" "arr[4] = 1 + 2;" `shouldBe`
      (Right $
       SAssignArr (Identifier "arr") (EInt 4) (EBinary BPlus (EInt 1) (EInt 2)))
    it "an while statement" $
      parse statementP "" "while (a < 10) { a = a - 1; }" `shouldBe`
      (Right $
       SWhile
         (EBinary BLT (EId (Identifier "a")) (EInt 10))
         (SBlock
            [ SAssignId
                (Identifier "a")
                (EBinary BMinus (EId (Identifier "a")) (EInt 1))
            ]))
    it "an if statement" $
      parse statementP "" "if (x) { a = 1; } else { b = 2; }" `shouldBe`
      (Right $
       SIf
         (EId $ Identifier "x")
         (SBlock [SAssignId (Identifier "a") (EInt 1)])
         (SBlock [SAssignId (Identifier "b") (EInt 2)]))

varDecPSpec :: Spec
varDecPSpec =
  describe "VarDec parser should parse" $ do
    it "a int declaration" $
      parse varDecP "" "int a;" `shouldBe`
      (Right $ VarDec TInt (Identifier "a"))
    it "a boolean declaration" $
      parse varDecP "" "boolean flag;" `shouldBe`
      (Right $ VarDec TBool (Identifier "flag"))
    it "a int array declaration" $
      parse varDecP "" "int[] arr;" `shouldBe`
      (Right $ VarDec TIntArray (Identifier "arr"))

methodDecPSpec :: Spec
methodDecPSpec =
  describe "MethodDec parser should parse" $ do
    it "a method declaration" $
      parse
        (sc >> methodDecP)
        ""
        [r|
              public int func(int a) {
                int b;
                b = a + 1;
                return b;
              }
                    |] `shouldBe`
      (Right $
       MethodDec
         TInt
         (Identifier "func")
         [(TInt, Identifier "a")]
         [VarDec TInt (Identifier "b")]
         [ SAssignId
             (Identifier "b")
             (EBinary BPlus (EId $ Identifier "a") (EInt 1))
         ]
         (EId $ Identifier "b"))
    it "another method declaration" $
      parse
        (sc >> methodDecP)
        ""
        "public int setValue(int a) { value = a; return value; }" `shouldBe`
      (Right $
       MethodDec
         TInt
         (Identifier "setValue")
         [(TInt, Identifier "a")]
         []
         [SAssignId (Identifier "value") (EId $ Identifier "a")]
         (EId $ Identifier "value"))
    it "a method containing a single return" $
      parse methodDecP "" "public int getValue() { return value; }" `shouldBe`
      (Right $
       MethodDec
         TInt
         (Identifier "getValue")
         []
         []
         []
         (EId $ Identifier "value"))

mainClassDecPSpec :: Spec
mainClassDecPSpec =
  describe "Main class parser should parse" $ do
    it "a main class declaration" $
      parse
        (sc >> mainClassDecP)
        ""
        [r|
      class Main {
        public static void main(String [] args) {
          System.out.println(1);
        }
      }
      |] `shouldBe`
      (Right $
       MainClass (Identifier "Main") (Identifier "args") (SPrint (EInt 1)))
    it "another main class declaration" $
      parse
        (sc >> mainClassDecP)
        ""
        [r|
          class QuickSort{
            public static void main(String[] a){
	      System.out.println(new QS().Start(10));
            }
          }|] `shouldBe`
      (Right $
       MainClass
         (Identifier "QuickSort")
         (Identifier "a")
         (SPrint
            (EMethodApp
               (ENewObj $ Identifier "QS")
               (Identifier "Start")
               [EInt 10])))

classDecPSpec :: Spec
classDecPSpec =
  describe "Class declaration parser should parse" $
  it "a class declaration" $
  parse
    (sc >> classDecP)
    ""
    [r|
        class Node extends AbstractNode {
          int value;
          boolean flag;

          public int getValue() { return value; }
          public int setValue(int a) { value = a; return value; }
        }
        |] `shouldBe`
  (Right $
   ClassDec
     (Identifier "Node")
     (Just $ Identifier "AbstractNode")
     [VarDec TInt $ Identifier "value", VarDec TBool $ Identifier "flag"]
     [ MethodDec
         TInt
         (Identifier "getValue")
         []
         []
         []
         (EId $ Identifier "value")
     , MethodDec
         TInt
         (Identifier "setValue")
         [(TInt, Identifier "a")]
         []
         [SAssignId (Identifier "value") (EId $ Identifier "a")]
         (EId $ Identifier "value")
     ])

testWithSrc :: FilePath -> IO ()
testWithSrc srcPath = do
  program <- TIO.readFile srcPath
  let result = parse miniJavaP srcPath program
  print result
  result `shouldSatisfy` isRight

miniJavaPSpec :: Spec
miniJavaPSpec =
  describe "MiniJava parser should parse" $ do
    it "a LinkedList program" $ testWithSrc "test/cases/LinkedList.java"
    it "a BinaryTree program" $ testWithSrc "test/cases/BinaryTree.java"
