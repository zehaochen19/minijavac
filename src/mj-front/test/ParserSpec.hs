{-# LANGUAGE OverloadedStrings #-}


module ParserSpec where

import           Data.Either
import           MiniJava.Parser
import           MiniJava.Symbol
import           Test.Hspec
import           Text.Megaparsec
import           Data.Text                     as T

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

defaultPos = SourcePos "" (mkPos 1) (mkPos 1)
mkSrcPos l c = SourcePos "" (mkPos l) (mkPos c)

identifierPSpec :: Spec
identifierPSpec = describe "Identifier parser should" $ do
  it "parse variables"
    $          parse identifierP "" "x123"
    `shouldBe` (Right $ Identifier "x123")
  it "not parse identifiers started with number"
    $               parse identifierP "" "123x"
    `shouldSatisfy` isLeft
  it "parse class name"
    $          parse identifierP "" "BinaryTree"
    `shouldBe` (Right $ Identifier "BinaryTree")
  it "not parse reserved word" $ do
    parse identifierP "" "class" `shouldSatisfy` isLeft
    parse identifierP "" "int" `shouldSatisfy` isLeft
  it "parse a identifier started with underscore"
    $          parse identifierP "" "_func__"
    `shouldBe` (Right $ Identifier "_func__")

expressionPSpec :: Spec
expressionPSpec = describe "Expression parser should" $ do
  it "parse booleans" $ do
    parse expressionP "" "true" `shouldBe` (Right $ ETrue (mkSrcPos 1 1))
    parse expressionP "" "false" `shouldBe` (Right $ EFalse defaultPos)
  it "parse arithmetic expressions" $ do
    parse expressionP "" "1+1"
      `shouldBe` (Right $ EBinary (mkSrcPos 1 2)
                                  BPlus
                                  (EInt (mkSrcPos 1 1) 1)
                                  (EInt (mkSrcPos 1 3) 1)
                 )
    parse expressionP "" "1 -1"
      `shouldBe` (Right $ EBinary (mkSrcPos 1 3)
                                  BMinus
                                  (EInt (mkSrcPos 1 1) 1)
                                  (EInt (mkSrcPos 1 4) 1)
                 )
    parse expressionP "" "1 * 3"
      `shouldBe` (Right $ EBinary (mkSrcPos 1 3)
                                  BMult
                                  (EInt (mkSrcPos 1 1) 1)
                                  (EInt (mkSrcPos 1 5) 3)
                 )
    parse expressionP "" "1239*     3   "
      `shouldBe` (Right $ EBinary (mkSrcPos 1 5)
                                  BMult
                                  (EInt (mkSrcPos 1 1) 1239)
                                  (EInt (mkSrcPos 1 11) 3)
                 )
  it "parse a new object"
    $          parse expressionP "" "new SomeClass()"
    `shouldBe` (Right $ ENewObj defaultPos (Identifier "SomeClass"))
  it "parse array indexing"
    $          parse expressionP "" "arr[3]"
    `shouldBe` (Right $ EArrayIndex (mkSrcPos 1 4)
                                    (EId defaultPos (Identifier "arr"))
                                    (EInt (mkSrcPos 1 5) 3)
               )
  it "parse a method application"
    $          parse expressionP "" "obj.func(1, foo)"
    `shouldBe` (Right $ EMethodApp
                 (mkSrcPos 1 4)
                 (EId defaultPos (Identifier "obj"))
                 (Identifier "func")
                 [ EInt (mkSrcPos 1 10) 1
                 , EId (mkSrcPos 1 13) (Identifier "foo")
                 ]
               )
  it "parse a new array"
    $          parse expressionP "" "new int[3+4]"
    `shouldBe` (Right $ ENewIntArr
                 defaultPos
                 (EBinary (mkSrcPos 1 10)
                          BPlus
                          (EInt (mkSrcPos 1 9) 3)
                          (EInt (mkSrcPos 1 11) 4)
                 )
               )
  it "parse a not-expression"
    $          parse expressionP "" "!true"
    `shouldBe` (Right $ ENot defaultPos (ETrue $ mkSrcPos 1 2))
  it "parse a length of array"
    $          parse expressionP "" "arr.length"
    `shouldBe` (Right $ EArrayLength (mkSrcPos 1 4)
                                     (EId defaultPos (Identifier "arr"))
               )
  it "parse a expression inside parentheses"
    $               parse expressionP "" "!((3 + 4) * 2 < (5 + count))"
    `shouldSatisfy` isRight

statementPSpec :: Spec
statementPSpec = describe "Statement parser should parse" $ do
  it "a print statement"
    $               parse statementP "" "System.out.println(1 + 2);"
    `shouldSatisfy` isRight
  it "a if assignment" $ parse statementP "" "a = 4;" `shouldSatisfy` isRight
  it "an array assignment"
    $               parse statementP "" "arr[4] = 1 + 2;"
    `shouldSatisfy` isRight
  it "an while statement"
    $               parse statementP "" "while (a < 10) { a = a - 1; }"
    `shouldSatisfy` isRight
  it "an if statement"
    $               parse statementP "" "if (x) { a = 1; } else { b = 2; }"
    `shouldSatisfy` isRight

varDecPSpec :: Spec
varDecPSpec = describe "VarDec parser should parse" $ do
  it "a int declaration"
    $          parse varDecP "" "int a;"
    `shouldBe` (Right $ VarDec defaultPos TInt (Identifier "a"))
  it "a boolean declaration"
    $          parse varDecP "" "boolean flag;"
    `shouldBe` (Right $ VarDec defaultPos TBool (Identifier "flag"))
  it "a int array declaration"
    $          parse varDecP "" "int[] arr;"
    `shouldBe` (Right $ VarDec defaultPos TIntArray (Identifier "arr"))

methodDecPSpec :: Spec
methodDecPSpec = describe "MethodDec parser should parse" $ do
  it "a method declaration"
    $               parse
                      (sc >> methodDecP)
                      ""
                      (  T.pack
                      $  "public int func(int a) {"
                      ++ "int b;"
                      ++ "b = a + 1;"
                      ++ "return b;"
                      ++ "}"
                      )
    `shouldSatisfy` isRight
  it "another method declaration"
    $          parse (sc >> methodDecP)
                     ""
                     "public int setValue(int a) { value = a; return value; }"
    `shouldBe` (Right $ MethodDec
                 defaultPos
                 TInt
                 (Identifier "setValue")
                 [(TInt, Identifier "a")]
                 []
                 [ SAssignId (mkSrcPos 1 30)
                             (Identifier "value")
                             (EId (mkSrcPos 1 38) (Identifier "a"))
                 ]
                 (EId (mkSrcPos 1 48) (Identifier "value"))
               )
  it "a method containing a single return"
    $          parse methodDecP "" "public int getValue() { return value; }"
    `shouldBe` (Right $ MethodDec defaultPos
                                  TInt
                                  (Identifier "getValue")
                                  []
                                  []
                                  []
                                  (EId (mkSrcPos 1 32) (Identifier "value"))
               )

mainClassDecPSpec :: Spec
mainClassDecPSpec = describe "Main class parser should parse" $ do
  it "a main class declaration"
    $          parse
                 (sc >> mainClassDecP)
                 ""
                 (  T.pack
                 $  "class Main {"
                 ++ "public static void main(String [] args) {"
                 ++ "System.out.println(1);"
                 ++ "}"
                 ++ "}"
                 )
    `shouldBe` (Right $ MainClass
                 (Identifier "Main")
                 (Identifier "args")
                 (SPrint (mkSrcPos 1 54) (EInt (mkSrcPos 1 73) 1))
               )
  it "another main class declaration"
    $          parse
                 (sc >> mainClassDecP)
                 ""
                 (  T.pack
                 $  "class QuickSort{"
                 ++ "public static void main(String[] a){"
                 ++ "System.out.println(new QS().Start(10));"
                 ++ "}"
                 ++ "}"
                 )
    `shouldBe` (Right $ MainClass
                 (Identifier "QuickSort")
                 (Identifier "a")
                 (SPrint
                   (mkSrcPos 1 53)
                   (EMethodApp (mkSrcPos 1 80)
                               (ENewObj (mkSrcPos 1 72) (Identifier "QS"))
                               (Identifier "Start")
                               [EInt (mkSrcPos 1 87) 10]
                   )
                 )
               )

classDecPSpec :: Spec
classDecPSpec =
  describe "Class declaration parser should parse"
    $          it "a class declaration"
    $          parse
                 (sc >> classDecP)
                 ""
                 (  T.pack
                 $  "class Node extends AbstractNode {"
                 ++ "int value;"
                 ++ "boolean flag;"
                 ++ "public int getValue() { return value; }"
                 ++ "public int setValue(int a) { value = a; return value; }"
                 ++ "}"
                 )
    `shouldBe` (Right $ ClassDec
                 (Identifier "Node")
                 (Just $ Identifier "AbstractNode")
                 [ VarDec (mkSrcPos 1 34) TInt  (Identifier "value")
                 , VarDec (mkSrcPos 1 44) TBool (Identifier "flag")
                 ]
                 [ MethodDec (mkSrcPos 1 57)
                             TInt
                             (Identifier "getValue")
                             []
                             []
                             []
                             (EId (mkSrcPos 1 88) (Identifier "value"))
                 , MethodDec
                   (mkSrcPos 1 96)
                   TInt
                   (Identifier "setValue")
                   [(TInt, Identifier "a")]
                   []
                   [ SAssignId (mkSrcPos 1 125)
                               (Identifier "value")
                               (EId (mkSrcPos 1 133) (Identifier "a"))
                   ]
                   (EId (mkSrcPos 1 143) (Identifier "value"))
                 ]
               )

miniJavaPSpec :: Spec
miniJavaPSpec = describe "MiniJava parser should parse" $ do
  it "a LinkedList program" $ testWithSrc "test/cases/LinkedList.java"
  it "a BinaryTree program" $ testWithSrc "test/cases/BinaryTree.java"
 where
  testWithSrc :: FilePath -> IO ()
  testWithSrc srcPath = do
    result <- parseFromSrc srcPath
    -- print result
    result `shouldSatisfy` isRight
