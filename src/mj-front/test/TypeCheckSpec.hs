module TypeCheckSpec where

import Control.Lens
import Control.Monad.State

import qualified Data.Map as M
import MiniJava.Symbol as S
import MiniJava.TypeCheck
import MiniJava.TypeCheck.Type as TC
import Test.Hspec

typeCheckSpecs :: Spec
typeCheckSpecs = do
  checkExprSpec

nodeInfo :: ClassInfo
nodeInfo =
  ClassInfo
    M.empty
    (Just $ Identifier "AbstractNode")
    (M.fromList
       [ (Identifier "getValue", MethodInfo TInt [])
       , (Identifier "setValue", MethodInfo TInt [TInt])
       ])

abstractNodeInfo :: ClassInfo
abstractNodeInfo =
  ClassInfo (M.fromList [(Identifier "value", TInt)]) Nothing M.empty

anotherNodeInfo :: ClassInfo
anotherNodeInfo = ClassInfo M.empty (Just $ Identifier "Node") M.empty

checkVariableSpec :: Spec
checkVariableSpec =
  describe "checkExprSpec should" $ do
    it "check a int variable in method scope" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (EId $ Identifier "num"))
              (SymbolTable
                 M.empty
                 M.empty
                 (M.fromList [(Identifier "num", TInt)])
                 Nothing
                 Nothing
                 [])
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` TInt
    it "complain when type of the variable cannot be found" $ do
      let (ty, symbolTable) =
            runState (checkExpr (EId $ Identifier "num")) emptySymbolTable
      symbolTable ^. errors `shouldSatisfy` not . null
      ty `shouldBe` TBottom
    it "should check a variable declared in superclass" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (EId $ Identifier "value"))
              (emptySymbolTable
                 { _curClass = Just $ Identifier "Node"
                 , _curMethod = Just $ Identifier "getValue"
                 , TC._classes =
                     M.fromList
                       [ (Identifier "Node", nodeInfo)
                       , (Identifier "AbstractNode", abstractNodeInfo)
                       ]
                 })
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` TInt
    it "should check a variable declare in superclass of superclass" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (EId $ Identifier "value"))
              (emptySymbolTable
                 { _curClass = Just $ Identifier "AnotherNode"
                 , _curMethod = Just $ Identifier "getValue"
                 , TC._classes =
                     M.fromList
                       [ (Identifier "Node", nodeInfo)
                       , (Identifier "AbstractNode", abstractNodeInfo)
                       , (Identifier "AnotherNode", anotherNodeInfo)
                       ]
                 })
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` TInt

checkExprSpec :: Spec
checkExprSpec = do
  checkVariableSpec
  describe "checkExpr should" $ do
    it "check this referencd" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr EThis)
              emptySymbolTable {_curClass = Just $ Identifier "Node"}
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` (TClass $ Identifier "Node")
    it "check a plus expression" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (EBinary BPlus (EInt 10) (EInt 20)))
              emptySymbolTable
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` TInt
    it "check a less than expression" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (EBinary BLT (EInt 1) (EInt 2)))
              emptySymbolTable
      symbolTable ^. errors `shouldBe` []
      ty `shouldBe` TBool
    it "check a new Node object" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr (ENewObj $ Identifier "Node"))
              (emptySymbolTable
                 { _curClass = Just $ Identifier "AnotherNode"
                 , _curMethod = Just $ Identifier "getValue"
                 , TC._classes =
                     M.fromList
                       [ (Identifier "Node", nodeInfo)
                       , (Identifier "AbstractNode", abstractNodeInfo)
                       , (Identifier "AnotherNode", anotherNodeInfo)
                       ]
                 })
      ty `shouldBe` (TClass $ Identifier "Node")
      symbolTable ^. errors `shouldBe` []
    it "check a new int array" $ do
      let (ty, symbolTable) =
            runState (checkExpr (ENewIntArr $ EInt 42)) emptySymbolTable
      ty `shouldBe` TIntArray
      symbolTable ^. errors `shouldBe` []
    it "check a int array length expression" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr $ EArrayLength (ENewIntArr $ EInt 42))
              emptySymbolTable
      ty `shouldBe` TInt
      symbolTable ^. errors `shouldBe` []
    it "check a array indexing expression" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr $ EArrayIndex (ENewIntArr $ EInt 42) (EInt 0))
              emptySymbolTable
      ty `shouldBe` TInt
      symbolTable ^. errors `shouldBe` []
    it "check a method application" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr $
               EMethodApp
                 (EId $ Identifier "node")
                 (Identifier "setValue")
                 [EInt 10])
              emptySymbolTable
                { TC._classes =
                    M.fromList
                      [ (Identifier "Node", nodeInfo)
                      , (Identifier "AbstractNode", abstractNodeInfo)
                      , (Identifier "AnotherNode", anotherNodeInfo)
                      ]
                , _vars =
                    M.fromList [(Identifier "node", TClass $ Identifier "Node")]
                }
      ty `shouldBe` TInt
      symbolTable ^. errors `shouldBe` []
    it "check a inherited method application" $ do
      let (ty, symbolTable) =
            runState
              (checkExpr $
               EMethodApp
                 (EId $ Identifier "node")
                 (Identifier "setValue")
                 [EInt 10])
              emptySymbolTable
                { TC._classes =
                    M.fromList
                      [ (Identifier "Node", nodeInfo)
                      , (Identifier "AbstractNode", abstractNodeInfo)
                      , (Identifier "AnotherNode", anotherNodeInfo)
                      ]
                , _vars =
                    M.fromList
                      [(Identifier "node", TClass $ Identifier "AnotherNode")]
                }
      ty `shouldBe` TInt
      symbolTable ^. errors `shouldBe` []
