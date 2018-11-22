module TypeCheckSpec where

import Control.Lens
import Control.Monad.State

import qualified Data.Map as M
import MiniJava.Symbol as S
import MiniJava.TypeCheck
import MiniJava.TypeCheck.Type as TC
import Test.Hspec

typeCheckSpecs :: Spec
typeCheckSpecs = checkVariableSpec

nodeInfo :: ClassInfo
nodeInfo =
  ClassInfo
    M.empty
    (Just $ Identifier "AbstractNode")
    (M.fromList [(Identifier "getValue", MethodInfo TInt [])])

abstractNodeInfo :: ClassInfo
abstractNodeInfo =
  ClassInfo (M.fromList [(Identifier "value", TInt)]) Nothing M.empty

checkVariableSpec :: Spec
checkVariableSpec =
  describe "checkExpressionSpec should" $ do
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
