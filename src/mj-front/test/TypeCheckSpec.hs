module TypeCheckSpec where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import MiniJava.Symbol as S
import MiniJava.TypeCheck
import MiniJava.TypeCheck.Type
import Test.Hspec

typeCheckSpecs :: Spec
typeCheckSpecs = checkExpressionSpec

checkExpressionSpec :: Spec
checkExpressionSpec =
  describe "checkExpressionSpec should" $
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
