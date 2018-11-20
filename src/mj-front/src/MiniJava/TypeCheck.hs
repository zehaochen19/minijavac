module MiniJava.TypeCheck where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified MiniJava.Symbol as S
import MiniJava.TypeCheck.Type

checkMiniJava :: S.MiniJavaAST -> State SymbolTable CheckResult
checkMiniJava ast = do
  initSymbolTable ast
  return Ok

initSymbolTable :: S.MiniJavaAST -> State SymbolTable ()
initSymbolTable ast = do
  put
    SymbolTable
      { _methods = M.empty
      , _classes = initClasses ast
      , _vars = M.empty
      , _curClass = Nothing
      , _curMethod = Nothing
      , _errors = V.empty
      }
  return ()

initClasses :: S.MiniJavaAST -> ClassTable
initClasses ast = M.fromList pairs
  where
    cls = ast ^. S.classes
    pairs = map (\c -> (c ^. S.className, c)) cls
