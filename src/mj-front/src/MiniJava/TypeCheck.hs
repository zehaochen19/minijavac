module MiniJava.TypeCheck where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import qualified MiniJava.Symbol as S
import MiniJava.TypeCheck.Type
import MiniJava.TypeCheck.Util

checkMiniJava :: Monad m => S.MiniJavaAST -> TC m CheckResult
checkMiniJava ast = do
  initSymbolTable ast
  return $ Right ()

initSymbolTable :: Monad m => S.MiniJavaAST -> TC m ()
initSymbolTable ast = do
  put
    SymbolTable
      { _methods = M.empty
      , _classes = collectClasses ast
      , _vars = M.empty
      , _curClass = Nothing
      , _curMethod = Nothing
      , _errors = []
      }
  return ()

-- Collect information of global classes
collectClasses :: S.MiniJavaAST -> ClassTable
collectClasses ast = M.fromList pairs
  where
    cls = ast ^. S.classes
    pairs = map (\c -> (c ^. S.className, c)) cls

checkMain :: Monad m => S.MainClass -> TC m ()
checkMain = undefined

checkClass :: Monad m => S.ClassDec -> TC m ()
checkClass classDec = do
  initClassScope classDec
  mapM_ checkMethod $ classDec ^. S.methods

checkMethod = undefined

-- Collect information of class variables and methods
initClassScope :: Monad m => S.ClassDec -> TC m ()
initClassScope classDec = do
  curClass .= Just (classDec ^. S.className)
  vars .= M.fromList varDecs
  methods .= M.fromList metDecs
  return ()
  where
    varDecs =
      fmap (\v -> (v ^. S.varId, v ^. S.varType)) (classDec ^. S.classVars)
    metDecs = fmap (\m -> (m ^. S.methodId, m)) (classDec ^. S.methods)

checkStatement :: Monad m => S.Statement -> TC m ()
checkStatement (S.SBlock stmts) = mapM_ checkStatement stmts
checkStatement (S.SIf pred trueBranch falseBranch) = do
  predType <- checkExpr pred
  _ <-
    case predType of
      S.TBool -> return ()
      S.TBottom -> return () -- errros in subexpression
      ty ->
        addError $
        T.pack $
        "Predicate expression: " ++
        show pred ++ "\nExpected type: TBool" ++ "\nBut has: " ++ show ty
  checkStatement trueBranch
  checkStatement falseBranch
  return ()

checkExpr :: Monad m => S.Expression -> TC m S.Type
checkExpr = undefined
