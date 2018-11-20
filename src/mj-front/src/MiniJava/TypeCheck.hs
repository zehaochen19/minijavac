{-# LANGUAGE FlexibleContexts #-}

module MiniJava.TypeCheck where

import Control.Lens
import Control.Monad (liftM2)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
      , _classes = fromClassDecs $ ast ^. S.classes
      , _vars = M.empty
      , _curClass = Nothing
      , _curMethod = Nothing
      , _errors = []
      }
  return ()

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
  methods .= fromMethodDecs (classDec ^. S.methods)
  return ()
  where
    varDecs =
      fmap (\v -> (v ^. S.varId, v ^. S.varType)) (classDec ^. S.classVars)

-- Check the predicate in `while` and `if`
checkPred :: Monad m => S.Expression -> TC m ()
checkPred pred = do
  predType <- checkExpr pred
  _ <-
    case predType of
      S.TBool -> return ()
      S.TBottom -> return () -- errros in subexpression
      ty ->
        addError $
        "Predicate expression: " ++
        show pred ++ "\nExpected type: TBool" ++ "\nBut has: " ++ show ty
  return ()

-- Find the type of an identifer in the following order:
-- 1. Current scope
-- 2. Class scope
-- 3. Supertype scope (until the topmost one)
findVarType :: Monad m => S.Identifier -> TC m S.Type
findVarType i = do
  symTable <- use vars
  let currScopeTy = M.lookup i symTable
  case currScopeTy of
    Nothing -> do
      maybeCls <- use curClass
      case maybeCls of
        Nothing -> return S.TBottom
        Just cls -> findInClassScope i cls
    Just ty -> return ty
  return S.TBottom
  where
    findInClassScope i cls = do
      classTable <- use classes
      case M.lookup cls classTable of
        Nothing -> return S.TBottom
        Just cInfo ->
          liftM2 fromMaybe findInSuper (return $ M.lookup i (cInfo ^. cVars))
            -- find var in superclass
          where findInSuper = do
                  let super = cInfo ^. superClass
                  case super of
                    Nothing -> return S.TBottom
                    Just c -> findInClassScope i c

checkStatement :: Monad m => S.Statement -> TC m ()
-- Block
checkStatement (S.SBlock stmts) = mapM_ checkStatement stmts
-- If
checkStatement (S.SIf pred trueBranch falseBranch) = do
  checkPred pred
  checkStatement trueBranch
  checkStatement falseBranch
  return ()
-- While
checkStatement (S.SWhile pred stmt) = checkPred pred >> checkStatement stmt
checkStatement (S.SPrint expr) = do
  ty <- checkExpr expr
  case ty of
    S.TBool -> return ()
    S.TInt -> return ()
    _ ->
      addError $
      "Expression: `" ++
      show expr ++ "`\nwith type: `" ++ show ty ++ "` cannot be printed"
-- Identifer Assignment
checkStatement (S.SAssignId idtf expr) = do
  tyExpr <- checkExpr expr
  tyIdtf <- findVarType idtf
  if tyExpr == tyIdtf
    then return ()
    else addError $
         "Cannot match expected type `" ++
         show tyExpr ++
         "`\nwith actual type `" ++
         show tyIdtf ++
         "`\nIn assigning: " ++
         "`" ++ show idtf ++ "`\nwith `" ++ show expr ++ "`"
-- Int Array Assignment
checkStatement (S.SAssignArr idtf idxExpr expr) = do
  tyIdtf <- findVarType idtf
  tyIdx <- checkExpr idxExpr
  tyExpr <- checkExpr expr
  checkOrError S.TIntArray tyIdtf idtf
  checkOrError S.TInt tyIdx idxExpr
  checkOrError S.TInt tyExpr expr

checkOrError :: (Show a, Monad m) => S.Type -> S.Type -> a -> TC m ()
checkOrError expectedType actualType symbol =
  if expectedType == actualType
    then return ()
    else typeError expectedType actualType symbol

checkExpr :: Monad m => S.Expression -> TC m S.Type
checkExpr S.ETrue = return S.TBool
checkExpr S.EFalse = return S.TBool
checkExpr (S.EInt _) = return S.TInt
checkExpr (S.EParen expr) = checkExpr expr
checkExpr (S.EId idtf) = findVarType idtf
checkExpr S.EThis = do
  maybeCls <- use curClass
  case maybeCls of
    Nothing ->
      addError "Not in a class scope when using `this`" >> return S.TBottom
    Just clsIdtf -> return $ S.TClass clsIdtf
checkExpr s@(S.ENewObj idtf) = do
  result <- fmap (\_ -> S.TClass idtf) . M.lookup idtf <$> use classes
  liftM2
    fromMaybe
    (do addError $ "Cannot find class `" ++ show idtf ++ "`\nin " ++ show s
        return S.TBool) $
    pure result
checkExpr (S.ENewIntArr len) = do
  tyLen <- checkExpr len
  checkOrError S.TInt tyLen len
  return tyLen
