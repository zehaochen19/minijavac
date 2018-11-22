{-# LANGUAGE FlexibleContexts #-}

module MiniJava.TypeCheck where

import Control.Lens
import Control.Monad (liftM2)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified MiniJava.Symbol as S
import MiniJava.TypeCheck.Type
import MiniJava.TypeCheck.Util

typeCheck :: S.MiniJavaAST -> [T.Text]
typeCheck ast = execState (checkMiniJava ast) emptySymbolTable ^. errors

checkMiniJava :: Monad m => S.MiniJavaAST -> TC m ()
checkMiniJava ast = do
  initSymbolTable ast
  checkMain $ ast ^. S.mainClass
  mapM_ checkClass $ ast ^. S.classes

emptySymbolTable = SymbolTable M.empty M.empty M.empty Nothing Nothing []

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
checkMain (S.MainClass _ _ mainFunc) = checkStatement mainFunc

checkClass :: Monad m => S.ClassDec -> TC m ()
checkClass classDec = do
  initClassScope classDec
  mapM_ checkMethod $ classDec ^. S.methods

checkMethod :: Monad m => S.MethodDec -> TC m ()
checkMethod methodDec = do
  curMethod .= (Just $ methodDec ^. S.methodId) -- update current method
  vars .= collectVars (methodDec ^. S.args) (methodDec ^. S.methodVars) -- update varaible table
  mapM_ checkStatement $ methodDec ^. S.statements
  -- Check return type
  tyRetExpr <- checkExpr $ methodDec ^. S.retExp
  let tyDecRet = methodDec ^. S.returnType
  if tyRetExpr == tyDecRet
    then return ()
    else typeError tyDecRet tyRetExpr $ methodDec ^. S.retExp
  where
    collectVars args varDecs =
      M.fromList $
      ((\varDec -> (varDec ^. S.varId, varDec ^. S.varType)) <$> varDecs) ++
      (swap <$> args)

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

-- Find the method info in the following order:
-- 1. Scope defined by class identifier
-- 3. Supertype scope (until the topmost one) 
findMetInfo ::
     Monad m => S.Identifier -> S.Identifier -> TC m (Maybe MethodInfo)
findMetInfo cls met = do
  classTable <- use classes
  case M.lookup cls classTable of
    Nothing -> do
      addError $
        "Cannot find class of `" ++
        show cls ++ "` when applying method `" ++ show met ++ "`"
      return Nothing
    Just clsInfo -> do
      let maybeMetInfo = clsInfo ^. cMethods
      case M.lookup met maybeMetInfo of
        Nothing -- find method info in super class
         ->
          case clsInfo ^. superClass of
            Nothing -> return Nothing
            Just super -> findMetInfo super met
        Just metInfo -> return $ Just metInfo

-- Find the type of an identifer in the following order:
-- 1. Current scope
-- 2. Class scope
-- 3. Supertype scope (until the topmost one)
findVarType :: Monad m => S.Identifier -> TC m S.Type
findVarType i = do
  ty <- findVarType' i
  case ty of
    S.TBottom -> do
      addError $ "Cannot find type of variable `" ++ show i ++ "`"
      return S.TBottom
    ty -> return ty

findVarType' :: Monad m => S.Identifier -> TC m S.Type
findVarType' i = do
  symTable <- use vars
  let currScopeTy = M.lookup i symTable
  case currScopeTy of
    Nothing -> do
      maybeCls <- use curClass
      case maybeCls of
        Nothing -> return S.TBottom
        Just cls -> findInClassScope i cls
    Just ty -> return ty
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
  return ()

checkOrError ::
     (Show a, Monad m) => S.Type -> S.Type -> a -> TC m (Maybe S.Type)
checkOrError expectedType actualType symbol =
  if expectedType == actualType
    then return $ Just expectedType
    else typeError expectedType actualType symbol >> return Nothing

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
checkExpr (S.EArrayLength expr) = do
  tyExpr <- checkExpr expr
  result <- checkOrError S.TIntArray tyExpr expr
  return $ maybe S.TBottom (const S.TInt) result
checkExpr (S.EArrayIndex arr idx) = do
  tyArr <- checkExpr arr
  tyIdx <- checkExpr idx
  resArr <- checkOrError S.TIntArray tyArr arr
  resIdx <- checkOrError S.TInt tyIdx idx
  return $
    case (resArr, resIdx) of
      (Just _, Just _) -> S.TInt
      _ -> S.TBottom
checkExpr (S.EMethodApp obj met args) = do
  tyObj <- checkExpr obj
  case tyObj of
    S.TClass cls -> do
      metInfo <- findMetInfo cls met
      case metInfo of
        Nothing -> do
          addError $
            "Cannot find method `" ++
            show met ++ "` in class `" ++ show cls ++ "`"
          return S.TBottom
        Just metInfo -> do
          let metArgsTypes = fst <$> metInfo ^. argsInfo
          argsTypes <- mapM checkExpr args
          return $
            if argsTypes == metArgsTypes
              then metInfo ^. retType
              else S.TBottom
    _ -> do
      addError $
        "Cannot apply method `" ++
        show obj ++ "` on non-class object`" ++ show obj ++ "`"
      return S.TBottom
checkExpr (S.EBinary op expr1 expr2)
  | op `elem` [S.BPlus, S.BMinus, S.BMult] = checkOperands S.TInt S.TInt S.TInt
  | op == S.BLT = checkOperands S.TInt S.TInt S.TBool
  | op == S.BAnd = checkOperands S.TBool S.TBool S.TBool
  where
    checkOperands ty1 ty2 resType = do
      tyExpr1 <- checkExpr expr1
      tyExpr2 <- checkExpr expr2
      res1 <- checkOrError tyExpr1 ty1 expr1
      res2 <- checkOrError tyExpr2 ty2 expr2
      return $
        case (res1, res2) of
          (Just _, Just _) -> resType
          _ -> S.TBottom
