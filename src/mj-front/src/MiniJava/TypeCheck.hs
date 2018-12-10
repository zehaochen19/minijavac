module MiniJava.TypeCheck where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Tuple                     ( swap )
import qualified MiniJava.Symbol               as S
import           MiniJava.TypeCheck.Type
import           MiniJava.TypeCheck.Util

typeCheck :: S.MiniJavaAST -> [T.Text]
typeCheck ast =
  reverse $ execState (checkMiniJava ast) emptySymbolTable ^. errors

checkMiniJava :: Monad m => S.MiniJavaAST -> TC m ()
checkMiniJava ast = do
  initSymbolTable ast
  checkMain $ ast ^. S.mainClass
  mapM_ checkClass $ ast ^. S.classes

emptySymbolTable = SymbolTable M.empty M.empty M.empty Nothing Nothing []

initSymbolTable :: Monad m => S.MiniJavaAST -> TC m ()
initSymbolTable ast = do
  put SymbolTable
    { _methods   = M.empty
    , _classes   = fromClassDecs $ ast ^. S.classes
    , _vars      = M.empty
    , _curClass  = Nothing
    , _curMethod = Nothing
    , _errors    = []
    }
  return ()

checkMain :: Monad m => S.MainClass -> TC m ()
checkMain (S.MainClass _ _ mainFunc) = checkStatement mainFunc

checkClass :: Monad m => S.ClassDec -> TC m ()
checkClass classDec = do
  initClassScope classDec
  mapM_ checkMethod $ classDec ^. S.methods

-- check all types of variables are in scope
-- will return all types that are not in scope
checkVarsTypeScope :: Monad m => VarTable -> TC m ()
checkVarsTypeScope varTable = do
  classTable <- use classes
  let unscoped = M.filter (not . inScope classTable) varTable
  forM_
    (M.toList unscoped)
    (\(idtf, ty) ->
      addError
        $  "Variable "
        ++ S.sShow idtf
        ++ " has type "
        ++ S.sShow ty
        ++ "\nbut this type is not in scope"
    )
 where
  inScope :: ClassTable -> S.Type -> Bool
  inScope _          S.TInt          = True
  inScope _          S.TBool         = True
  inScope classTable (S.TClass idtf) = isJust $ M.lookup idtf classTable

checkMethod :: Monad m => S.MethodDec -> TC m ()
checkMethod methodDec = do
  curMethod .= (Just $ methodDec ^. S.methodId) -- update current method
  vars .= collectVars (methodDec ^. S.args) (methodDec ^. S.methodVars) -- update varaible table
  use vars >>= checkVarsTypeScope
  mapM_ checkStatement $ methodDec ^. S.statements
  -- Check return type
  tyRetExpr <- checkExpr $ methodDec ^. S.retExp
  let tyDecRet = methodDec ^. S.returnType
  if tyRetExpr == tyDecRet
    then return ()
    else typeError tyDecRet tyRetExpr $ methodDec ^. S.retExp
 where
  collectVars args varDecs =
    M.fromList
      $  ((\varDec -> (varDec ^. S.varId, varDec ^. S.varType)) <$> varDecs)
      ++ (swap <$> args)

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
  case predType of
    S.TBool   -> return ()
    S.TBottom -> return () -- errros in subexpression
    ty        -> do
      addError
        $  "Predicate expression: "
        ++ S.sShow pred
        ++ "\nExpected type: TBool"
        ++ "\nBut has: "
        ++ S.sShow ty
      return ()

-- Find the method info in the following order:
-- 1. Scope defined by class identifier
-- 3. Supertype scope (until the topmost one) 
findMetInfo
  :: Monad m => S.Identifier -> S.Identifier -> TC m (Maybe MethodInfo)
findMetInfo cls met = do
  classTable <- use classes
  case M.lookup cls classTable of
    Nothing -> do
      addError
        $  "Cannot find class of "
        ++ S.sShow cls
        ++ " when applying method "
        ++ S.sShow met
      return Nothing
    Just clsInfo -> do
      let maybeMetInfo = clsInfo ^. cMethods
      case M.lookup met maybeMetInfo of
        Nothing -> -- find method info in super class
                   case clsInfo ^. superClass of
          Nothing    -> return Nothing
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
      addError $ "Cannot find type of variable " ++ S.sShow i
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
        Nothing  -> return S.TBottom
        Just cls -> findInClassScope i cls []
    Just ty -> return ty
 where
  findInClassScope i cls checkedClss = if cls `elem` checkedClss
    then do
      addError $ "Circular inheritance chain: " ++ S.sShow (cls : checkedClss)
      return S.TBottom
    else do
      classTable <- use classes
      case M.lookup cls classTable of
        Nothing    -> return S.TBottom
        Just cInfo -> case M.lookup i (cInfo ^. cVars) of
          Nothing -> findInSuper
          Just c  -> return c
            -- find var in superclass
         where
          findInSuper = do
            let super = cInfo ^. superClass
            case super of
              Nothing -> return S.TBottom
              Just c  -> findInClassScope i c (cls : checkedClss)

checkStatement :: Monad m => S.Statement -> TC m ()
-- Block
checkStatement (S.SBlock stmts _) = mapM_ checkStatement stmts
-- If
checkStatement (S.SIf pred trueBranch falseBranch _) = do
  checkPred pred
  checkStatement trueBranch
  checkStatement falseBranch
  return ()
-- While
checkStatement (S.SWhile pred stmt _) = checkPred pred >> checkStatement stmt
checkStatement (S.SPrint expr _     ) = do
  ty <- checkExpr expr
  case ty of
    S.TBool -> return ()
    S.TInt  -> return ()
    _ ->
      addError
        $  "Expression: "
        ++ S.sShow expr
        ++ "\nwith type: "
        ++ S.sShow ty
        ++ " cannot be printed"
-- Identifer Assignment
checkStatement (S.SAssignId idtf expr _) = do
  tyExpr <- checkExpr expr
  tyIdtf <- findVarType idtf
  if tyExpr == tyIdtf
    then return ()
    else
      addError
      $  "Cannot match expected type "
      ++ S.sShow tyExpr
      ++ "\nwith actual type "
      ++ S.sShow tyIdtf
      ++ "\nIn assigning: "
      ++ S.sShow idtf
      ++ "\nwith "
      ++ S.sShow expr
-- Int Array Assignment
checkStatement (S.SAssignArr idtf idxExpr expr _) = do
  tyIdtf <- findVarType idtf
  tyIdx  <- checkExpr idxExpr
  tyExpr <- checkExpr expr
  checkOrError S.TIntArray tyIdtf idtf
  checkOrError S.TInt      tyIdx  idxExpr
  checkOrError S.TInt      tyExpr expr
  return ()

checkOrError
  :: (S.MiniJavaSymbol a, Monad m)
  => S.Type
  -> S.Type
  -> a
  -> TC m (Maybe S.Type)
checkOrError expectedType actualType symbol = if expectedType == actualType
  then return $ Just expectedType
  else typeError expectedType actualType symbol >> return Nothing

checkExpr :: Monad m => S.Expression -> TC m S.Type
checkExpr (S.ETrue  _   ) = return S.TBool
checkExpr (S.EFalse _   ) = return S.TBool
checkExpr (S.ENot _ expr) = do
  tyExpr <- checkExpr expr
  result <- checkOrError S.TBool tyExpr expr
  return $ fromMaybe S.TBottom result
checkExpr (S.EInt   _ _   ) = return S.TInt
checkExpr (S.EParen _ expr) = checkExpr expr
checkExpr (S.EId    _ idtf) = findVarType idtf
checkExpr (S.EThis _      ) = do
  maybeCls <- use curClass
  case maybeCls of
    Nothing ->
      addError "Not in a class scope when using this" >> return S.TBottom
    Just clsIdtf -> return $ S.TClass clsIdtf
checkExpr s@(S.ENewObj _ idtf) = do
  result <- fmap (\_ -> S.TClass idtf) . M.lookup idtf <$> use classes
  case result of
    Nothing -> do
      addError $ "Cannot find class " ++ S.sShow idtf ++ "\nin " ++ S.sShow s
      return S.TBool
    Just ty -> return ty
checkExpr (S.ENewIntArr _ len) = do
  tyLen <- checkExpr len
  checkOrError S.TInt tyLen len
  return S.TIntArray
checkExpr (S.EArrayLength _ expr) = do
  tyExpr <- checkExpr expr
  result <- checkOrError S.TIntArray tyExpr expr
  return $ maybe S.TBottom (const S.TInt) result
checkExpr (S.EArrayIndex _ arr idx) = do
  tyArr  <- checkExpr arr
  tyIdx  <- checkExpr idx
  resArr <- checkOrError S.TIntArray tyArr arr
  resIdx <- checkOrError S.TInt tyIdx idx
  return $ case (resArr, resIdx) of
    (Just _, Just _) -> S.TInt
    _                -> S.TBottom
checkExpr (S.EMethodApp _ obj met args) = do
  tyObj <- checkExpr obj
  case tyObj of
    S.TClass cls -> do
      metInfo <- findMetInfo cls met
      case metInfo of
        Nothing -> do
          addError
            $  "Cannot find method "
            ++ S.sShow met
            ++ " in class "
            ++ S.sShow cls
          return S.TBottom
        Just metInfo -> do
          let metArgsTypes = metInfo ^. argsInfo
          argsTypes <- mapM checkExpr args
          return $ if argsTypes == metArgsTypes
            then metInfo ^. retType
            else S.TBottom
    _ -> do
      addError
        $  "Cannot apply method "
        ++ S.sShow obj
        ++ " on non-class object"
        ++ S.sShow obj
      return S.TBottom
checkExpr (S.EBinary _ op expr1 expr2)
  | op `elem` [S.BPlus, S.BMinus, S.BMult] = checkOperands S.TInt S.TInt S.TInt
  | op == S.BLT = checkOperands S.TInt S.TInt S.TBool
  | op == S.BAnd = checkOperands S.TBool S.TBool S.TBool
 where
  checkOperands ty1 ty2 resType = do
    tyExpr1 <- checkExpr expr1
    tyExpr2 <- checkExpr expr2
    res1    <- checkOrError tyExpr1 ty1 expr1
    res2    <- checkOrError tyExpr2 ty2 expr2
    return $ case (res1, res2) of
      (Just _, Just _) -> resType
      _                -> S.TBottom
