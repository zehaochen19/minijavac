{-# LANGUAGE TemplateHaskell #-}

module MiniJava.TypeCheck.Type where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Text
import qualified Data.Vector as V
import MiniJava.Symbol

data ClassInfo = ClassInfo
  { _cVars :: VarTable
  , _superClass :: Maybe Identifier
  , _cMethods :: MethodTable
  } deriving (Show)

fromClassDec :: ClassDec -> (Identifier, ClassInfo)
fromClassDec dec =
  ( dec ^. className
  , ClassInfo
      (fromVarDecList $ dec ^. classVars)
      (dec ^. superClass)
      (fromMethodDecs $ dec ^. methods))

data MethodInfo = MethodInfo
  { _retType :: Type
  , _argsInfo :: [(Type, Identifier)]
  } deriving (Show)

fromMethodDec :: MethodDec -> (Identifier, MethodInfo)
fromMethodDec dec =
  (dec ^. methodId, MethodInfo (dec ^. returnType) (dec ^. args))

type ClassTable = M.Map Identifier ClassInfo

fromClassDecs :: [ClassDec] -> ClassTable
fromClassDecs = M.fromList . fmap fromClassDec

type MethodTable = M.Map Identifier MethodInfo

fromMethodDecs :: [MethodDec] -> MethodTable
fromMethodDecs = M.fromList . fmap fromMethodDec

type VarTable = M.Map Identifier Type

fromVarDecList :: [VarDec] -> VarTable
fromVarDecList = M.fromList . fmap (\dec -> (dec ^. varId, dec ^. varType))

data SymbolTable = SymbolTable
  { _methods :: MethodTable
  , _classes :: ClassTable
  , _vars :: VarTable
  , _curClass :: Maybe Identifier
  , _curMethod :: Maybe Identifier
  , _errors :: [Text]
  } deriving (Show)

type CheckResult = Either (V.Vector Text) ()

type TC m = StateT SymbolTable m

type TC' = State SymbolTable

makeLenses ''SymbolTable

makeLenses ''ClassInfo

makeLenses ''MethodInfo
