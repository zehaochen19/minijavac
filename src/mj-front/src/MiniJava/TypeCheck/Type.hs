{-# LANGUAGE TemplateHaskell #-}

module MiniJava.TypeCheck.Type where

import Control.Lens
import qualified Data.Map as M
import Data.Text
import qualified Data.Vector as V
import MiniJava.Symbol

type ClassTable = M.Map Identifier ClassDec

type MethodTable = M.Map Identifier MethodDec

type VarTable = M.Map Identifier Type

data SymbolTable = SymbolTable
  { _methods :: MethodTable
  , _classes :: ClassTable
  , _vars :: VarTable
  , _curClass :: Maybe ClassDec
  , _curMethod :: Maybe MethodDec
  , _errors :: V.Vector Text
  } deriving (Show)

makeLenses ''SymbolTable

data CheckResult
  = Err
  | Ok
  deriving (Eq, Show)
