{-# LANGUAGE TemplateHaskell #-}

module MiniJava.TypeCheck.Type where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Text
import qualified Data.Vector as V
import MiniJava.Symbol

type TC m = StateT SymbolTable m

type ClassTable = M.Map Identifier ClassDec

type MethodTable = M.Map Identifier MethodDec

type VarTable = M.Map Identifier Type

data SymbolTable = SymbolTable
  { _methods :: MethodTable
  , _classes :: ClassTable
  , _vars :: VarTable
  , _curClass :: Maybe Identifier
  , _curMethod :: Maybe Identifier
  , _errors :: [Text]
  } deriving (Show)

makeLenses ''SymbolTable

type CheckResult = Either (V.Vector Text) ()
