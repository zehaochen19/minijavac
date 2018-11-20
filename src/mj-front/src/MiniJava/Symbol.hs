{-# LANGUAGE TemplateHaskell #-}

module MiniJava.Symbol where

import Control.Lens
import Data.Text (Text)

-- Wrapper of Text
newtype Identifier =
  Identifier Text
  deriving (Eq, Show, Ord)

-- All supported type in MiniJava
data Type
  = TInt
  | TIntArray
  | TBool
  | TClass Identifier
  | TBottom -- representing errors
  deriving (Eq, Show)

data Expression
  = EBinary BinOp
            Expression
            Expression
  | EArrayIndex Expression
                Expression
  | EArrayLength Expression
  | EMethodApp Expression
               Identifier
               [Expression]
  | EInt Integer
  | ETrue
  | EFalse
  | EId Identifier
  | EThis
  | ENewIntArr Expression
  | ENewObj Identifier
  | ENot Expression
  | EParen Expression
  deriving (Eq, Show)

-- Binary Operators
data BinOp
  = BAnd
  | BLT
  | BPlus
  | BMinus
  | BMult
  deriving (Eq, Show)

data Statement
  = SBlock [Statement]
  | SIf Expression
        Statement
        Statement
  | SWhile Expression
           Statement
  | SPrint Expression
  | SAssignId Identifier
              Expression
  | SAssignArr Identifier
               Expression
               Expression
  deriving (Eq, Show)

data MainClass = MainClass
  { _mainClassName :: Identifier
  , _mainArgs :: Identifier
  , _mainFunc :: Statement
  } deriving (Eq, Show)

data ClassDec = ClassDec
  { _className :: Identifier
  , _superClass :: Maybe Identifier
  , _classVars :: [VarDec]
  , _methods :: [MethodDec]
  } deriving (Eq, Show)

data VarDec = VarDec
  { _varType :: Type
  , _varId :: Identifier
  } deriving (Eq, Show)

data MethodDec = MethodDec
  { _returnType :: Type
  , _methodId :: Identifier
  , _args :: [(Type, Identifier)]
  , _methodVars :: [VarDec]
  , _statements :: [Statement]
  , _retExp :: Expression
  } deriving (Eq, Show)

data MiniJavaAST = MiniJavaAST
  { _mainClass :: MainClass
  , _classes :: [ClassDec]
  } deriving (Eq, Show)

makeLenses ''VarDec

makeLenses ''ClassDec

makeLenses ''MiniJavaAST

makeLenses ''MethodDec
