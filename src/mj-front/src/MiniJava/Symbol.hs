{-# LANGUAGE TemplateHaskell #-}

module MiniJava.Symbol where

import           Control.Lens
import           Data.Text                      ( Text )

-- Wrapper of Text
newtype Identifier =
  Identifier Text
  deriving (Eq, Ord, Show)

-- All supported type in MiniJava
data Type
  = TInt
  | TIntArray
  | TBool
  | TClass Identifier
  | TBottom -- representing errors
  deriving (Eq)

instance Show Type where
  show TInt = "int"
  show TIntArray = "int[]"
  show TBool = "boolean"
  show (TClass ident) = show ident
  show TBottom = "⊥"

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
  deriving (Eq)

instance Show Expression where
  show (EBinary op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (EArrayIndex arr idx) = show arr ++ "[" ++ show idx ++ "]"

-- Binary Operators
data BinOp
  = BAnd
  | BLT
  | BPlus
  | BMinus
  | BMult
  deriving (Eq)

instance Show BinOp where
  show BAnd = "&&"
  show BLT = "<"
  show BPlus = "+"
  show BMinus = "-"
  show BMult = "*"

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
  deriving (Eq)

instance Show Statement where
  show (SBlock statements) =
    "{ " ++ foldr (\s str -> show s ++ " " ++  str) "" statements ++ " }"
  show (SIf pred trueClause falseClause) =
    "if (" ++ show pred ++ ") " ++ show trueClause ++ " else " ++ show falseClause
  show (SWhile pred body) =
    "while (" ++ show pred ++ ") " ++ show body
  show (SPrint expr) = "System.out.println(" ++ show expr ++ ");"
  show (SAssignId idtf expr) = show idtf ++ " = " ++ show expr ++ ";"
  show (SAssignArr arr idx value) = show arr ++ "[" ++ show idx ++ "] = " ++ show value ++ ";"


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
