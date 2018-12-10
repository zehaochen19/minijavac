{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MiniJava.Symbol where

import           Control.Lens
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified Text.Megaparsec               as M

data PosSymbol a = PosSymbol
  { _sym :: a
  , _pos :: M.SourcePos
  }

-- typeclass for showing symbols when errors occur
class MiniJavaSymbol s where
  sShow :: s -> String

symbolsJoin :: MiniJavaSymbol s => Char -> [s] -> String
symbolsJoin c ss = joined'
 where
  joined  = foldr (\s str -> c : ' ' : sShow s ++ str) "" ss
  joined' = if null joined then joined else drop 2 joined

instance MiniJavaSymbol s => MiniJavaSymbol [s] where
  sShow ss = '[' : symbolsJoin ',' ss ++ "]"

-- Wrapper of Text
newtype Identifier =
  Identifier Text
  deriving (Eq, Ord, Show, Generic)

instance MiniJavaSymbol Identifier where
  sShow (Identifier t) = show t

-- All supported type in MiniJava
data Type
  = TInt
  | TIntArray
  | TBool
  | TClass Identifier
  | TBottom -- representing errors
  deriving (Eq, Show, Generic)

instance MiniJavaSymbol Type where
  sShow TInt           = "int"
  sShow TIntArray      = "int[]"
  sShow TBool          = "boolean"
  sShow (TClass ident) = sShow ident
  sShow TBottom        = "⊥"

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
  deriving (Eq, Show, Generic)

instance MiniJavaSymbol Expression where
  sShow (EBinary op e1 e2   ) = sShow e1 ++ " " ++ sShow op ++ " " ++ sShow e2
  sShow (EArrayIndex arr idx) = sShow arr ++ "[" ++ sShow idx ++ "]"
  sShow (EArrayLength arr   ) = sShow arr ++ ".length"
  sShow (EMethodApp obj met args) =
    sShow obj ++ "." ++ sShow met ++ "(" ++ symbolsJoin ',' args ++ ")"
  sShow (EInt i   )       = show i
  sShow (EId  idtf)       = sShow idtf
  sShow ETrue             = "true"
  sShow EFalse            = "false"
  sShow EThis             = "this"
  sShow (ENewIntArr len ) = "new int[" ++ sShow len ++ "]"
  sShow (ENewObj    c   ) = "new " ++ sShow c ++ "()"
  sShow (ENot       expr) = '!' : sShow expr
  sShow (EParen     expr) = '(' : sShow expr ++ ")"


-- Binary Operators
data BinOp
  = BAnd
  | BLT
  | BPlus
  | BMinus
  | BMult
  deriving (Eq, Show, Generic)

instance MiniJavaSymbol BinOp where
  sShow BAnd   = "&&"
  sShow BLT    = "<"
  sShow BPlus  = "+"
  sShow BMinus = "-"
  sShow BMult  = "*"

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
  deriving (Eq, Show, Generic)

instance MiniJavaSymbol Statement where
  sShow (SBlock statements) =
    "{ " ++ foldr (\s str -> sShow s ++ " " ++ str) "" statements ++ " }"
  sShow (SIf pred trueClause falseClause) =
    "if ("
      ++ sShow pred
      ++ ") "
      ++ sShow trueClause
      ++ " else "
      ++ sShow falseClause
  sShow (SWhile pred body   ) = "while (" ++ sShow pred ++ ") " ++ sShow body
  sShow (SPrint expr        ) = "System.out.println(" ++ sShow expr ++ ");"
  sShow (SAssignId idtf expr) = sShow idtf ++ " = " ++ sShow expr ++ ";"
  sShow (SAssignArr arr idx value) =
    sShow arr ++ "[" ++ sShow idx ++ "] = " ++ sShow value ++ ";"


data MainClass = MainClass
  { _mainClassName :: Identifier
  , _mainArgs :: Identifier
  , _mainFunc :: Statement
  } deriving (Eq, Show, Generic)

data ClassDec = ClassDec
  { _className :: Identifier
  , _superClass :: Maybe Identifier
  , _classVars :: [VarDec]
  , _methods :: [MethodDec]
  } deriving (Eq, Show, Generic)

data VarDec = VarDec
  { _varType :: Type
  , _varId :: Identifier
  --, _varPos :: M.SourcePos
  } deriving (Eq, Show, Generic)

data MethodDec = MethodDec
  { _returnType :: Type
  , _methodId :: Identifier
  , _args :: [(Type, Identifier)]
  , _methodVars :: [VarDec]
  , _statements :: [Statement]
  , _retExp :: Expression
  } deriving (Eq, Show, Generic)

data MiniJavaAST = MiniJavaAST
  { _mainClass :: MainClass
  , _classes :: [ClassDec]
  } deriving (Eq, Show, Generic)

makeLenses ''VarDec

makeLenses ''ClassDec

makeLenses ''MiniJavaAST

makeLenses ''MethodDec
