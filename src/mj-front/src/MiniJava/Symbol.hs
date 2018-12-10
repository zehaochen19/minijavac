{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MiniJava.Symbol where

import           Control.Lens
import           Data.Text                      ( Text )
import           GHC.Generics
import qualified Text.Megaparsec               as M


-- typeclass for showing symbols when errors occur
class MiniJavaSymbol s where
  sShow :: s -> String

class WithPos s where
  getPos :: s -> M.SourcePos

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
  = EBinary M.SourcePos
            BinOp
            Expression
            Expression
  | EArrayIndex M.SourcePos
                Expression
                Expression
  | EArrayLength M.SourcePos Expression
  | EMethodApp M.SourcePos
               Expression
               Identifier
               [Expression]
  | EInt M.SourcePos Integer
  | ETrue M.SourcePos
  | EFalse M.SourcePos
  | EId M.SourcePos Identifier
  | EThis M.SourcePos
  | ENewIntArr M.SourcePos Expression
  | ENewObj M.SourcePos Identifier
  | ENot M.SourcePos Expression
  | EParen M.SourcePos Expression
  deriving (Eq, Show, Generic)


instance MiniJavaSymbol Expression where
  sShow (EBinary  _ op e1 e2   ) = sShow e1 ++ " " ++ sShow op ++ " " ++ sShow e2
  sShow (EArrayIndex _ arr idx ) = sShow arr ++ "[" ++ sShow idx ++ "]"
  sShow (EArrayLength _ arr    ) = sShow arr ++ ".length"
  sShow (EMethodApp _ obj met args ) =
    sShow obj ++ "." ++ sShow met ++ "(" ++ symbolsJoin ',' args ++ ")"
  sShow (EInt _  i       ) = show i
  sShow (EId  _ idtf       ) = sShow idtf
  sShow (ETrue  _         ) = "true"
  sShow (EFalse _         ) = "false"
  sShow (EThis  _         ) = "this"
  sShow (ENewIntArr _ len  ) = "new int[" ++ sShow len ++ "]"
  sShow (ENewObj _     c    ) = "new " ++ sShow c ++ "()"
  sShow (ENot       _ expr ) = '!' : sShow expr
  sShow (EParen     _ expr ) = '(' : sShow expr ++ ")"

instance WithPos Expression where
  getPos (EBinary pos _ _ _   ) = pos
  getPos (EArrayIndex pos _ _  ) = pos
  getPos (EArrayLength pos _   ) = pos
  getPos (EMethodApp pos _ _ _ ) = pos
  getPos (EInt  pos _           ) = pos
  getPos (EId   pos _           ) = pos
  getPos (ETrue  pos          ) = pos
  getPos (EFalse pos          ) = pos
  getPos (EThis  pos          ) = pos
  getPos (ENewIntArr  pos _    ) = pos
  getPos (ENewObj     pos _    ) = pos
  getPos (ENot        pos _     ) = pos
  getPos (EParen      pos _     ) = pos


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
  = SBlock M.SourcePos [Statement]
  | SIf M.SourcePos Expression
        Statement
        Statement
  | SWhile M.SourcePos
          Expression
           Statement
  | SPrint M.SourcePos Expression
  | SAssignId M.SourcePos
              Identifier
              Expression
  | SAssignArr M.SourcePos
               Identifier
               Expression
               Expression
  deriving (Eq, Show, Generic)

instance WithPos Statement where
  getPos (SBlock  pos _        ) = pos
  getPos (SIf pos _ _ _        ) = pos
  getPos (SWhile  pos _ _      ) = pos
  getPos (SPrint pos _         ) = pos
  getPos (SAssignId pos _ _    ) = pos
  getPos (SAssignArr pos _ _ _ ) = pos

instance MiniJavaSymbol Statement where
  sShow (SBlock _ statements ) =
    "{ " ++ foldr (\s str -> sShow s ++ " " ++ str) "" statements ++ " }"
  sShow (SIf  _ pred trueClause falseClause) =
    "if ("
      ++ sShow pred
      ++ ") "
      ++ sShow trueClause
      ++ " else "
      ++ sShow falseClause
  sShow (SWhile _ pred body    ) = "while (" ++ sShow pred ++ ") " ++ sShow body
  sShow (SPrint  _ expr        ) = "System.out.println(" ++ sShow expr ++ ");"
  sShow (SAssignId  _ idtf expr) = sShow idtf ++ " = " ++ sShow expr ++ ";"
  sShow (SAssignArr _ arr idx value ) =
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
  { _varPos :: M.SourcePos
  , _varType :: Type
  , _varId :: Identifier
  } deriving (Eq, Show, Generic)


data MethodDec = MethodDec
  {  _metPos :: M.SourcePos
  ,  _returnType :: Type
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

instance WithPos MethodDec where
  getPos metDec = metDec ^. metPos
