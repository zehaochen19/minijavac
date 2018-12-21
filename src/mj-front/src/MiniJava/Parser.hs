{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module MiniJava.Parser
  ( Parser
  , ParserT
  , parseFromSrc
  , identifierP
  , expressionP
  , statementP
  , varDecP
  , sc
  , methodDecP
  , mainClassDecP
  , classDecP
  , ConfigReader
  )
where

import           Control.Monad.Combinators.Expr
import           Data.Functor                   ( ($>) )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void
import           MiniJava.Symbol
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Identity
import qualified Control.Monad.Reader          as R
import           MiniJava.Config
import qualified Data.List.NonEmpty            as NE


type Parser = Parsec Void Text
type ParserT = ParsecT Void Text

type ConfigReader =  R.MonadReader Config

parseFromSrc :: FilePath -> IO (Either (ParseErrorBundle Text Void) MiniJavaAST)
parseFromSrc src = do
  program <- TIO.readFile src
  return $ runIdentity $ R.runReaderT (runParserT miniJavaP src program)
                                      (Config False)

sc :: ParserT m ()
sc = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

lexeme :: ParserT m a -> ParserT m a
lexeme = L.lexeme sc

symbol :: Text -> ParserT m Text
symbol = L.symbol sc

paren :: ParserT m a -> ParserT m a
paren = between (symbol "(") (symbol ")")

block :: ParserT m a -> ParserT m a
block = between (symbol "{") (symbol "}")

bracket :: ParserT m a -> ParserT m a
bracket = between (symbol "[") (symbol "]")

integerP :: ParserT m Integer
integerP = lexeme L.decimal

data Semi = Semi

type RawSemi s e = Either (ParseError s e) Semi

semiP :: ParserT m Semi
semiP = Semi <$ symbol ";" <?> "semicolon"

rawSemiP :: ParserT m (RawSemi Text Void)
rawSemiP = withRecovery (pure . Left) (Right <$> semiP)

expectSemi :: ParseError Text Void -> Bool
expectSemi (TrivialError _ _ expected) =
  S.member (Label $ NE.fromList "semicolon") expected

dotP :: ParserT m Text
dotP = symbol "." <?> "dot symbol"

commaP :: ParserT m Text
commaP = symbol "," <?> "comma"

-- Reserved key words
reserved :: S.Set Text
reserved = S.fromList
  [ "class"
  , "public"
  , "static"
  , "void"
  , "main"
  , "boolean"
  , "int"
  , "if"
  , "else"
  , "while"
  , "true"
  , "false"
  , "new"
  , "this"
  , "return"
  , "extends"
  ]

-- Identifiers should start with [0..] underscores and an alphabet
-- and shouldn't be a reserved keyword
identifierP :: ParserT m Identifier
identifierP = (lexeme . try) (p >>= check) <?> "Ideintifier"
 where
  p =
    fmap T.pack
      $   (\us x xs -> us ++ x : xs)
      <$> many (char '_')
      <*> letterChar
      <*> many (char '_' <|> alphaNumChar)
  check x = if x `S.member` reserved
    then fail $ "keyword" ++ show x ++ " cannot be an identifier"
    else return $ Identifier x

expressionListP :: ParserT m [Expression]
expressionListP =
  label "List of Expressions" $ paren $ sepBy expressionP commaP

expressionP :: ParserT m Expression
expressionP = label "Expression" $ makeExprParser basicExpressionP operatorP

-- Unambiguous parts of Expressions
basicExpressionP :: ParserT m Expression
basicExpressionP =
  (ETrue <$> getSourcePos <* symbol "true")
    <|> (EFalse <$> getSourcePos <* symbol "false")
    <|> (EInt <$> getSourcePos <*> integerP)
    <|> (EParen <$> getSourcePos <*> paren expressionP)
    <|> (EId <$> getSourcePos <*> identifierP)
    <|> (EThis <$> (symbol "this" >> getSourcePos))
    <|> try newObjectP
    <|> try newArrayP
 where
  newObjectP = do
    pos <- getSourcePos
    idt <- symbol "new" >> identifierP
    symbol "(" >> symbol ")"
    return $ ENewObj pos idt
  newArrayP =
    ENewIntArr
      <$> getSourcePos
      <*> (symbol "new" >> symbol "int" >> bracket expressionP)

-- Operators for Expressions
-- To handle left recursive situations, method application,
-- array indexing and array lenght are treated as operators
operatorP :: [[Operator (ParserT m) Expression]]
operatorP =
  [ [Postfix indexingP, Postfix arrayLenP, Postfix methodP]
  , [ Prefix $ do
        pos <- getSourcePos
        symbol "!"
        return $ \expr -> ENot pos expr
    ]
  , [ InfixL $ do
        pos <- getSourcePos
        symbol "*"
        return $ \expr1 expr2 -> EBinary pos BMult expr1 expr2
    ]
  , [ InfixL $ do
      pos <- getSourcePos
      symbol "+"
      return $ \expr1 expr2 -> EBinary pos BPlus expr1 expr2
    , InfixL $ do
      pos <- getSourcePos
      symbol "-"
      return $ \expr1 expr2 -> EBinary pos BMinus expr1 expr2
    ]
  , [ InfixL $ do
        pos <- getSourcePos
        symbol "<"
        return $ \expr1 expr2 -> EBinary pos BLT expr1 expr2
    ]
  , [ InfixL $ do
        pos <- getSourcePos
        symbol "&&"
        return $ \expr1 expr2 -> EBinary pos BAnd expr1 expr2
    ]
  ]
 where
  indexingP :: ParserT m (Expression -> Expression)
  indexingP = do
    pos <- getSourcePos
    idx <- bracket expressionP
    return $ \expr -> EArrayIndex pos expr idx
  arrayLenP :: ParserT m (Expression -> Expression)
  arrayLenP = try $ do
    pos <- getSourcePos
    dotP >> symbol "length"
    return $ \expr -> EArrayLength pos expr
  methodP :: ParserT m (Expression -> Expression)
  methodP = try $ do
    pos <- getSourcePos
    dotP
    idt     <- identifierP
    argList <- expressionListP
    return $ \expr -> EMethodApp pos expr idt argList

typeP :: ParserT m Type
typeP =
  label "Type"
    $   try intArrP
    <|> (TInt <$ symbol "int")
    <|> (TBool <$ symbol "boolean")
    <|> (TClass <$> identifierP)
  where intArrP = symbol "int" *> symbol "[" *> symbol "]" $> TIntArray

typeIdtPairP :: ParserT m (Type, Identifier)
typeIdtPairP = do
  t   <- typeP
  idt <- identifierP
  return (t, idt)

varDecP :: ParserT m VarDec
varDecP = label "Variable Declaration" $ do
  pos      <- getSourcePos
  (t, idt) <- typeIdtPairP
  semiP
  return $ VarDec pos t idt

statementFixP :: ConfigReader m => ParserT m Statement
statementFixP =
  label "Statement"
    $   printP
    <|> try assignP
    <|> try arrayAssignP
    <|> ifP
    <|> whileP
    <|> blockStmtP
 where
  printP       = printBodyP >>= checkSemi
  assignP      = assignBodyP >>= checkSemi
  arrayAssignP = arrayAssignBodyP >>= checkSemi
  checkSemi p = do
    semi <- rawSemiP
    case semi of
      Right _   -> return p
      Left  err -> if expectSemi err then return p else undefined

statementNoFixP :: ConfigReader m => ParserT m Statement
statementNoFixP =
  label "Statement"
    $   printP
    <|> try assignP
    <|> try arrayAssignP
    <|> ifP
    <|> whileP
    <|> blockStmtP
 where
  printP       = printBodyP <* semiP
  assignP      = assignBodyP <* semiP
  arrayAssignP = arrayAssignBodyP <* semiP

statementP :: ConfigReader m => ParserT m Statement
statementP = do
  (Config fixSemi) <- R.ask
  if fixSemi then statementFixP else statementNoFixP


blockStmtP :: ConfigReader m => ParserT m Statement
blockStmtP = SBlock <$> getSourcePos <*> (block . many) statementP

ifP :: ConfigReader m => ParserT m Statement
ifP = do
  pos <- getSourcePos
  symbol "if"
  predicate  <- paren expressionP
  bodyClause <- statementP
  symbol "else"
  SIf pos predicate bodyClause <$> statementP

whileP :: ConfigReader m => ParserT m Statement
whileP = do
  pos <- getSourcePos
  symbol "while"
  predicate <- paren expressionP
  SWhile pos predicate <$> statementP

printBodyP :: ParserT m Statement
printBodyP = do
  pos <- getSourcePos
  symbol "System.out.println"
  expr <- paren expressionP
  return $ SPrint pos expr

assignBodyP :: ParserT m Statement
assignBodyP = do
  pos <- getSourcePos
  idt <- identifierP
  SAssignId pos idt <$> assignTail

arrayAssignBodyP :: ParserT m Statement
arrayAssignBodyP = do
  pos <- getSourcePos
  idt <- identifierP
  idx <- bracket expressionP
  SAssignArr pos idt idx <$> assignTail

assignTail :: ParserT m Expression
assignTail = symbol "=" >> expressionP


methodDecP :: ConfigReader m => ParserT m MethodDec
methodDecP = label "Method Declaration" $ do
  pos <- getSourcePos
  symbol "public"
  t       <- typeP
  idt     <- identifierP
  argList <- argListP
  symbol "{"
  vs <- many $ try varDecP
  ss <- many $ try statementP
  symbol "return"
  result <- expressionP
  semiP
  symbol "}"
  return $ MethodDec pos t idt argList vs ss result
  where argListP = paren $ sepBy typeIdtPairP commaP


mainClassDecP :: ConfigReader m => ParserT m MainClass
mainClassDecP = label "Main Class Declaration" $ do
  symbol "class"
  clsName <- identifierP
  symbol "{" >> symbol "public" >> symbol "static" >> symbol "void" >> symbol
    "main"
  argsName <- paren $ symbol "String" >> symbol "[" >> symbol "]" >> identifierP
  symbol "{"
  body <- statementP
  symbol "}" >> symbol "}"
  return $ MainClass clsName argsName body


classDecP :: ConfigReader m => ParserT m ClassDec
classDecP = label "Class Declaration" $ do
  symbol "class"
  clsName    <- identifierP
  superClass <- optional $ try $ symbol "extends" >> identifierP
  symbol "{"
  vars <- many varDecP
  mets <- many methodDecP
  symbol "}"
  return $ ClassDec clsName superClass vars mets


miniJavaP :: ParserT (R.Reader Config) MiniJavaAST
miniJavaP = do
  sc
  mc   <- mainClassDecP
  clss <- many classDecP
  eof
  return $ MiniJavaAST mc clss
