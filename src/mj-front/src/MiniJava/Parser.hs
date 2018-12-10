module MiniJava.Parser where

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



type Parser = Parsec Void Text

parseFromSrc :: FilePath -> IO (Either (ParseErrorBundle Text Void) MiniJavaAST)
parseFromSrc src = do
  program <- TIO.readFile src
  return $ parse miniJavaP src program

sc :: Parser ()
sc = L.space space1 lineComment blockComment
 where
  lineComment  = L.skipLineComment "//"
  blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

block :: Parser a -> Parser a
block = between (symbol "{") (symbol "}")

bracket :: Parser a -> Parser a
bracket = between (symbol "[") (symbol "]")

integerP :: Parser Integer
integerP = lexeme L.decimal

semiP :: Parser Text
semiP = symbol ";" <?> "semicolon"

dotP :: Parser Text
dotP = symbol "." <?> "dot symbol"

commaP :: Parser Text
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
identifierP :: Parser Identifier
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

expressionListP :: Parser [Expression]
expressionListP =
  label "List of Expressions" $ paren $ sepBy expressionP commaP

expressionP :: Parser Expression
expressionP = label "Expression" $ makeExprParser basicExpressionP operatorP

-- Unambiguous parts of Expressions
basicExpressionP :: Parser Expression
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
operatorP :: [[Operator Parser Expression]]
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
  indexingP :: Parser (Expression -> Expression)
  indexingP = do
    pos <- getSourcePos
    idx <- bracket expressionP
    return $ \expr -> EArrayIndex pos expr idx
  arrayLenP :: Parser (Expression -> Expression)
  arrayLenP = try $ do
    pos <- getSourcePos
    dotP >> symbol "length"
    return $ \expr -> EArrayLength pos expr
  methodP :: Parser (Expression -> Expression)
  methodP = try $ do
    pos <- getSourcePos
    dotP
    idt     <- identifierP
    argList <- expressionListP
    return $ \expr -> EMethodApp pos expr idt argList

typeP :: Parser Type
typeP =
  label "Type"
    $   try intArrP
    <|> (TInt <$ symbol "int")
    <|> (TBool <$ symbol "boolean")
    <|> (TClass <$> identifierP)
  where intArrP = symbol "int" *> symbol "[" *> symbol "]" $> TIntArray

typeIdtPairP :: Parser (Type, Identifier)
typeIdtPairP = do
  t   <- typeP
  idt <- identifierP
  return (t, idt)

varDecP :: Parser VarDec
varDecP = label "Variable Declaration" $ do
  pos      <- getSourcePos
  (t, idt) <- typeIdtPairP
  semiP
  return $ VarDec pos t idt

statementP :: Parser Statement
statementP =
  label "Statement"
    $   printP
    <|> try assignP
    <|> try arrayAssignP
    <|> ifP
    <|> whileP
    <|> blockStmtP
 where
  ifP = do
    pos <- getSourcePos
    symbol "if"
    predicate  <- paren expressionP
    bodyClause <- statementP
    symbol "else"
    SIf pos predicate bodyClause <$> statementP
  whileP = do
    pos <- getSourcePos
    symbol "while"
    predicate <- paren expressionP
    SWhile pos predicate <$> statementP
  printP = do
    pos <- getSourcePos
    symbol "System.out.println"
    expr <- paren expressionP
    semiP
    return $ SPrint pos expr
  assignP = do
    pos <- getSourcePos
    idt <- identifierP
    SAssignId pos idt <$> assignTail
  arrayAssignP = do
    pos <- getSourcePos
    idt <- identifierP
    idx <- bracket expressionP
    SAssignArr pos idt idx <$> assignTail
  blockStmtP = SBlock <$> getSourcePos <*> (block . many) statementP
  assignTail = do
    symbol "="
    expr <- expressionP
    semiP
    return expr

methodDecP :: Parser MethodDec
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


mainClassDecP :: Parser MainClass
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


classDecP :: Parser ClassDec
classDecP = label "Class Declaration" $ do
  symbol "class"
  clsName    <- identifierP
  superClass <- optional $ try $ symbol "extends" >> identifierP
  symbol "{"
  vars <- many varDecP
  mets <- many methodDecP
  symbol "}"
  return $ ClassDec clsName superClass vars mets


miniJavaP :: Parser MiniJavaAST
miniJavaP = do
  sc
  mc   <- mainClassDecP
  clss <- many classDecP
  eof
  return $ MiniJavaAST mc clss
