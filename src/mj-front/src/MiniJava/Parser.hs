module MiniJava.Parser where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text as T
import Data.Void
import MiniJava.Symbol
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
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
semiP = symbol ";"

dotP :: Parser Text
dotP = symbol "."

commaP :: Parser Text
commaP = symbol ","

-- Reserved key words
reversed :: S.Set Text
reversed =
  S.fromList
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
    ]

-- Identifiers should start with [0..] underscores and an alphabet
-- and shouldn't be a reserved keyword
identifierP :: Parser Identifier
identifierP = (lexeme . try) (p >>= check) <?> "Ideintifier"
  where
    p =
      fmap T.pack $
      (\us x xs -> us ++ x : xs) <$> many (char '_') <*> letterChar <*>
      many (char '_' <|> alphaNumChar)
    check x =
      if x `S.member` reversed
        then fail $ "keyword" ++ show x ++ " cannot be an identifier"
        else return $ Identifier x

expressionListP :: Parser [Expression]
expressionListP = paren $ sepBy expressionP commaP

expressionP :: Parser Expression
expressionP = makeExprParser basicExpressionP operatorP

-- Unambiguous parts of Expressions
basicExpressionP :: Parser Expression
basicExpressionP =
  label "Terminal Expression" $
  (ETrue <$ symbol "true") <|> (EFalse <$ symbol "false") <|>
  (EInt <$> integerP) <|>
  (EParen <$> paren expressionP) <|>
  (EId <$> identifierP) <|>
  (EThis <$ symbol "this") <|>
  try newObjectP <|>
  try newArrayP
  where
    newObjectP =
      ENewObj <$> do
        idt <- symbol "new" >> identifierP
        symbol "(" >> symbol ")"
        return idt
    newArrayP =
      ENewIntArr <$> (symbol "new" >> symbol "int" >> bracket expressionP)

-- Operators for Expressions
-- To handle left recursive situations, method application,
-- array indexing and array lenght are treated as operators
operatorP :: [[Operator Parser Expression]]
operatorP =
  [ [ Postfix (flip EArrayIndex <$> bracket expressionP)
    , Postfix arrayLenP
    , Postfix methodP
    ]
  , [Prefix (ENot <$ symbol "!")]
  , [InfixL (EBinary BMult <$ symbol "*")]
  , [ InfixL (EBinary BPlus <$ symbol "+")
    , InfixL (EBinary BMinus <$ symbol "-")
    ]
  , [InfixL (EBinary BLT <$ symbol "<")]
  , [InfixL (EBinary BAnd <$ symbol "&&")]
  ]
  where
    arrayLenP :: Parser (Expression -> Expression)
    arrayLenP =
      try $ do
        dotP >> symbol "length"
        return $ \expr -> EArrayLength expr
    methodP :: Parser (Expression -> Expression)
    methodP =
      try $ do
        dotP
        idt <- identifierP
        argList <- expressionListP
        return $ \expr -> EMethodApp expr idt argList

typeP :: Parser Type
typeP =
  try intArrP <|> (TInt <$ symbol "int") <|> (TBool <$ symbol "boolean") <|>
  (TClass <$> identifierP)
  where
    intArrP = symbol "int" *> symbol "[" *> symbol "]" $> TIntArray

typeIdtPairP :: Parser (Type, Identifier)
typeIdtPairP = do
  t <- typeP
  idt <- identifierP
  return (t, idt)

varDecP :: Parser VarDec
varDecP = do
  (t, idt) <- typeIdtPairP
  semiP
  return $ VarDec t idt

statementP :: Parser Statement
statementP =
  label "Statement" $
  printP <|> try assignP <|> try arrayAssignP <|> ifP <|> whileP <|> blockStmtP
  where
    ifP = do
      symbol "if"
      predicate <- paren expressionP
      bodyClause <- statementP
      symbol "else"
      SIf predicate bodyClause <$> statementP
    whileP = do
      symbol "while"
      predicate <- paren expressionP
      SWhile predicate <$> statementP
    printP = do
      symbol "System.out.println"
      expr <- paren expressionP
      semiP
      return $ SPrint expr
    assignP = do
      idt <- identifierP
      SAssignId idt <$> assignTail
    arrayAssignP = do
      idt <- identifierP
      idx <- bracket expressionP
      SAssignArr idt idx <$> assignTail
    blockStmtP = SBlock <$> (block . many) statementP
    assignTail = do
      symbol "="
      expr <- expressionP
      semiP
      return expr

methodDecP :: Parser MethodDec
methodDecP = do
  symbol "public"
  t <- typeP
  idt <- identifierP
  argList <- argListP
  symbol "{"
  vs <- many $ try varDecP
  ss <- many $ try statementP
  symbol "return"
  result <- expressionP
  semiP
  symbol "}"
  return $ MethodDec t idt argList vs ss result
  where
    argListP = paren $ sepBy typeIdtPairP commaP
