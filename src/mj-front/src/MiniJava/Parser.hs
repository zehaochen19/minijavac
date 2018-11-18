module MiniJava.Parser where

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
