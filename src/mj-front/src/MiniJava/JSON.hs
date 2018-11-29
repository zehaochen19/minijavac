{-# LANGUAGE FlexibleInstances #-}
module MiniJava.JSON where

import           Data.Aeson
import           MiniJava.Symbol               as S
import           Text.Megaparsec.Error
import           Data.Void
import           Data.ByteString.Lazy
import           MiniJava.Parser               as P
import           MiniJava.TypeCheck            as TC
import qualified Text.Megaparsec.Error         as ME

instance ToJSON Identifier

instance ToJSON Type

instance ToJSON Expression

instance ToJSON BinOp

instance ToJSON Statement

instance ToJSON MainClass

instance ToJSON ClassDec

instance ToJSON VarDec

instance ToJSON MethodDec

instance ToJSON MiniJavaAST

instance ToJSON (ParseError Char Void)
    where toJSON err = object [ "errors" .= ME.parseErrorPretty err]


compileToJSON :: FilePath -> IO ByteString
compileToJSON src = do
  result <- P.parseFromSrc src
  case result of
    Left  err -> return $ encode err
    Right ast -> do
      let checked = typeCheck ast
      return $ case checked of
        []       -> encode ast
        tcErrors -> encode $ object ["errors" .= tcErrors]
