{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module MiniJava.JSON where

import           Data.Aeson
import           MiniJava.Symbol               as S
import           MiniJava.Config
import           Text.Megaparsec
import           Data.Void
import qualified Data.ByteString.Lazy          as BS
import           MiniJava.Parser               as P
import           MiniJava.TypeCheck            as TC
import qualified Text.Megaparsec.Error         as ME
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

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

instance ToJSON (ParseErrorBundle T.Text Void)
    where
  toJSON err = object ["errors" .= ME.errorBundlePretty err]

instance ToJSON SourcePos where
  toJSON (SourcePos _ srcLine srcCol) =
    object ["line" .= unPos srcLine, "column" .= unPos srcCol]

compileToJSON :: FilePath -> Config -> IO BS.ByteString
compileToJSON src cfg = do
  result <- P.parseFromSrc src cfg
  case result of
    Left err -> do
      putStrLn $ ME.errorBundlePretty err
      return $ encode err
    Right ast -> do
      let checked = typeCheck ast
      case checked of
        []       -> return $ encode ast
        tcErrors -> do
          mapM_ TIO.putStrLn tcErrors
          return $ encode $ object ["errors" .= tcErrors]



postToJSON :: T.Text -> Config -> IO BS.ByteString
postToJSON program cfg = do
  result <- P.parseFromText program cfg
  case result of
    Left err -> do
      putStrLn $ ME.errorBundlePretty err
      return $ encode err
    Right ast -> do
      let checked = typeCheck ast
      case checked of
        []       -> return $ encode ast
        tcErrors -> do
          mapM_ TIO.putStrLn tcErrors
          return $ encode $ object ["errors" .= tcErrors]