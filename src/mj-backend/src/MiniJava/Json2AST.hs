{-# LANGUAGE FlexibleInstances #-}
module MiniJava.Json2AST where

import           Data.Aeson
import           MiniJava.Symbol               as S
import           Text.Megaparsec.Error
import           Data.Void
import           Data.ByteString.Lazy        as  B
import qualified Text.Megaparsec.Error         as ME


instance FromJSON Identifier

instance FromJSON Type

instance FromJSON Expression

instance FromJSON BinOp

instance FromJSON Statement

instance FromJSON MainClass

instance FromJSON ClassDec

instance FromJSON VarDec

instance FromJSON MethodDec

instance FromJSON MiniJavaAST 



astFromSrc :: FilePath -> IO ()
astFromSrc src = do
  astJson <- B.readFile src
  print astJson
 --- return $ decode astJson :: MiniJavaAST 
  
  