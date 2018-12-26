{-# LANGUAGE OverloadedStrings #-}
module MiniJava
  ( postMiniJavaJSON
  , Config(..)
  )
where

import           MiniJava.JSON                 as J
import           MiniJava.Config
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T

postMiniJavaJSON :: T.Text -> Config -> IO BS.ByteString
postMiniJavaJSON program cfg = do
  json <- J.postToJSON program cfg
  return json
