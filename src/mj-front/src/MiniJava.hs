module MiniJava
  ( compileMiniJavaJSON
  )
where

import           MiniJava.JSON                 as J
import           MiniJava.Config
import qualified Data.ByteString.Lazy          as BS

compileMiniJavaJSON :: FilePath -> FilePath -> Config -> IO ()
compileMiniJavaJSON src out cfg = do
  json <- J.compileToJSON src cfg
  BS.writeFile out json
