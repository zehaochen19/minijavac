module MiniJava where


import           MiniJava.Symbol               as S
import           MiniJava.Parser               as P
import           MiniJava.TypeCheck            as TC
import           MiniJava.JSON                 as J
import qualified Data.ByteString.Lazy          as BS

compileMiniJavaJSON :: FilePath -> FilePath -> IO ()
compileMiniJavaJSON src out = do
    json <- J.compileToJSON src
    BS.writeFile out json
