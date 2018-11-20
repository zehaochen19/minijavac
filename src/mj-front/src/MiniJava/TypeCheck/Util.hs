module MiniJava.TypeCheck.Util where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Text
import qualified Data.Text as T
import qualified MiniJava.Symbol as S
import MiniJava.TypeCheck.Type

addError :: Monad m => String -> TC m ()
addError msg = errors %= (:) (T.pack msg)
