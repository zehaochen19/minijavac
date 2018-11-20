module MiniJava.TypeCheck.Util where

import Control.Lens
import Data.Text
import MiniJava.TypeCheck.Type

addError :: Monad m => Text -> TC m ()
addError msg = errors %= (:) msg
