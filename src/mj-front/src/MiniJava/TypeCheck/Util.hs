module MiniJava.TypeCheck.Util where

import Control.Lens
import Control.Monad
import qualified Data.Text as T
import qualified MiniJava.Symbol as S
import MiniJava.TypeCheck.Type

addError :: Monad m => String -> TC m ()
addError msg = errors %= (:) (T.pack msg)

-- Supresss adding error if any of the types is TBottom
typeError :: (Monad m, Show a) => S.Type -> S.Type -> a -> TC m ()
typeError expectedType actualType symbol =
  when (expectedType /= S.TBottom && actualType /= S.TBottom) $
  addError $
  "Cannot match expected type `" ++
  show expectedType ++
  "`\nwith actual type `" ++ show actualType ++ "`\n in `" ++ show symbol ++ "`"
