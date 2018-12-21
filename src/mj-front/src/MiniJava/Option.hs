module MiniJava.Option where

import qualified Control.Monad.Reader          as R

newtype CompilerOption = CompilerOption
    {  fixSemi :: Bool
    }

type OptionParse m a = R.ReaderT CompilerOption m a
