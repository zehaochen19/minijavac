module MiniJava.Config where

newtype Config = Config
    {  fixSemi :: Bool
    } deriving (Eq, Show)
