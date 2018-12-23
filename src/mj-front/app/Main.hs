module Main where

import           MiniJava
import           MiniJava.Config
import           System.Environment
import           System.Exit



main :: IO ()
main = do
  putStrLn "minijavac"
  args <- getArgs
  case args of
    [src, out]          -> compileMiniJavaJSON src out (Config False)
    [src, out, "--fix"] -> compileMiniJavaJSON src out (Config True)
    _                   -> do
      putStrLn "Usage: minijavac src.java output.json (--fix)"
      exitFailure
