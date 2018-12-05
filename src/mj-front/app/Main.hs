module Main where

import           MiniJava
import           System.Environment
import           System.Exit


main :: IO ()
main = do
  putStrLn "minijavac"
  args <- getArgs
  case args of
    src : out : _ -> compileMiniJavaJSON src out
    _             -> do
      putStrLn "Usage: minijavac src.java output.json"
      exitFailure

