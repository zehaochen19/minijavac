module Main where

import           Web.Scotty                     ( middleware
                                                , scotty
                                                )
import           Control.Monad                  ( join )
import           Control.Applicative            ( (<$>) )
import           Controllers.Home               ( index
                                                , postJava
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Network.Wai.Middleware.Static  ( addBase
                                                , noDots
                                                , staticPolicy
                                                , (>->)
                                                )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
    port <- fromMaybe 3000 . join . fmap readMaybe <$> lookupEnv "PORT"
    scotty port $ do
        middleware $ staticPolicy (noDots >-> addBase "static") -- for static sources
        middleware logStdoutDev
        index >> postJava
