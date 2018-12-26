{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Controllers.Home
  ( index
  , postJava
  )
where
import           Views.Home                     ( homeView )
import           Web.Scotty                     ( ScottyM
                                                , get
                                                , post
                                                , html
                                                , json
                                                , file
                                                )
import           Web.Scotty.Trans               ( jsonData
                                                , param
                                                )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics
import           MiniJava.Config
import           MiniJava.JSON
import qualified MiniJava.Parser               as P



index = get "/" $ file "static/index.html"
{-
  json api

data Post = Post
  { postId    :: Int
  , postTitle :: String } deriving Generic

instance ToJSON Post

post :: ScottyM()
post = get "/post" $ json $ Post 1 "Yello world"
-}


postJava = post "/java" $ do
  javaProgram <- param "java"
  let result = P.parseFromText javaProgram "program.java" (Config False)
  json result
  --case result of 
  --Left err -> json err
  --Right ast -> json ast
