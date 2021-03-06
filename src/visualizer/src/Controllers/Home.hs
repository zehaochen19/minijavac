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
                                                , object
                                                , (.=)
                                                , Value
                                                )
import           GHC.Generics
import           MiniJava.Config
import           MiniJava.JSON
import qualified MiniJava.Parser               as P
import qualified MiniJava.TypeCheck            as TC
import qualified Data.Text                     as T
import           MiniJava.Symbol               as S



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
  case result of
    Left  err -> json result
    Right ast -> do
      let checked = TC.typeCheck ast
      case checked of
        [] -> json result
        tcErrors ->
          json ((Left $ object ["errors" .= tcErrors]) :: Either Value Value)

  --json result
  --case result of 
  --Left err -> json err
  --Right ast -> json ast
