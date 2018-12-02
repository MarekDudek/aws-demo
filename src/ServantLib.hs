{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ServantLib where

import Data.Text
import Data.Time 
import Servant.API

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: Day
} deriving (Eq, Show, Generic)

instance ToJSON User

type RootEndpoint = Get '[JSON] User

type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]

type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]

type UserAPI5 = "user" :> Capture "userId" Integer :> Get '[JSON] User
           :<|> "user" :> Capture "userid" Integer :> DeleteNoContent '[JSON] NoContent

type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UsersAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
            :<|> "users" :> Capture "userid" Integer
                         :> ReqBody '[JSON] User
                         :> Put '[JSON] User

type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] User

type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

type ProtectedAPI11 = UserAPI :<|> BasicAuth "my-realm" User :> UserAPI2

type UserAPI12 innerAPI = UserAPI :<|> "inner" :> innerAPI

type UserAPI12Alone = UserAPI12 EmptyAPI

type UserAPI13 = "users" :> Get '[JSON] [User] :<|> Raw


type UserAPI1 = "users" :> Get '[JSON] [User]


users1 :: [User]
users1 = 
  [ User "Issac Newtown"    372 "issac@newtown.co.uk" (fromGregorian 1683  3  1),
    User "Albert Einstein"  136 "ae@c2.org"           (fromGregorian 1905 12  1),
    User "Piotr Banach"     100 "pbanach@lwow.pl"     (fromGregorian 1928  1 31)
  ]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

runMain :: IO ()
runMain =  run 8080 app1
