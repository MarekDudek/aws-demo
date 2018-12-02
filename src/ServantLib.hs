{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServantLib where

import Data.Text
import Data.Time 
import Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}

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
