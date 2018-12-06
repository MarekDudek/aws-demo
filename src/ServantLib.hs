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
import Servant.HTML.Blaze

import qualified DbLib 
import Database.PostgreSQL.Simple

import Debug.Trace

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


type UserAPI14 = "users" :> Get '[JSON] [User]


users1 :: [User]
users1 = [ issac, albert, stefan ]

server1 :: Server UserAPI14
server1 = return users1

userAPI14 :: Proxy UserAPI14
userAPI14 = Proxy

app1 :: Application
app1 = serve userAPI14 server1

runMain1 :: IO ()
runMain1 =  run 8080 app1

type UserAPI15 = "users" :> Get '[JSON] [User]
            :<|> "albert" :> Get '[JSON] User
            :<|> "issac" :> Get '[JSON] User
            :<|> "stefan" :> Get '[JSON] User

issac :: User
issac  = User "Issac Newtown"    372 "issac@newtown.co.uk" (fromGregorian 1683  3  1)

albert :: User
albert = User "Albert Einstein"  136 "ae@c2.org"           (fromGregorian 1905 12  1)

stefan :: User
stefan = User "Stefan Banach"     100 "sbanach@lwow.pl"     (fromGregorian 1928  1 31)

server2 :: Server UserAPI15
server2 = return users1
     :<|> return albert
     :<|> return issac 
     :<|> return stefan 

userAPI15 :: Proxy UserAPI15
userAPI15 = Proxy

app2 :: Application
app2 = serve userAPI15 server2

runMain2 :: IO ()
runMain2 =  run 8080 app2


data Position = Position 
  { xCoord :: Int 
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo
instance FromJSON ClientInfo

data Email = Email 
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where from' = "great@company.com"
        to' = clientEmail c
        subject' = "Hey " ++ name ++ ", we miss you!"
        body' = "Hi " ++ name ++ ", \n\n" 
            ++ " Since you've recently turned " ++ show (clientAge c)
            ++ ", have you checked our latest "
            ++ Data.List.intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"
        name = clientName c

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email


server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing 
  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n
        
        marketing :: ClientInfo -> Handler Email
        marketing ci = return (emailForClient ci)

api :: Proxy API
api = Proxy

app3 :: Application
app3 = serve api server3

runMain3 :: IO ()
runMain3 = run 8080 app3


data HTMLBlaze

instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml


data Person = Person
  { firstName :: String
  , lastName :: String
  } deriving Generic

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)
  toHtmlRaw = toHtml

instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
    foldMap toHtml persons
  toHtmlRaw = toHtml

type PersonAPI = "persons" :> Get '[JSON] [Person]


people :: [Person]
people = 
  [ Person "Isaac" "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app4 :: Application
app4 = serve personAPI server4

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  conn <- liftIO (DbLib.awsDemoConnection)
  users <- liftIO (DbLib.selectUsersByFirstNameAndMinimumAge conn "Marek" 40)
  liftIO (print users)
  fileContent <- liftIO (readFile "res/myfile.txt")
  return $ FileContent fileContent

ioapi1 :: Proxy IOAPI1
ioapi1 = Proxy

app5 :: Application
app5 = serve ioapi1 server5

runMain5 :: IO ()
runMain5 = run 8080 app5

failingHandler :: Handler ()
failingHandler = throwError myerr
  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }

type FileAPI = "files" :> Capture "name" String :> Get '[JSON] FileContent

fileApi :: Proxy FileAPI
fileApi = Proxy

server6 :: Server FileAPI
server6 = file
  where
    file :: String -> Handler FileContent
    file name = do
      exists <- liftIO ( doesFileExist $ "res/" ++ name )
      if exists
        then liftIO ( readFile ("res/" ++ name) ) >>= return . FileContent
        else throwError err404 { errBody = "file does not exist" }

app6 :: Application
app6 = serve fileApi server6

runMain6 :: IO ()
runMain6 = run 8080 app6


type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert

type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert

type MyMaybeHeaderHandler = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader       albert

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "res"

app7 :: Application
app7 = serve staticAPI server7

runMain7 :: IO ()
runMain7 = run 8080 app7
