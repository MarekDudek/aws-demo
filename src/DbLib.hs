module DbLib where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Data.Word
import Data.Text
import Control.Monad

awsDemoConnection :: IO Connection
awsDemoConnection = connect defaultConnectInfo {
    connectDatabase = "aws-demo",
    connectUser     = "aws-demo-user",
    connectPassword = "aws-demo-password",
    connectHost = "localhost",
    connectPort = 5432 :: Word16
  }


awsDemoConnectionString = "postgresql://aws-demo-user:aws-demo-password@localhost:5432/aws-demo" 

testConnectionByAddition :: IO Int
testConnectionByAddition = do
  conn <- connectPostgreSQL awsDemoConnectionString
  [Only i] <- query_ conn "select 2 + 2"
  return i

inMonadicStyle :: IO ()
inMonadicStyle = do
  conn <- awsDemoConnection
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 3" :: IO [Only Int] )


data Present = Present { presentName :: Text } deriving Show

data Location = Location { locLat  :: Double
                         , locLong :: Double 
                         } deriving Show

data Child = Child { childName :: Text
                   , childLocation :: Location
                   } deriving Show

instance FromRow Present where
  fromRow = Present <$> field

instance FromRow Child where
  fromRow = Child <$> field <*> liftM2 Location field field

allChildren :: Connection -> IO [Child]
allChildren c = query_ c "SELECT name, loc_lat, loc_long FROM child"

allPresents :: Connection -> IO [Present]
allPresents c = query_ c "SELECT name FROM present"
