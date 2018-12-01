module DbLib where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Word
import Data.Text
import Control.Monad
import GHC.Int

awsDemoConnection :: IO Connection
awsDemoConnection = connect defaultConnectInfo {
    connectDatabase = "aws-demo",
    connectUser     = "aws-demo-user",
    connectPassword = "aws-demo-password",
    connectHost = "localhost",
    connectPort = 5432 :: Word16
  }


awsDemoConnectionString = "postgresql://aws-demo-user:aws-demo-password@localhost:5432/aws-demo" 

testConnection :: IO Bool
testConnection = do
  conn <- connectPostgreSQL awsDemoConnectionString
  sum <- runAdditionQuery conn 2 3
  return $ sum == 5

inMonadicStyle :: IO ()
inMonadicStyle = do
  conn <- awsDemoConnection
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 3" :: IO [Only Int] )

runAdditionQuery :: Connection -> Int -> Int -> IO Int
runAdditionQuery c a b = do
  [Only sum] <- query c additionQuery $ (a, b)
  return sum

additionQuery :: Query
additionQuery = "SELECT ? + ?" 

data Present = Present { presentId :: Int
                       , presentName :: Text } deriving Show

data Location = Location { locLat  :: Double
                         , locLong :: Double 
                         } deriving Show

data Child = Child { childName :: Text
                   , childLocation :: Location
                   } deriving Show

instance FromRow Present where
  fromRow = Present <$> field <*> field

instance FromRow Child where
  fromRow = Child <$> field <*> liftM2 Location field field

instance ToRow Present where
  toRow p = [toField (presentId p), toField (presentName p)]

allChildren :: Connection -> IO [Child]
allChildren c = query_ c "SELECT name, loc_lat, loc_long FROM child"

allPresents :: Connection -> IO [Present]
allPresents c = query_ c "SELECT id, name FROM present"

insertPresents :: Connection -> IO ()
insertPresents c = do
  insertPresentQuery c (Present 1 "pony")
  insertPresentQuery c (Present 2 "doll")
  insertPresentQuery c (Present 3 "ball")
  return ()

insertPresentQuery :: Connection -> Present -> IO GHC.Int.Int64
insertPresentQuery c p = do
  execute c "INSERT INTO present (id, name) VALUES (?, ?)" $ p
