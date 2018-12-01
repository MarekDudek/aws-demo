module DbLib where

import Database.PostgreSQL.Simple
import Data.Word

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
