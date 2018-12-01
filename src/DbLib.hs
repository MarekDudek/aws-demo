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
