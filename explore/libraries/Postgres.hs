{-# LANGUAGE OverloadedStrings #-}

module Postgres where


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad
import Control.Applicative

import Data.Text

main = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "config"
  }

  xs <- allIds conn
  forM_ xs $ \id -> putStrLn $ show (id :: Name)

--  --select * from users where first_name in ('Anna', 'Boris', 'Carla')
--  xs <- query_ conn "select id from dnsrecords"
--  forM_ xs $ \id ->
--    putStrLn $ show (id :: Int)
--    --putStrLn $ Text.unpack id ++ " is " ++ show (age :: Int)

--  --putStrLn "2 + 2"
--  --mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )
--  --mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )


allIds :: FromRow r => Connection -> IO [r]
allIds c = query_ c "SELECT zone from dnsforwardzones"


data Name = Name { itemName :: Text }
    deriving Show

instance FromRow Name where
    fromRow = Name <$> field
