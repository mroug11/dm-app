{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use >=>" #-}

module Servers ( initialize
          , updateServer
          , getAll
          , getAllByRegion
          , getAllByRegionParsed
          , getRegion
          , getAllPair
          , Server(Server)
          , Unique (UniqueServer)
          ) where
    
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           System.Directory (doesFileExist)

import           GHC.Generics
import           GHC.IO.Windows.Handle (Io)

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader

import           Data.Text as T (Text, pack, unpack)
import           Data.Time (UTCTime, getCurrentTime, utctDayTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.ByteString.Lazy as BS (split, ByteString, readFile)

import           SteamApi (ServerStatus (ServerStatus, ServerStatusLong))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server json
    addr         BS.ByteString
    port         BS.ByteString
    name         BS.ByteString
    map          BS.ByteString      default='dummy'
    players      Int                default=0
    capacity     Int                default=8
    queue        Int                default=0
    start        (Maybe UTCTime)    default=CURRENT_TIME
    region       BS.ByteString
    UniqueServer addr port

    deriving Show Generic
|]

initialize _ db
    | doesFileExist db == return True
    = runSqlite (T.pack db) $ selectList [][] >>= return . map 
        (\(Entity _ s) -> serverAddr s <> ":" <> serverPort s)

initialize serverlist db
    | doesFileExist serverlist == return True
    = BS.readFile serverlist  >>= \sl ->

        let f [reg, name, addr, port] = do insert $ Server addr port name "" 0 8 0 Nothing reg
                                           return $ addr <> ":" <> port
            ss = BS.split 10 sl -- '\n'
            
        in runSqlite (T.pack db) $ do 
                runMigration migrateAll
                forM ss $ \s -> f (BS.split 124 s)  -- '|'
    
initialize _ _ = return $ error "couldn't initialize servers db"

{-
dbPath
    unless dbExists $ runSqlite (pack dbPath) $ do 
           liftIO $ putStrLn "running migration on new db file"
           runMigration migrateAll
           servers <- liftIO $ openFile serversFile ReadMode >>= readServers
           forM_ servers $ \server -> do insert server
        
        where   
            readServers fromHandle = do
                contents <- hGetContents fromHandle
                t <- getCurrentTime
                return (map (go t) (splitStr '\n' contents))
            
            go t str =  let row = splitStr '|' str
                            (region, name, address, port) = (head row, row !! 1, row !! 2, row !! 3)
                        in Server region name address port "dummy" 0 8 0 t


splitStr :: Char -> String -> [String]
splitStr delim str             = splitStr' delim str ""
splitStr' _ []           right = [right]
splitStr' delim (l:left) right = if l == delim then right : splitStr' delim left ""
                                               else splitStr' delim left (right ++ [l])
-}

-- | Get a list of all the servers by their unique identifier
getAll :: FilePath -> IO [Unique Server]
getAll dbPath = runSqlite (pack dbPath) $ do
    servers <- selectList [][] -- get all the IDs and values from the DB
    return (makeUnique servers) -- but return only their keys
    where makeUnique = map (\(Entity _ serv) -> UniqueServer (serverAddress serv) (serverPort serv)) 

getAllPair :: FilePath -> IO [(String, String)]
getAllPair db = runSqlite (pack db) $ do
    servers <- selectList [][]
    return $ go servers

    where go ((Entity _ s):ss) = (serverAddress s, serverPort s) : go ss
          go []                = []

getRegion :: FilePath -> String -> String -> IO String
getRegion db addr port = runSqlite (pack db) $ do
    Just (Entity id info) <- getBy $ UniqueServer addr port
    return $ serverRegion info

-- | Get all servers in a specific Region
getAllByRegion :: FilePath -> String -> IO [Server]
getAllByRegion dbPath region = runSqlite (pack dbPath) $ do 
    servers <- selectList [ServerRegion ==. region] []
    return (map (\(Entity _ server) -> server) servers)

getAllByRegionParsed :: FilePath -> String -> IO [ServerStatus]
getAllByRegionParsed db reg = do
    servers <- getAllByRegion reg db
    return $ map serverToStatus servers

    where serverToStatus :: Server -> ServerStatus
          serverToStatus s = ServerStatusLong (serverAddress s) (serverPort s) (serverName s) (serverMap s) (serverPlayers s) (serverCapacity s) (serverQueue s) (utcToISO8601 (serverStart s))

          utcToISO8601 :: UTCTime -> String
          utcToISO8601 utc = utcToYMD utc ++ "T" ++ utcToHMS utc ++ ".000Z"

          utcToYMD :: UTCTime -> String
          utcToYMD = formatTime defaultTimeLocale  "%Y-%m-%d"

          utcToHMS :: UTCTime -> String
          utcToHMS = formatTime defaultTimeLocale  "%H:%M:%S"

-- TODO: add exception handling
{-| Update server status in the database.
    return: True if the servers status was updated, false if nothing needed to be updated
-}
updateServer :: FilePath -> String -> String -> ServerStatus -> IO [Bool]
updateServer dbPath address port (ServerStatus map players capacity queue) = runSqlite (pack dbPath) $ do
    Just (Entity id info) <- getBy $ UniqueServer address port

    let updates = [ map      /= serverMap info
                  , players  /= serverPlayers info
                  , capacity /= serverCapacity info
                  , queue    /= serverQueue info
                  ]

    when (updates !! 0) $ do 
        liftIO $ print "updating map"
        update id [ServerMap =. map]
        time <- liftIO getCurrentTime
        update id [ServerStart =. time]
    when (updates !! 1) $ do update id [ServerPlayers =. players]
    when (updates !! 2) $ do update id [ServerCapacity =. capacity]
    when (updates !! 3) $ do update id [ServerQueue =. queue]

    return updates