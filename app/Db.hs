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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

-- | Import as qualified
module Db ( initialize
          , updateServer
          , getAll
          , getAllByRegion
          , getAllByRegionParsed
          , getRegion
          , getAllPair
          , Server(Server)
          , Unique (UniqueServer)
          ) where
    
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Directory (doesFileExist)
import System.IO (openFile, hGetContents, IOMode (ReadMode))
import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Time (UTCTime, getCurrentTime, utctDayTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import SteamApi (ServerStatus (ServerStatus, ServerStatusLong))
import Control.Monad.Trans.Reader
import GHC.IO.Windows.Handle (Io)

-- | DB contains information about the DM servers. Static info is read from an on-disk file during startup.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server
    region String
    name String
    address String
    port String
    map String default='dummy'
    players Int default=0
    capacity Int default=8
    queued Int default=0
    started UTCTime default=CURRENT_TIME
    UniqueServer address port
    deriving Show Generic
Players
    ownerId ServerId
    name String
    connected UTCTime default=CURRENT_TIME
    UniquePlayer name ownerId
    deriving Show Generic
|]

splitStr :: Char -> String -> [String]
splitStr delim str             = splitStr' delim str ""
splitStr' _ []           right = [right]
splitStr' delim (l:left) right = if l == delim then right : splitStr' delim left ""
                                               else splitStr' delim left (right ++ [l])

-- | Conditionally initialize db from serversfile
initialize :: FilePath -> FilePath -> IO ()
initialize serversFile dbPath = do
    dbExists <- doesFileExist dbPath
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
          serverToStatus s = ServerStatusLong (serverAddress s) (serverPort s) (serverName s) (serverMap s) (serverPlayers s) (serverCapacity s) (serverQueued s) (utcToISO8601 (serverStarted s))

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
                  , queue    /= serverQueued info
                  ]

    when (updates !! 0) $ do 
        liftIO $ print "updating map"
        update id [ServerMap =. map]
        time <- liftIO getCurrentTime
        update id [ServerStarted =. time]
    when (updates !! 1) $ do update id [ServerPlayers =. players]
    when (updates !! 2) $ do update id [ServerCapacity =. capacity]
    when (updates !! 3) $ do update id [ServerQueued =. queue]

    return updates