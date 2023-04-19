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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use >=>" #-}

module Servers ( initialize
               , updateStatus
               , getRegion
               , Server(Server)
               ) where
    
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           System.Directory (doesFileExist)
import           System.IO

import           GHC.Generics
import           GHC.IO.Windows.Handle (Io)

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader

import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hGetLine)
import           Data.Time (UTCTime, getCurrentTime, utctDayTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.ByteString.Lazy as BL (ByteString, split,readFile)
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)

import qualified SteamApi as SA (ServerStatus (ServerStatus, ServerStatusLong))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server json
    addr         T.Text
    port         T.Text
    name         T.Text
    map          T.Text             default='empty'
    region       T.Text
    players      Int                default=0
    capacity     Int                default=8
    queue        Int                default=0
    start        (Maybe UTCTime)    default=CURRENT_TIME
    UniqueServer addr port
    deriving Show Generic
|]

initialize serverlist db = 
    doesFileExist db >>= \case

        True -> runSqlite (T.pack db) $ selectList [][] >>= return . map 
                    (\(Entity _ s) -> (serverAddr s, serverPort s))

        False -> doesFileExist serverlist                        >>= \exists ->
                    if exists then openFile serverlist ReadMode  >>= \h ->
                        parseLines h                             >>= \lines ->

                        runSqlite (T.pack db) $ do
                            runMigration migrateAll 
                            forM lines $ \l -> f l  -- '|'

                    else return $ error "couldn't initialize db from serverlist"

        where f [reg, name, addr, port] = do insert $ Server addr port name "empty" reg 0 8 0 Nothing 
                                             return (addr, port)

              parseLines h = 
                    TIO.hGetLine h >>= \line ->
                    if line == "" then do hClose h; return []
                    else do 
                        lines <- parseLines h
                        return $ T.split (=='|') line : lines 

{-
initialize serverlist db
    | doesFileExist serverlist == return True
    = BL.readFile serverlist  >>= \sl ->

        let f [reg, name, addr, port] = do insert $ Server addr port name "" 0 8 0 Nothing reg
                                           return $ addr <> ":" <> port
            ss = BL.split 10 sl -- '\n'
            
        in runSqlite (T.pack db) $ do 
            runMigration migrateAll
            forM ss $ \s -> f (BL.split 124 s)  -- '|'
    
initialize _ _ = return $ error "couldn't initialize servers db"
-}
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


-- | Get a list of all the servers by their unique identifier
getAll :: FilePath -> IO [Unique Server]
getAll dbPath = runSqlite (pack dbPath) $ do
    servers <- selectList [][] -- get all the IDs and values from the DB
    return (makeUnique servers) -- but return only their keys
    where makeUnique = map (\(Entity _ serv) -> UniqueServer (serverAddr serv) (serverPort serv)) 

getAllPair :: FilePath -> IO [(String, String)]
getAllPair db = runSqlite (pack db) $ do
    servers <- selectList [][]
    return $ go servers

    where go ((Entity _ s):ss) = (serverAddr s, serverPort s) : go ss
          go []                = []

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
-}

getRegion db addr port = 
    runSqlite (T.pack db) $ do
        Just (Entity id server) <- getBy $ UniqueServer addr port
        return $ serverRegion server

updateStatus db addr port (SA.ServerStatus map players capacity) = 
    runSqlite (T.pack db) $ do 
        Just (Entity id server) <- getBy $ UniqueServer addr port

        let updates = [ map      /= serverMap server
                      , players  /= serverPlayers server
                      , capacity /= serverCapacity server
                      ]

        when (updates !! 0) $ do
            time <- liftIO getCurrentTime
            update id [ServerMap =. map]
            update id [ServerStart =. Just time]
        when (updates !! 1) $ update id [ServerPlayers =. players]
        when (updates !! 2) $ update id [ServerCapacity =. capacity]

        return updates