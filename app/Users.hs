{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE InstanceSigs               #-}

module Users where
    
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time (UTCTime, getCurrentTime, secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Directory (doesFileExist)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless)
import Users (User(userServers))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    token       String
    servers     (M.Map T.Text Bool) 
    threshold   Int                     default=4
    queued      Bool                    default=false
    lastRenew   UTCTime
    UniqueToken token
    deriving    Show
|]

initialize db slist = do
    file <- doesFileExist db
    unless file $ migrateUsers db
    let defaultServerList = zip (flatten slist) (repeat True)

    runSqlite (T.pack db) $ insert (User "default" (M.fromList defaultServerList) 0 False (posixSecondsToUTCTime 0))

    where flatten ((a,b):xs) = T.pack (a ++ ":" ++ b) : flatten xs 
          flatten []         = []

migrateUsers db = runSqlite (T.pack db) $ runMigration migrateAll

addUser db token = runSqlite db $ do
    t <- liftIO getCurrentTime
    (Just (Entity _ def)) <- selectFirst[UserToken ==. "default"][]
    insert (User token (userServers def) 4 False t)

remUser db token = runSqlite db $ deleteBy (UniqueToken token)

remExpiredUsers db time = do
    t <- getCurrentTime
    let expired = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds t - secondsToNominalDiffTime time
    runSqlite db $ do
        deleteWhere [UserLastRenew <=. expired]

renewUser db token = runSqlite db $ do
    user <- selectFirst [UserToken ==. token] []
    case user of 
        (Just (Entity userKey _)) -> do
                   t <- liftIO getCurrentTime
                   update userKey [UserLastRenew =. t]
                   return True
        Nothing -> return False

readyToJoin db numPlayers (addr, port) = runSqlite db $ do
    users <- selectList [UserQueued ==. True, UserThreshold >=. numPlayers] []
    let server = T.pack $ addr ++ ":" ++ show port
    return $ foldr (isServer server) 0 users

    where  
        isServer s (Entity _ (User _ m _ _ _)) acc = if m M.! s then acc + 1 else acc
