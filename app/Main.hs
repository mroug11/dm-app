{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import Configuration.Utils (runWithConfiguration)
import Network.Wai.Handler.Warp as Warp --(runSettings, setPort, setServerName, defaultSettings)
import Network.Wai.Handler.WarpTLS as WarpTLS (runTLS, tlsSettings, TLSSettings)
import Network.Socket
import Control.Exception as E

import App (api, app, Opts(Opts))
import Render (apiToJS)
import Settings
import Stream (trackServer, sendKeepAlive, listener)
import Db (initialize, getAll, Unique(UniqueServer), getAllPair)
import qualified Users (initialize)

main :: IO () 
main = runWithConfiguration runtimeInfo $ \conf -> startServer conf
    where 
        startServer c = E.bracket (initialize c) {-TODO-}return (runServer c)
    
        initialize c = do
            -- initialize the database
            Db.initialize (_serverlist c) (_dbPath c)
            uniqueIds <- getAll (_dbPath c)

            -- initialize users database
            serverPairs <- Db.getAllPair (_dbPath c)
            Users.initialize (_userdb c) serverPairs

            -- create a stream source for server updates
            listenC <- Stream.listener

            -- start a tracker per each server in a new user thread, spaced by linear intervals
            forM_ uniqueIds $ \(Db.UniqueServer addr port) -> do
                trackC <- dupChan listenC
                -- track a server in 15 second intervals
                forkIO $ trackServer (_dbPath c) addr port (show $ _localport c) 15000000 trackC 
                threadDelay (15000000 `div` length uniqueIds){--}

            -- send keepalive pings every 10 minutes
            --forkIO $ sendKeepAlive listenC (60000000*10)

            -- turn the api into callable JS functions
            --apiToJS api (_staticDir c)

            -- open a socket on the server
            --let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
            --addr:_ <- getAddrInfo (Just hints) (Just "localhost") (Just  (show $ _port c) )
            --sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            --bind sock (addrAddress addr)

            -- run options
            return $ Opts (_staticDir c) (_dbPath c) (_userdb c) listenC

        runServer c runOpts = do
            let serverSettings = setTimeout (60*10) $ setServerName "" $ setPort (_port c) defaultSettings
            let appSettings = app runOpts

            (if _isHttps c 
                then WarpTLS.runTLS (tlsSettings (_cert . _tlsOpts $ c) (_key . _tlsOpts $ c)) 
                else Warp.runSettings) 
                    serverSettings appSettings


