{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import Configuration.Utils (runWithConfiguration)
import Network.Wai.Handler.Warp as Warp --(runSettings, setPort, setServerName, defaultSettings)
import Network.Wai.Handler.WarpTLS as WarpTLS (runTLS, tlsSettings, TLSSettings)
import Network.Socket
import Control.Exception as E

import App (api, app)
import Render (apiToJS)
import Settings
import Stream (trackServer, sendKeepAlive, listener)
import Api (ServerUpdate(ServerUpdate,ServerUpdateKeepalive))
import Db (initialize, getAll, Unique(UniqueServer))
import Data.Aeson.Key (toString)

main :: IO () 
main = runWithConfiguration runtimeInfo $ \conf -> startServer conf
    where 
        startServer c = E.bracket (initialize c) ({-TODO-}return) (runServer c)
    
        initialize c = do
            -- initialize the database
            Db.initialize (_serverlist c) (_dbPath c)
            uniqueIds <- getAll (_dbPath c)

            -- create a stream source for server updates
            listenC <- Stream.listener

            -- start a tracker per each server in a new user thread, spaced by linear intervals
            forM_ uniqueIds $ \(Db.UniqueServer addr port) -> do
                trackC <- dupChan listenC
                forkIO $ trackServer (_dbPath c) addr port (show $ _localport c) 15000000 trackC -- track a server in 15 second intervals
                threadDelay (15000000 `div` length uniqueIds){--}

            -- send keepalive pings every 10 minutes
            forkIO $ sendKeepAlive listenC (60000000*10)

            -- turn the api into callable JS functions
            --apiToJS api (_staticDir c)

            -- open a socket on the server
            --let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
            --addr:_ <- getAddrInfo (Just hints) (Just "localhost") (Just  (show $ _port c) )
            --sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            --bind sock (addrAddress addr)

            return (_staticDir c, _dbPath c, listenC) -- run options

        runServer c (static, db, listener) = do
            let serverSettings = setHTTP2Disabled $ setServerName "" $ setPort (_port c) defaultSettings
            let appSettings = (app (static, db, listener))

            (if _isHttps c 
                then WarpTLS.runTLS (tlsSettings (_cert . _tlsOpts $ c) (_key . _tlsOpts $ c)) 
                else Warp.runSettings) 
                    serverSettings appSettings


