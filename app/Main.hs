{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use >=>" #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Monad

import           Configuration.Utils (runWithConfiguration)

import           Control.Exception as E

import           Network.Wai.Handler.Warp as Warp --(runSettings, setPort, setServerName, defaultSettings)
import           Network.Wai.Handler.WarpTLS as WarpTLS (runTLS, tlsSettings, TLSSettings)
import           Network.Socket

import           App (api, app, Opts(Opts))
import           Render (apiToJS)
import           Settings
import           Stream (trackServer, sendKeepAlive, listener)
import qualified Servers (initialize, getAll, Unique(UniqueServer), getAllPair)
import qualified Users (initialize)

main :: IO () 
main = runWithConfiguration runtimeInfo $ \conf -> 

    let initialize c =
            Servers.initialize (_serverlist c) (_dbPath c) >>= \connList ->
            Users.initialize (_userdb c) connList          >>= \numThreads ->

            --listenC <- Stream.listener

                {-forM_ uniqueIds $ \(Db.UniqueServer addr port) -> do
                    trackC <- dupChan listenC
                    -- track a server in 15 second intervals
                    forkIO $ trackServer (_dbPath c) addr port (show $ _localport c) 15000000 trackC 
                    threadDelay (15000000 `div` length uniqueIds)-}

                --let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
                --addr:_ <- getAddrInfo (Just hints) (Just "localhost") (Just  (show $ _port c) )
                --sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                --bind sock (addrAddress addr)

            return $ Opts (_staticDir c) (_dbPath c) (_userdb c) listenC

        run c runOpts = do
            let serverSettings = setTimeout (60*10) $ setServerName "" $ setPort (_port c) defaultSettings
            let appSettings = app runOpts

            (if _isHttps c 
                then WarpTLS.runTLS (tlsSettings (_cert . _tlsOpts $ c) (_key . _tlsOpts $ c)) 
                else Warp.runSettings) 
                    serverSettings appSettings

    in E.bracket (initialize conf) {-TODO-}return (run conf)


