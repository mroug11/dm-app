{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Redundant >>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use if" #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay, ThreadId)
import           Control.Concurrent.Chan
import           Control.Monad

import qualified Data.ByteString.Lazy as BS (ByteString, split)
import qualified Data.Text as T

import           Configuration.Utils (runWithConfiguration)

import           Control.Exception as E

import           Network.Wai.Handler.Warp as Warp --(runSettings, setPort, setServerName, defaultSettings)
import           Network.Wai.Handler.WarpTLS as WarpTLS (runTLS, tlsSettings, TLSSettings)
import           Network.Socket

import           App (api, app, Opts(Opts))
import           Settings
import qualified Servers as S (initialize, updateStatus, getRegion)
import qualified Users as U (initialize)
import           SteamApi as SA (ServerStatus(ServerStatus), getServerStatus)
import           Types 

main :: IO () 
main = runWithConfiguration runtimeInfo $ \conf -> 

    let initialize c =

            S.initialize (_serverlist c) (_dbPath c)    >>= \connList ->
            U.initialize (_userdb c) connList           >>= \numThreads ->
            mkUpdateChannel                             >>= \updateChan ->

            forM connList (\conn -> do
                threadDelay (150000000 `div` numThreads)
                writer <- dupChan updateChan
                forkIO . forever $
                    tracker conn writer 150000000 1)    >>= \threads -> 
                    
            return (updateChan, threads)                              

            where forever :: IO () -> IO ()
                  forever f = f >> forever f
                    
                  mkUpdateChannel :: IO (Chan Update)
                  mkUpdateChannel = newChan

                  tracker conn chan interval n =  
                        fetchStatus conn                >>= 
                        applyUpdate (_dbPath c) conn    >>= 
                        relayUpdate chan                >>
                        varDelay interval n 

        run c (chan, threads) =

            (if _isHttps c 
                then WarpTLS.runTLS (tlsSettings (_cert . _tlsOpts $ c) (_key . _tlsOpts $ c)) 
                else Warp.runSettings) serverSettings 
                
                (app appSettings)

            where serverSettings = setTimeout (60*10) $ 
                                   setServerName "" $ 
                                   setPort (_port c) 
                                   defaultSettings

                  appSettings = Opts (_staticDir c) (_dbPath c) (_userdb c) chan

    in E.bracket (initialize conf) {-TODO-}return (run conf)


fetchStatus (addr, port) = SA.getServerStatus (T.unpack addr) (T.unpack port)

applyUpdate db (addr, port) ss@(SA.ServerStatus map players capacity) = do
    updates <- S.updateStatus db addr port ss
    region  <- S.getRegion db addr port

    case or updates of
        False -> return NoUpdate
        True  -> return (Header region (StatusUpdate (f (head updates) map) 
                                                     (f (updates !! 1) players) 
                                                     (f (updates !! 2) capacity)))

    where f update newVal = if update then Just newVal else Nothing

relayUpdate _    NoUpdate = return ()
relayUpdate chan update   = writeChan chan update

varDelay interval n = threadDelay interval