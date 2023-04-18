{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Redundant >>" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay, ThreadId)
import           Control.Concurrent.Chan
import           Control.Monad

import qualified Data.ByteString.Lazy as BS (ByteString)

import           Configuration.Utils (runWithConfiguration)

import           Control.Exception as E

import           Network.Wai.Handler.Warp as Warp --(runSettings, setPort, setServerName, defaultSettings)
import           Network.Wai.Handler.WarpTLS as WarpTLS (runTLS, tlsSettings, TLSSettings)
import           Network.Socket

import           App (api, app, Opts(Opts))
import           Settings
import qualified Servers as S (initialize)
import qualified Users as U (initialize)

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
                    tracker conn writer 150000000
            )                                           >>= \threads -> 
                    
            return (updateChan, threads)                              

            where forever :: IO () -> IO ()
                  forever f = f >> forever f
                    
                  mkUpdateChannel :: IO (Chan Update)
                  mkUpdateChannel = newChan

                  tracker conn chan interval =  
                        fetchStatus conn     >>= \status ->
                        updateWith status    >>= \diff ->
                        writeChan chan diff  >>
                        threadDelay interval 

        run c (chan, threads) = do

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


fetchStatus = error "..."
updateWith = error "..."

data Update = StatusUpdate String 
            | QueueUpdate String Int 
            | ServerUpdate String String Int