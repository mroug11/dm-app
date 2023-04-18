
module Stream ( listener, trackServer, sendKeepAlive ) where

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)

import Servers (updateServer, getRegion)
import App (ServerUpdate(ServerUpdate,ServerUpdateKeepalive))
import SteamApi (getServerStatus, ServerStatus(ServerStatus))

listener :: IO (Chan ServerUpdate)
listener = newChan

-- TODO: make tracking interval dynamic depending on number of players
-- | Query a server for status information in X microsecond intervals
trackServer ::  String -> -- ^ db path
                String -> -- ^ Server address
                String -> -- ^ Server port (ServiceName)
                String -> -- ^ Local port for sending streaming updates
                Int    -> -- ^ Interval between loops
                Chan ServerUpdate ->  -- ^ send server updates between threads (trackers to listener)
                IO ()
trackServer db addr port localport interval listen = do
    serverStatus <- getServerStatus addr port
    updated <- updateServer db addr port serverStatus
    
    when (or updated) $ do -- or updated
        reg <- getRegion db addr port -- tag the update

        let (ServerStatus mapName players capacity queue) = serverStatus
            mapUpdate      = if updated !! 0 then Just mapName  else Nothing
            playersUpdate  = if updated !! 1 then Just players  else Nothing
            capacityUpdate = if updated !! 2 then Just capacity else Nothing
            queueUpdate    = if updated !! 3 then Just queue    else Nothing
            serverUpdate   = ServerUpdate reg addr port mapUpdate playersUpdate capacityUpdate queueUpdate

        writeChan listen serverUpdate -- send updates to the stream endpoint

    threadDelay interval
    trackServer db addr port localport interval listen

{-| keepAlive thread sends a ping to every stream endpoint every X seconds
    to keep the http connection from expiring if no frequent updates happen -}
sendKeepAlive :: Chan ServerUpdate -> Int -> IO ()
sendKeepAlive chan delay = do
    threadDelay delay
    writeChan chan (ServerUpdateKeepalive "")
    sendKeepAlive chan delay
