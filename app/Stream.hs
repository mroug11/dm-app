
module Stream ( listener, trackServer, sendKeepAlive ) where

--import System.IO
--import Control.Concurrent (forkIO)
--import Network.Socket
--import Network.Socket.ByteString (recv, sendAll)
--import qualified Data.ByteString as B
--import qualified Control.Exception as E
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)

import Db (updateServer, getRegion)
import Api (ServerUpdate(ServerUpdate,ServerUpdateKeepalive))
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
    
    when (True) $ do -- or updated
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

{-
update :: HostName ->     -- ^ Address
          String ->       -- ^ Port
          B.ByteString -> -- ^ Message
          IO Bool         -- ^ Status
update host port msg = withSocketsDo $ do
    let hints = defaultHints { addrSocketType = Stream }
    (addr:_) <- getAddrInfo (Just hints) (Just host) (Just port) 
    E.bracketOnError (openSocket addr) (\s -> close s >> return False) (\s -> do 
        E.catch (sendUpdate s addr msg) (\e -> do 
            let err = show (e :: E.IOException)
            return False))

    where sendUpdate s addr msg = do
            connect s (addrAddress addr) 
            sendAll s msg
            close s
            return True
-}

{-
listener :: String ->   -- ^ localport
            Int ->      -- ^ maximum number of queued connections
            IO Socket
listener port n = withSocketsDo $ do
    addr:_ <- getAddrInfo Nothing Nothing (Just port) --use loopback
    s <- open addr
    print s
    stype <- getSocketType s
    print stype
    sname <- getSocketName s
    print sname
    return s

    {-E.catch (open addr) (\e -> do 
            let err = show (e :: E.IOException)
            print err
            return stderr)
-}
    where
        open addr = do
            s <- openSocket addr
            bind s $ addrAddress addr
            listen s n
            return s

        loop s = do
            (_conn, address) <- accept s
            msg <- recv _conn 512
            print msg
            loop s

-}


