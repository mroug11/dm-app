{-# LANGUAGE OverloadedStrings #-}

module SteamApi ( getServerStatus
                , queryServer
                , ServerStatus (ServerStatus,ServerStatusLong)
                ) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Data.Word (Word8)
import Data.Char (ord)

import qualified Data.ByteString as BS (ByteString,pack,drop,take,append,singleton)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict, fromStrict, dropWhile, tail, takeWhile, drop, take)
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)
import qualified Data.Text as T

import qualified Control.Exception as E

data ServerStatus = 
    ServerStatus
        T.Text -- ^ Map name
        Int    -- ^ Number of players
        Int    -- ^ Max number of players
    | ServerStatusLong
        String -- ^ Server address
        String -- ^ Server port
        String -- ^ Server name
        String -- ^ Map name
        Int    -- ^ Number of players
        Int    -- ^ Max number of players
        Int    -- ^ Players in queue
        String -- ^ Time when the map started, formatted in h:m
    deriving Eq

-- | Perform a A2S_INFO query for server status
queryServer :: HostName -> ServiceName -> IO BL.ByteString
queryServer ip port = do
    s <- openConn ip port
    sendAll s a2sInfoReq
    receivedStrict <- recv s 4096

    -- Server can respond with a S2C_CHALLENGE (identified by 'A' == 0x41)
    if (BS.take 1 . BS.drop 4 $ receivedStrict) == BS.singleton 0x41 
    then do
        -- append the challenge and send the request again
        -- melkor.tf doesnt seem to respect the challenge properly
        sendAll s (a2sInfoReq `BS.append` BS.drop 5 receivedStrict)
        responseStrict <- recv s 4096
        close s
        return (BL.fromStrict responseStrict)
    else do
        close s
        return (BL.fromStrict receivedStrict)

    where openConn ip port = do
            (addrInfo:_) <- getAddrInfo Nothing (Just ip) (Just port)
            sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
            connect sock (addrAddress addrInfo)
            return sock

-- | parse the A2S response to a more tractable format
parseResponse :: BL.ByteString -> ServerStatus
parseResponse bytes = let 
        mapName     = T.pack . LC.unpack . takeUntilNul . dropUntilNul $ bytes -- skip server name, take map name
        players     = BL.take 2 . BL.drop 2 $ iterate dropUntilNul bytes !! 4
        curPlayers  = head . LC.unpack . BL.take 1 $ players
        maxPlayers  = head . LC.unpack . BL.take 1 . BL.drop 1 $ players
        dropUntilNul = BL.tail . BL.dropWhile (/= 0x0) 
        takeUntilNul = BL.takeWhile (/= 0x0)
    in  ServerStatus mapName (ord curPlayers) (ord maxPlayers)

-- | Query a Source server and return the parsed response
getServerStatus :: HostName -> ServiceName -> IO ServerStatus
getServerStatus addr port = do
    a2sInfo <- queryServer addr port
    E.catch (return $ parseResponse a2sInfo) (\e -> do
        let err = show (e :: E.PatternMatchFail)
        print err
        return $ error "parsing error")

{-| A2S_INFO request: Retrieves information about the Source server including, but not  
    limited to: its name, the map currently being played, and the number of players.
-}
a2sInfoReq :: BS.ByteString
a2sInfoReq = BS.pack
            [   0xFF, 0xFF, 0xFF, 0xFF, -- ^ not a split packet (-1)
                0x54,                   -- ^ A2S header, 'I'
                                        -- ^ A2S request, "Source Engine Query\0"
                0x53,0x6f,0x75,0x72,0x63,0x65,0x20,0x45,0x6e,0x67,0x69,0x6e,0x65,0x20,0x51,0x75,0x65,0x72,0x79,0x0
            ]

-- "0xFF 0xFF 0xFF 0xFF"              
              
{-| Turning A2S_INFO payload string into a Word8 list
    from https://developer.valvesoftware.com/wiki/Server_queries

    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Char8 as BS8
    import Numeric (showHex)

    payloadAsWord8List  = BS.unpack (BS8.pack "Source Engine Query\0")
    payloadWord8Strings = map (\w8 -> "0x" ++ showHex w8 "") queryAsWord8List
    
    payloadAsWord8List :: [Word8]
    payloadAsWord8List = [0x53,0x6f,0x75,0x72,0x63,0x65,0x20,0x45,0x6e,0x67,0x69,0x6e,0x65,0x20,0x51,0x75,0x65,0x72,0x79,0x0]
-}
