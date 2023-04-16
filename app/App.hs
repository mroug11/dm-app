{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App ( api
           , app
           ) where

import System.IO (readFile)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy(Proxy))
import Servant
import Servant.Types.SourceT (source)
import Lucid (Html, source_)

import Api (API, API', Region, regionToString, ServerUpdate(ServerUpdate, ServerUpdateKeepalive))
import qualified Db (getAllByRegion, Server, getAllByRegionParsed)
import qualified Render (header, body)
import qualified Data.ByteString as Render

server :: FilePath -> FilePath -> Chan ServerUpdate -> Server API
server db statics chan = status :<|> graphics
    where
        -- | Query for server status
        status reg = pool reg :<|> stream reg
            where            
                pool:: Region -> Handler [Db.Server]
                pool reg = liftIO $ Db.getAllByRegion db (regionToString reg)

                -- | Stream partial status updates for servers that are in Region reg
                stream :: Region -> Handler (SourceIO ServerUpdate)
                stream reg = liftIO $ do 
                        dup <- dupChan chan
                        contents <- getChanContents dup
                        return $ source (ServerUpdateKeepalive "" : [c | c <- contents, matchRegion c (regionToString reg)])

                    where matchRegion (ServerUpdateKeepalive _) _                      = True
                          matchRegion (ServerUpdate region _ _ _  _ _ _ ) clientRegion = region == clientRegion

        -- | Request correctly sized graphics 
        graphics :: String -> Maybe Int -> Maybe Int -> Handler String
        graphics img width height = return "test"
        
server' :: FilePath -> FilePath -> Chan ServerUpdate -> Server API'
server' db  statics chan = server db statics chan :<|> (serveDirectoryWebApp statics :<|> (index :<|> dm )) 
    where -- Render app server-side to memory once during startup
        index :: Handler String -- | Serve the static front page
        index = liftIO $ readFile $ statics ++ "/index.html"

        -- TODO: render the app differently for other devices (pc vs tablet vs phone),
        -- or maybe per the user clients request.
        -- | Send a cacheable web app which provides an interface to query DmServers DB.
        dm :: Maybe Region -> Handler (Html ())
        dm _ = return interface

api :: Proxy API
api = Proxy
api' :: Proxy API'
api' = Proxy

app (statics, db, chan) = serve api' (server' db statics chan)

interface = Render.header <> Render.body