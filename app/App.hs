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
import qualified Render (header, body1, body2)

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
server' db  statics chan = server db statics chan :<|> (static :<|> root) 
    where 
        static = serveDirectoryWebApp statics
        root = index :<|> dm 
            -- | Render app server-side to memory once during startup
            where 
                -- | Serve the static front page
                index :: Handler String
                index = liftIO $ readFile $ statics ++ "/index.html"

                -- TODO: render the app differently for other devices (pc vs tablet vs phone),
                -- or maybe per the user clients request.
                -- | Send a cacheable web app which provides an interface to query DmServers DB.
                dm :: Maybe Region -> Handler (Html ())
                dm Nothing    = return $ Render.header statics <> Render.body1
                dm (Just reg) = do
                        servers <- liftIO $ Db.getAllByRegionParsed (regionToString reg) db
                        return $ Render.header statics <> Render.body2 servers

{-
    Idea: add a middleware layer to check the clients request header for a JSON web token. 
    If the token exists, it means that the client has the site cached and loaded (because the token
    is also saved in the clients offline cache with the static files), therefore the server has 
    to send only the JSON response to the client querying the API. If the token doesn't exist, 
    e.g., the client doesn't have the neccessary static files (.css, .html, .js etc) to operate 
    the app, then the middleware will first push (HTTP/2) the files to the client and afterwards
    let the application create and push a response to the API query.
-}

api :: Proxy API
api = Proxy
api' :: Proxy API'
api' = Proxy

app :: (FilePath, FilePath, Chan ServerUpdate) -> Application
app (statics, db, chan) = serve api' (server' db statics chan)