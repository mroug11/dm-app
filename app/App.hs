{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App ( api
           , app
           , ServerUpdate(ServerUpdate,ServerUpdateKeepalive)
           , Region
           ) where

import           Servant
import           Servant.API
import           Servant.Types.SourceT (source)
import           Data.Aeson
import           Data.ByteString.Builder    as B
import qualified Data.Text                  as T (Text, unpack)
import           Data.Data (Proxy(Proxy))
import           GHC.Generics
import           System.IO (readFile)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class (liftIO)
import           Lucid (Html, source_)

import qualified Db (getAllByRegion, Server(Server), getAllByRegionParsed)
import qualified Render (header, body, HTML)

{-| The application is made up of static file-serving endpoints
    and a public API that can be queried for JSON formatted data
    about the status of the DM servers, the status of the queue,
    and allows the users to join and renew their place in the queue.
-}
app (statics, db, chan) = serve api' (server' db statics chan)

server :: FilePath -> FilePath -> Chan ServerUpdate -> Server API
server db statics chan = status :<|> (join :<|> renew) :<|> graphics
    where
        join (UserSettings limit (ServerSettings serverlist)) = return True
        renew = return True
        
        -- | Request correctly sized graphics 
        graphics img width height = return "test"
        
        -- | Query for server status
        status reg = pool reg :<|> stream reg
            where            
                pool   reg = liftIO $ Db.getAllByRegion db (regionToString reg)
                stream reg = liftIO $ do 
                          dup <- dupChan chan
                          contents <- getChanContents dup
                          return $ source [c | c <- contents, matchRegion c (regionToString reg)]

                    where matchRegion (ServerUpdateKeepalive _) _                      = True
                          matchRegion (ServerUpdate region _ _ _  _ _ _ ) clientRegion = region == clientRegion
        
server' :: FilePath -> FilePath -> Chan ServerUpdate -> Server API'
server' db  statics chan = server db statics chan :<|> (serveDirectoryWebApp statics :<|> (index :<|> dm )) 
    where -- Render app server-side to memory once during startup
        index :: Handler String -- | Serve the static front page
        index = liftIO $ readFile $ statics ++ "/index.html"

        -- TODO: render the app differently for other devices (pc vs tablet vs phone),
        -- or maybe per the user clients request.
        -- | Send a cacheable web app which provides an interface to query DmServers DB.
        dm :: Maybe Region -> Handler (Html ())
        dm _ = return $ Render.header <> Render.body

{-| API is split into two parts, the static file-server and the public interface
    for querying server databases and joining and renewing the queue status, so 
    that we can generate Javascript for the public API part automatically.
-}
api :: Proxy API
api = Proxy
api' :: Proxy API'
api' = Proxy

type API  = ApiEndpoint
type API' = API :<|> (Static :<|> RootEndpoint)

type ApiEndpoint = "api" :> -- | Public API endpoint
    (    "status" :> Capture "region" Region :> -- | Query status of all the servers in a region             
         (       
              "pool" :> Get '[JSON] [Db.Server] -- | Get status of all servers in one lump   
         :<|> "stream" :> StreamGet NewlineFraming EvStream (SourceIO ServerUpdate) -- | Streaming (partial) server updates
         )
    
    :<|> "queue" :> -- | Join the queue, renew/get the clients queue status
         (
              "join" :> ReqBody '[JSON] UserSettings :> Post '[PlainText] Bool
         :<|> "renew" :> Get '[PlainText] Bool
         )
                            
    :<|> "graphics" :> -- | Get correctly sized graphics
         (
              Capture "name" String :> QueryParam "width" Int :> QueryParam "height" Int :> Get '[JSON] String
         )
    )

type Static = "static" :> Raw -- | Access static assets through raw http

type RootEndpoint = 
    ( 
         Get '[Render.HTML] String -- | Static index page
    :<|> "dm" :> QueryParam "region" Region :> Get '[Render.HTML] (Html ()) -- | Serve the app interface
    )

{-| EvStream is a streaming endpoint that responds with server-sent events type JSON messages
    to client queries.
-}
data EvStream

instance Accept EvStream where
    contentType _ = "text/event-stream"

instance MimeRender EvStream String where
    mimeRender _ ev = toLazyByteString $ stringUtf8 ev <> charUtf8 '\n' <> charUtf8 '\n'

instance MimeRender EvStream ServerUpdate where -- build a JSON response for EvStream
    mimeRender _ status = toLazyByteString $ 
        "event: status\n" <> "data: " <> stringUtf8 (mkJson status) <> stringUtf8 "\n\n"

        where  
            mkJson (ServerUpdate reg addr port m p c q) = "{\"addr\":\"" ++ addr ++ "\",\"port\":\"" ++ port ++ "\"" ++
                                                                        showIf "map" m ++ showIf "players" p ++ showIf "capacity" c ++ showIf "queue" q ++ "}"
            mkJson (ServerUpdateKeepalive _)            = "{}"
        
            showIf key (Just x) = ",\"" ++ key ++ "\":\"" ++ showIntStr x ++ "\""
            showIf _ Nothing    = ""

class ShowIntStr a where
    showIntStr :: a -> String 
instance ShowIntStr Int where
    showIntStr = show
instance ShowIntStr String where
    showIntStr = id

{-| ServerUpdate data packets are a container type for query results from the Servers database.
-}
data ServerUpdate   = -- | A partial server update packet identified by address and port, carrying optional fields
     ServerUpdateKeepalive String -- ^ Only region tag
     | ServerUpdate    
     { region   :: String       -- ^ Region tag, stripped before sending
     , address  :: String       -- ^ Server address
     , port     :: String       -- ^ Server port (ServiceName)
     , mapName  :: Maybe String -- ^ Current map
     , players  :: Maybe Int    -- ^ Number of players
     , capacity :: Maybe Int    -- ^ Maximum players
     , queued   :: Maybe Int    -- ^ Players in queue
     }
        
instance FromJSON Db.Server
instance ToJSON   Db.Server where
    toJSON (Db.Server reg name addr port m p c q t) = 
        object  [ "addr" .= addr, "port" .= port, "name" .= name, "map" .= m, 
                  "players" .= p, "capacity" .= c, "queue" .= q, "started" .= t
                ]

-- | Region type binds the applications query parameter
data Region = EU | NA | OZ | UNDEF

instance FromHttpApiData Region where
    parseQueryParam :: T.Text -> Either T.Text Region
    parseQueryParam reg = 
        case T.unpack reg of
            "eu" -> Right EU
            "na" -> Right NA
            "oz" -> Right OZ
            _    -> Right UNDEF

instance ToHttpApiData Region where
    toQueryParam :: Region -> T.Text
    toQueryParam NA = "na"
    toQueryParam EU = "eu"
    toQueryParam OZ = "oz"
    toQueryParam UNDEF = ""

regionToString :: Region -> String
regionToString reg = 
    case reg of 
        EU    -> "eu"
        NA    -> "na"
        OZ    -> "oz"
        UNDEF -> ""

{-| UserSettings are sent by the client when they join the queue, or update
    their queueing preferences. Settings contain the number of players the 
    user wants to play with, and a list of (addr,port) server-identifying
    tuples that the user is queueing to.
-}
data    UserSettings   = UserSettings Int ServerSettings deriving Generic
newtype ServerSettings = ServerSettings [(String, Int)] deriving Generic

instance FromJSON ServerSettings
instance FromJSON UserSettings
    
instance MimeRender PlainText Bool where 
    mimeRender _ True  = "true" -- Make bool lowercase for JS to process
    mimeRender _ False = "false"