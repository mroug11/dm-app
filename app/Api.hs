{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Api ( API, API'
           , Region(NA,EU,OZ,UNDEF)
           , regionToString
           , ServerUpdate (ServerUpdate,ServerUpdateKeepalive)
           , HTML
           , UserSettings(UserSettings), ServerSettings(ServerSettings)
           ) where 

import           Data.Aeson
import           Data.ByteString.Builder as B
import qualified Data.Text               as T (Text, unpack)
import           Servant.API
import           Lucid (Html)
import           Db (Server(Server))
import GHC.Generics
import Data.Text (toLower)

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
         Get '[HTML] String -- | Static index page
    :<|> "dm" :> QueryParam "region" Region :> Get '[HTML] (Html ()) -- | Serve the app interface
    )

data    EvStream
data    HTML
data    Region         = EU | NA | OZ | UNDEF
data    UserSettings   = UserSettings Int ServerSettings deriving Generic
newtype ServerSettings = ServerSettings [(String, Int)] deriving Generic
data    ServerUpdate   = -- | A partial server update packet identified by address and port, carrying optional fields
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

instance MimeRender PlainText Bool where 
    mimeRender _ True  = "true" -- Make bool lowercase for JS to process
    mimeRender _ False = "false"
        
instance FromJSON Db.Server
instance ToJSON Db.Server where
    toJSON (Db.Server reg name addr port m p c q t) = 
        object  [ "addr" .= addr, "port" .= port, "name" .= name, "map" .= m, 
                  "players" .= p, "capacity" .= c, "queue" .= q, "started" .= t
                ]

instance FromJSON ServerSettings
instance FromJSON UserSettings

instance FromHttpApiData Region where
    parseQueryParam :: T.Text -> Either T.Text Region
    parseQueryParam reg = case T.unpack reg of
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
regionToString reg = case reg of 
                    EU    -> "eu"
                    NA    -> "na"
                    OZ    -> "oz"
                    UNDEF -> ""

class ShowIntStr a where
    showIntStr :: a -> String 
instance ShowIntStr Int where
    showIntStr = show
instance ShowIntStr String where
    showIntStr = id
