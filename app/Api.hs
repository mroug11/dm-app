{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api ( API, API'
           , Region(NA,EU,OZ,UNDEF)
           , regionToString
           , ServerUpdate (ServerUpdate,ServerUpdateKeepalive)
           , HTML
           ) where 

import           Data.Aeson
import           Data.ByteString.Builder as B
import qualified Data.Text               as T (Text, unpack)
import           Servant.API
import           Lucid (Html)
import           Db (Server(Server))

-- | A partial server update packet identified by address and port, carrying optional fields
data ServerUpdate = 
    ServerUpdateKeepalive String | -- ^ Only region tag
    ServerUpdate    
    { region   :: String       -- ^ Region tag, stripped before sending
    , address  :: String       -- ^ Server address
    , port     :: String       -- ^ Server port (ServiceName)
    , mapName  :: Maybe String -- ^ Current map
    , players  :: Maybe Int    -- ^ Number of players
    , capacity :: Maybe Int    -- ^ Maximum players
    , queued   :: Maybe Int    -- ^ Players in queue
    }

class ShowIntStr a where
    showIntStr :: a -> String 
instance ShowIntStr Int where
    showIntStr = show
instance ShowIntStr String where
    showIntStr = id

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
                                                                        showIf "m" m ++ showIf "p" p ++ showIf "c" c ++ showIf "q" q ++ "}"
            mkJson (ServerUpdateKeepalive _)            = "{}"
        
            showIf key (Just x) = ",\"" ++ key ++ "\":\"" ++ showIntStr x ++ "\""
            showIf _ Nothing    = ""

instance FromJSON Db.Server
instance ToJSON Db.Server where
    toJSON (Db.Server reg name addr port m p c q t) = 
        object  [ "addr" .= addr, "port" .= port, "name" .= name, "map" .= m, 
                  "players" .= p, "capacity" .= c, "queued" .= q, "started" .= t
                ]

data HTML

type API  = ApiEndpoint
type API' = API :<|> (Static :<|> RootEndpoint)

type ApiEndpoint = "api" :>
                    (       "status" :> Capture "region" Region :> -- | Query status of all the servers in a region             
                            (       
                                    "pool" :> Get '[JSON] [Db.Server] -- | Get status of all servers in one lump   
                            :<|>    "stream" :> StreamGet NewlineFraming EvStream (SourceIO ServerUpdate) -- | Streaming (partial) server updates
                            )
                            -- | Get correctly sized graphics
                    :<|>    "graphics" :> Capture "name" String :> QueryParam "width" Int :> QueryParam "height" Int :> Get '[JSON] String
                    )

type Static       = "static" :> Raw
type RootEndpoint = (       Get '[HTML] String    -- static index page
                    :<|>    "dm" :> QueryParam "region" Region :> Get '[HTML] (Html ()) -- dm web app
                    )


data Region = EU | NA | OZ | UNDEF

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