{-# LANGUAGE InstanceSigs #-} -- for parseQueryParam :: Text -> Either Text Region
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
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder as B
import qualified Data.Text               as T (Text, unpack, pack)
import           Servant.API
import           Lucid (Html)

import qualified Db (Server)

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
                                                              apIf "m" m ++ apIf "p" p ++ apIf "c" c ++ apIf "q" q ++ "}"
            mkJson (ServerUpdateKeepalive _)            = "{}"

            apIf key (Just x) = ",\"" ++ key ++ "\":\"" ++ showIntStr x ++ "\""
            apIf _ Nothing    = ""

class ShowIntStr a where
    showIntStr :: a -> String 
instance ShowIntStr Int where
    showIntStr = show
instance ShowIntStr String where
    showIntStr = id

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

instance ToJSON Db.Server
instance FromJSON Db.Server

instance ToJSON ServerUpdate where
    toJSON (ServerUpdate region addr port map players capacity queued) =
        -- If two key-values ('addr') are clashing, the later keys and their values are preferred
        -- Use this to prune fields that have no values from the JSON response
        let mapPair = case map of
                        (Just m) -> ("map", String (T.pack m))
                        Nothing -> ("addr", String "dummy")
            playerPair = case players of
                        (Just p) -> ("players", String (T.pack . show $ p))
                        Nothing -> ("addr", String "dummy") 
            capacityPair = case capacity of
                        (Just c) -> ("players", String (T.pack . show $ c))
                        Nothing -> ("addr", String "dummy")
            queuePair = case queued of
                        (Just q) -> ("players", String (T.pack . show $ q))
                        Nothing -> ("addr", String "dummy")

        in object ([mapPair] ++ [playerPair] ++ [capacityPair] ++[queuePair] ++ ["addr" .= addr,"port" .= port])

    toJSON (ServerUpdateKeepalive region) = object []

{-
instance FromJSON ServerUpdate where
    parseJSON (Object json) = ServerUpdate 
        <$> json .: "addr"
        <*> json .: "port" 
-}
data HTML
-- | Region query returns a datatype
data Region = EU | NA | OZ | UNDEF

{-| The API is made up from a static file server (CSS, JS, PNG etc), 
    HTML server (user interface) and API server (code interface).
    We split the API and server in two parts so that we don't have
    to generate any JS for the static parts of the application.
-}
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