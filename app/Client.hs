{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Client (statusPool) where

import Data.Data (Proxy(Proxy))
import qualified Data.Text as T
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S

import App (Region, ServerUpdate)
import Db (Server)

-- | Target specific parts of the API
type PoolApi   = "api" :> "status" :> Capture "region" Region :> "pool" :> Get '[JSON] [Db.Server]      
type StreamApi = "api" :> "status" :> Capture "region" Region :> "stream" :> StreamGet NewlineFraming JSON (SourceIO ServerUpdate)    

poolApi :: Proxy PoolApi
poolApi = Proxy
streamApi :: Proxy StreamApi
streamApi = Proxy -- TODO

statusPool = client poolApi


