{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-} 

module Settings ( runtimeInfo
                , _dbPath, _serverlist, _staticDir, _port, _interleave
                , _isHttps, _tlsOpts, _localport, _cert, _key ) where

import Configuration.Utils
import Control.Monad.IO.Class

runtimeInfo :: ProgramInfo AppConf
runtimeInfo = programInfo "web server" pAppConf defaultAppConf

defaultAppConf :: AppConf
defaultAppConf = AppConf
    { _dbPath     = "db.sqlite3" 
    , _serverlist = "serverlist.txt" 
    , _staticDir  = "static" 
    , _port       = 8080
    , _interleave = 100000
    , _localport  = 3306
    , _isHttps    = True
    , _tlsOpts    = defaultTlsOpts
    }

defaultTlsOpts :: TlsOpts
defaultTlsOpts = TlsOpts
    { _cert = "cert.pem"
    , _key  = "key.pem"
    }

data AppConf = AppConf
    { _dbPath :: FilePath
    , _serverlist :: FilePath
    , _staticDir :: FilePath
    , _port   :: Int
    , _interleave :: Int
    , _localport :: Int
    , _isHttps :: Bool
    , _tlsOpts :: TlsOpts
    }

data TlsOpts = TlsOpts
    { _cert :: FilePath
    , _key :: FilePath
    }

cert :: Functor f => (FilePath -> f FilePath) -> TlsOpts -> f TlsOpts
cert f s = (\u -> s { _cert = u }) <$> f (_cert s)
key :: Functor f => (FilePath -> f FilePath) -> TlsOpts -> f TlsOpts
key f s = (\u -> s { _key = u }) <$> f (_key s)

instance FromJSON (TlsOpts -> TlsOpts) where
    parseJSON = withObject "TlsOpts" $ \o -> id
        <$< cert ..: "cert" % o
        <*< key ..: "key" % o

instance ToJSON TlsOpts where
    toJSON a = object
        [ "cert" .= _cert a
        , "key" .= _key a
        ]

pTlsOpts :: MParser TlsOpts
pTlsOpts = id 
    <$< cert .:: strOption
        % long "cert"
        <> help "cert.pem file path"
    <*< key .:: strOption
        % long "key"
        <> help "key.pem file path"

dbPath :: Functor f => (String -> f String) -> AppConf -> f AppConf
dbPath f s = (\u -> s { _dbPath = u }) <$> f (_dbPath s)
localport :: Functor f => (Int -> f Int) -> AppConf -> f AppConf
localport f s = (\u -> s { _localport = u }) <$> f (_localport s)
serverlist :: Functor f => (String -> f String) -> AppConf -> f AppConf
serverlist f s = (\u -> s { _serverlist = u }) <$> f (_serverlist s)
staticDir :: Functor f => (String -> f String) -> AppConf -> f AppConf
staticDir f s = (\u -> s { _staticDir = u }) <$> f (_staticDir s)
port :: Functor f => (Int -> f Int) -> AppConf -> f AppConf
port f s = (\u -> s { _port = u }) <$> f (_port s)
interleave :: Functor f => (Int -> f Int) -> AppConf -> f AppConf
interleave f s = (\u -> s { _interleave= u }) <$> f (_interleave s)
isHttps :: Functor f => (Bool -> f Bool) -> AppConf -> f AppConf
isHttps f s = (\u -> s { _isHttps = u }) <$> f (_isHttps s)
tlsOpts :: Functor f => (TlsOpts -> f TlsOpts) -> AppConf -> f AppConf
tlsOpts f s = (\u -> s { _tlsOpts = u }) <$> f (_tlsOpts s)

instance FromJSON (AppConf -> AppConf) where
    parseJSON = withObject "AppConf" $ \o -> id
        <$< tlsOpts %.: "tlsOpts" % o
        <*< dbPath ..: "dbPath" % o
        <*< serverlist ..: "serverlist" % o
        <*< staticDir ..: "staticDir" % o
        <*< port ..: "port" % o
        <*< interleave ..: "interleave" % o
        <*< localport ..: "localport" % o
        <*< isHttps ..: "isHttps" % o

instance ToJSON AppConf where
    toJSON a = object
        [ "tlsOpts" .= _tlsOpts a
        , "dbPath" .= _dbPath a
        , "serverlist" .= _serverlist a
        , "staticDir" .= _staticDir a
        , "port" .= _port a
        , "interleave" .= _interleave a
        , "localport" .= _localport a
        , "isHttps" .= _isHttps a
        ]

pAppConf :: MParser AppConf
pAppConf = id 
    <$< tlsOpts %:: pTlsOpts
    <*< dbPath .:: strOption
        % long "dbPath"
        <> help "Path to the database file"
    <*< serverlist .:: strOption
        % long "serverlist"
        <> help "Path to a list of servers"
    <*< serverlist .:: strOption
        % long "staticDir"
        <> help "Directory where static files are stored"
    <*< port .:: option auto
        % long "port"
        <> short 'p'
    <*< port .:: option auto
        % long "interleave"
        <> short 'i'
        <> help "Initial delay in microseconds to produce the interval between launching new tracker threads"
    <*< localport  .:: option auto
        % long "localport"
        <> help "Port of the local listener service used to consolidate all tracker update streams into one source"
    <*< isHttps .:: option auto
        % long "isHttps"