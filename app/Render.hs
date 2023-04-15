{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Render ( apiToJS
              , header
              , body1
              , body2
              , HTML ) where
    
import Lucid
import Servant
import Servant.JS
import Language.Javascript.JQuery
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile, readFile)
import Data.ByteString.Lazy.Char8 as C8 (pack, unpack)

import Api (HTML, Region)
import Client (statusPool)
import SteamApi (ServerStatus(ServerStatusLong))

instance ToHtml ServerStatus where
    toHtml (ServerStatusLong addr port name map players capacity queued started) = do
        let mapGraphic = T.pack $ "background-image: url(\"" ++ "static/images/" ++ mapNameToGraphic map ++ "\")"
        let connect = T.pack $ addr ++ ":" ++ port
        div_ [class_ "server-status", name_ connect] $ do
                    div_ [class_ "server-status-info", style_ mapGraphic, data_ "time" (T.pack started)] $ do
                        p_ $ do
                            "players: "
                            span_ [class_ "status-players"] $ toHtml (T.pack $ show players)
                            "/"
                            span_ [class_ "status-capacity"] $ toHtml (T.pack $ show capacity)
                            "("
                            span_ [class_ "status-queued"] $ toHtml  (T.pack $ show queued)
                            ")"
                    div_ [class_ "server-name-banner"] (p_ (toHtml name))

-- TODO: get rid of manually updating the pattern, string matching
mapNameToGraphic :: String -> String
mapNameToGraphic "cp_snakewater_final1" = "230x130px-Snakewater_compressed.jpg"
mapNameToGraphic "cp_sunshine"          = "230x130px-Sunshine_compressed.jpg"
mapNameToGraphic "cp_metalworks"        = "230x130px-Metalworks_compressed.jpg"
mapNameToGraphic "cp_metalworks_f4"     = "230x130px-Metalworks_compressed.jpg"
mapNameToGraphic "cp_granary_pro_rc8"   = "230x130px-Granary_compressed.jpg"
mapNameToGraphic "cp_granary_pro2"      = "230x130px-Granary_compressed.jpg"
mapNameToGraphic "koth_bagel_rc5"       = "230x130px-Bagel_compressed.jpg"
mapNameToGraphic "cp_process_f7"        = "230x130px-Process_compressed.jpg"
mapNameToGraphic "cp_process_f11"       = "230x130px-Process_compressed.jpg"
mapNameToGraphic "cp_process_final"     = "230x130px-Process_compressed.jpg"
mapNameToGraphic "cp_prolands_rc2ta"    = "230x130px-Badlands_compressed.jpg"
mapNameToGraphic "koth_clearcut_b15d"   = "230x130px-Clearcut_compressed.jpg"
mapNameToGraphic "koth_product_final"   = "230x130px-Product_compressed.jpg"
mapNameToGraphic "koth_product_rcx"     = "230x130px-Product_compressed.jpg"
mapNameToGraphic "cp_reckoner_rc6"      = "230x130px-Reckoner_compressed.jpg"
mapNameToGraphic "cp_gullywash_f9"      = "230x130px-Gullywash_compressed.jpg"
mapNameToGraphic "ctf_turbine_pro_rc4"  = "230x130px-Turbine_compressed.jpg"
mapNameToGraphic "cp_sultry_b7"         = "230x130px-Sultry_compressed.jpg"
mapNameToGraphic "cp_villa_b19"         = "230x130px-Villa_compressed.jpg"
mapNameToGraphic "cp_freight_final1"    = "230x130px-Freight_compressed.jpg"
mapNameToGraphic s                      = s

instance Accept HTML where
    contentType _ = "text/html"

-- | Rendering pages for Servant
instance MimeRender HTML String where -- render raw from a file
    mimeRender _ = C8.pack

instance MimeRender HTML (Html ()) where -- render from Lucid HTML
    mimeRender _ = renderBS

-- | Generate jquery.js and Javascript from the API type to dir
apiToJS api dir = do
    let content = jsForAPI api jquery
    T.writeFile (dir ++ "/api.js") content
    --fp <- Language.Javascript.JQuery.file
    --jquery <- T.readFile fp
    --putStrLn "writing jquery.js"
    --T.writeFile (dir ++ "/jquery.js") jquery

header :: FilePath -> Html ()
header static = html_ $ do
    head_ $ do
        title_ "Deathmatch Auto-Queue"
        link_ [ rel_ "stylesheet"
              , type_ "text/css"
              , href_ "static/app.css"
              ]

        with (script_ "") [ src_ "https://code.jquery.com/jquery-3.6.4.min.js"
                          , type_ "text/javascript" 
                          , integrity_ "sha256-oP6HI9z1XaZNBrJURtCoUT5SUnxFr8s3BzRl+cbzUq8="
                          , crossorigin_ "anonymous"
                          ]

        with (script_ "") [ src_ "static/app.js"
                          , type_ "module" 
                          , defer_ ""
                          ]

staticPage :: Html ()
staticPage = bannerArea <> settingsColumn <> panelBase

body1 :: Html ()
body1 = staticPage <> panelIntro

body2 :: [ServerStatus] -> Html ()
body2 servers = staticPage <> panelStatus servers

bannerArea :: Html ()
bannerArea = html_ $ do
    body_ $ do
        div_ [id_ "main-banner"] ""
        div_ [style_ "grid-area: left-banner"] ""
        div_ [id_ "topic-cell", style_ "grid-area: topic"] $ do
            h3_ (a_ [href_ "/dm"] "Deathmatch Auto-Queue")
        div_ [style_ "grid-area: right-banner"] ""

panelBase :: Html ()
panelBase =  html_ $ 
    body_ $ do
        div_ [id_ "main-panel"] ""
        div_ [style_ "grid-area: right-panel"] ""
        div_ [style_ "grid-area: left-panel"] ""

panelStatus :: [ServerStatus] -> Html ()
panelStatus servers = 
    div_ [id_ "main-cell", style_ "grid-area: main"] $ do 
        div_ [id_ "server-status-container"] $ do 
            list servers

            where list []       = body_ ""
                  list (s:serv) = toHtml s <> list serv

panelIntro :: Html ()
panelIntro = 
    div_ [id_ "main-cell", style_ "grid-area: main"] $ do
        div_ [id_ "main-text-body"] $ do
            h2_ "Main page" 
            p_ "The last few Team Fortress summer events have only been item updates. But this year, we're planning on shipping a full-on holiday-sized update — with items, maps, taunts, unusual effects, war paints, and other community-contributed fixes for the game! Which means we need Steam Workshop content! YOUR Steam Workshop content!"
            p_ "So get to work! (Or back to work, if you were already working but got distracted when the entire internet simultaneously found out about this state-of-the-art blog-post.) Make sure to get your submissions into the Steam Workshop by May 1st, so they can be considered for this as-yet-unnamed, un-themed, but still very exciting summer-situated (but not summer-themed) (unless you wanted to develop summer-themed stuff) update."
            p_ "So get to work! (Or back to work, if you were already working but got distracted when the entire internet simultaneously found out about this state-of-the-art blog-post.) Make sure to get your submissions into the Steam Workshop by May 1st, so they can be considered for this as-yet-unnamed, un-themed, but still very exciting summer-situated (but not summer-themed) (unless you wanted to develop summer-themed stuff) update."
                                    
settingsColumn :: Html ()
settingsColumn = html_ $ do 
    body_ $ do
        div_ [id_ "settings-column", style_ "grid-area: settings"] $ do
            div_ [class_ "website-logo"] ""
            div_ [class_ "website-title"] (h1_ "Settings")
            div_ [class_ "settings"] $ do
                legend_ "Search for servers in"
                div_ [id_ "settings-region-select"] $ do
                    button_ [type_ "button", name_ "eu", onclick_ "queryServerPool(\"eu\")"] "Europe"
                    button_ [type_ "button", name_ "na", onclick_ "queryServerPool(\"na\")"] "North America"

                hr_ []

                legend_ "Join once"
                div_ [id_ "settings-queue-size"] $ do
                    input_ [type_ "range", id_ "queue-size", name_ "queue_size", list_ "queue-size-values", min_ "1", max_ "10", step_ "1"]
                    datalist_ [id_ "queue-size-values"] $ do
                        option_ [value_ "1", label_ "1"] "1"
                        option_ [value_ "2", label_ "2"] "2"
                        option_ [value_ "3", label_ "3"] "3"
                        option_ [value_ "4", label_ "4"] "4"
                        option_ [value_ "5", label_ "5"] "5"
                        option_ [value_ "6", label_ "6"] "6"
                        option_ [value_ "7", label_ "7"] "7"
                        option_ [value_ "8", label_ "8"] "8"
                        option_ [value_ "9", label_ "9"] "9"
                        option_ [value_ "10", label_ "10"] "10"
                    br_ []
                    label_ [for_ "queue-size"] "players are in queue"

                hr_ []

                legend_ "Prompt when the queue is ready"
                div_ [class_ "settings-join-behavior"] $ do
                    form_ [radiogroup_ "join-behavior", id_ "join-behavior"] $ do
                        input_ [type_ "radio", name_ "join", id_ "confirm-join", value_ "confirm"]
                        label_ [for_ "confirm-join"] "Ask for confirmation"
                        br_ []
                        input_ [type_ "radio", name_ "join", id_ "instant-join", value_ "instant"]
                        label_ [for_ "instant-join"] "Join the server instantly"

