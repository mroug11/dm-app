{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Render ( apiToJS
              , header
              , banner, main, status
              , HTML
              ) where

import           SteamApi (ServerStatus(ServerStatusLong))

import           Servant
import           Servant.JS
import           Language.Javascript.JQuery
import           Lucid
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile, readFile)
import           Data.ByteString.Lazy.Char8 as C8 (pack, unpack)

header :: Html ()
header = html_ [lang_ "en"] $ do
    head_ $ do
        title_ "Deathmatch Auto-Queue"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [charset_ "utf-8"]
        link_ [ rel_ "stylesheet"
              , type_ "text/css"
              , href_ "static/app.css"
              ]
        link_ [ href_ "https://fonts.googleapis.com/css2?family=Quicksand:wght@300&display=swap" 
              , rel_ "stylesheet"
              ]

banner :: Html ()
banner = html_ $ do
    body_ $ do
        div_ [id_ "main-banner"] ""
        div_ [id_ "main-panel"] ""
        div_ [style_ "grid-area: right-body"] ""
        div_ [style_ "grid-area: left-body"] ""
        div_ [id_ "topic-cell", style_ "grid-area: topic"] $ do
            h3_ (a_ [href_ "/dm"] "Deathmatch Auto-Queue")

main :: Html ()
main = html_ $ 
    body_ $ do
        div_ [id_ "main-cell", style_ "grid-area: main"] $ do
            div_ [id_ "main-cell-top"] $ do
                nav_ $ do
                    div_ [id_ "settings-region-select"] $ do
                        a_ [href_ "/dm?region=eu"] $ button_ [type_ "button", name_ "eu"] "Europe"
                        a_ [href_ "/dm?region=na"] $ button_ [type_ "button", name_ "na"] "North America"
            div_ [id_ "main-text-body"] $ do
                h2_ "introduction" 
                p_ "The last few Team Fortress summer events have only been item updates. But this year, we're planning on shipping a full-on holiday-sized update — with items, maps, taunts, unusual effects, war paints, and other community-contributed fixes for the game! Which means we need Steam Workshop content! YOUR Steam Workshop content!"
                p_ "So get to work! (Or back to work, if you were already working but got distracted when the entire internet simultaneously found out about this state-of-the-art blog-post.) Make sure to get your submissions into the Steam Workshop by May 1st, so they can be considered for this as-yet-unnamed, un-themed, but still very exciting summer-situated (but not summer-themed) (unless you wanted to develop summer-themed stuff) update."
                p_ "So get to work! (Or back to work, if you were already working but got distracted when the entire internet simultaneously found out about this state-of-the-art blog-post.) Make sure to get your submissions into the Steam Workshop by May 1st, so they can be considered for this as-yet-unnamed, un-themed, but still very exciting summer-situated (but not summer-themed) (unless you wanted to develop summer-themed stuff) update."
        div_ [style_ "grid-area: footer"] ""

status :: Html ()
status = html_ $ do
    head_ $ do
        with (script_ "") [ src_ "static/app.js"
                          , type_ "module" 
                          , defer_ ""
                          ]
    body_ $ do
        div_ [id_ "main-cell", style_ "grid-area: main"] $ do
            div_ [id_ "main-cell-top"] $ do
                div_ [id_ "settings-column"] $ do
                    div_ [class_ "settings"] $ do
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
                nav_ $ do
                    div_ [id_ "settings-region-select"] $ do
                        button_ [type_ "button", name_ "eu"] "Europe"
                        button_ [type_ "button", name_ "na"] "North America"            
            div_ [id_ "main-cell-bottom"] $ div_ [id_ "server-status-container"] ""
        div_ [style_ "grid-area: footer"] ""

data HTML

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