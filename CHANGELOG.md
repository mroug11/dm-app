# Revision history for erehwn-net

## 0.4.5 -- 2023-04-17

* user post to join and renew queue status works
* added error checking to user database methods

## 0.4.3 -- 2023-04-16

* Users module handles clients joining, renewing and leaving the queue
* database to keep track of all current users identified by browser token
* merged App and Api modules

## 0.4.0

* serving server-rendered pages from memory for caching purposes
* using client side rendering to render the status page

## 0.3.7

* moved config-utils into its own module
* implemented the eventsource mime type
* made all fetch requests into server-sent events
* reorganized static folder, some css tweaks
* added exception handling to main

## 0.3.5

* webworker queries stream api for partial updates
* using fetch api to read the response stream and return values asynchronously
* javascript reads the stream and does partial updates on the DOM

## 0.3.0

* started writing Javascript for the ui
* made the first HTML and CSS layout
* working page navigation 
* js calls to pool api for server updates
* js renders the received json into html
* user state is saved in browser cookies and restored using js

## 0.2.7 -- 2023-04-09

* re-partitioned the API into two parts to separate the static application
* made Client to generate javascript from the API and JQuery files
* added a conf parameter to specify location of static file dir

## 0.2.5 -- 2023-04-09

* switched to Control.Concurrent.Chan over Network.Socket for message passing
* implemented an optimized toJSON instance for ServerUpdate 
* completed the streaming endpoint to work with multiple connections

## 0.2.2 -- 2023-04-07

* made tracker forking interleaved to spread Db access over an interval
* refactored Db code to be more readable
* updateServer checks for server status changes before writing to the Db
* Trackers now send a partial server update to a local listener
* added a streaming endpoint for server updates; updates get polled from the local listener

## 0.1.7 -- 2023-04-06

* added a port parameter to the DB and serverlist
* added function to update DmServer DB rows
* added functions to query Source servers for information with A2S_INFO request
* added support for forking a user thread for each server tracked

## 0.1.5 -- 2023-04-05

* Read/render the static html pages once on startup
* Split endpoints into staticFiles, root files (index.html, dm.html) and the API
* dmStatusQuery, returns a streaming source of the servers statuses for that region
* imgProvider gives a requested graphic in correct dimensions
