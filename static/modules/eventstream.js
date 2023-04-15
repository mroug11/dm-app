export { listen, listenUpdates }

import { _log } from "./helper.js";
function LOG (...args) {_log(true, ...args)}

/** 
 * Add listeners to events
 * 
 * @param {EventSource} source Server event stream node
 * @param {keyof EventSourceEventMap} event Event field type, default: message
 * @param {Function} handler Handler for the event
 * @param {options} opts Listener options
 */
function listen(source, event = message, handler, opts) {
    source.addEventListener(event, (ev) => { 
        LOG("listen got ", ev);
        handler(ev.data) }, 
        opts);
    return;
}

/**
 * Listen for status updates from DM servers
 * 
 * @param {string} region Query status updates from a subset 
 * 
 * @return {AbortController} Abort() removes the listener
 */
function listenUpdates(region) {
    LOG("listen updates called for ", region);
    let evtSource = new EventSource("/api/status/" + region + "/stream" );
    let controller = new AbortController(); // controllers are consumed when .abort() is called

    listen(evtSource, "status", partialRender, { signal: controller.signal });

    return controller;
}

/**
 * Update a part of the status page DOM
 * 
 * @param {any} data
 */
let partialRender = function(data) {
    LOG("partialRender received data ", data);

    const json = JSON.parse(data);
    if (!json.addr || !json.port) return; // ill-formed update packet; probably a keepalive 

    const serverStatus = document.getElementsByName(json.addr + ':' + json.port)[0].getElementsByClassName("server-status-info")[0];
    const status = serverStatus.getElementsByTagName("p")[0];
    const started = serverStatus.getElementsByTagName("time")[0];

    if (json.map) {  // TODO: make sure the graphic is in cache, or else download and cache it
        serverStatus.setAttribute("style", `background-image: url(${mapNameToUri(json.map)})`);
        started.innerHTML = isoTimeToStr();
    }

    if (json.players) { 
        let statusPlayer = status.getElementsByClassName("status-players")[0];
        statusPlayer.innerHTML = json.players; 
    }

    if (json.capacity) { 
        console.log("updating capacity: " + json.capacity); 
        status.getElementsByClassName("status-capacity")[0].innerHTML = json.capacity; 
    }

    if (json.queued) { 
        console.log("updating queued: " + json.queued); 
        status.getElementsByClassName("status-queued")[0].innerHTML = json.queued; 
    }

    return;
}