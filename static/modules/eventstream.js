export { listen, listenUpdates }

import { _log, updateStatusFromJSON } from "./helper.js";
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
 * @returns {EventSource} .close(). Stop the event stream
 */
function listenUpdates(region) {
    LOG("listen updates called for ", region);
    let evtSource = new EventSource("/api/status/" + region + "/stream" );
    //let controller = new AbortController(); // controllers are consumed when .abort() is called

    listen(evtSource, "status", updateServerStatus, { /*signal: controller.signal*/});

    return evtSource;
}

/**
 * Update a part of the status page

 * @param {any} data Data from the event listener
 */
let updateServerStatus = function(data) {
    LOG("updateServerStatus received data ", data);
    const json = JSON.parse(data);
    const server = document.getElementsByName(json.addr + ':' + json.port)[0];

    updateStatusFromJSON(json, server);
}