export { switchStatusPage }

import { _log, updateStatusFromJSON } from "./modules/helper.js";
import { listenUpdates } from "./modules/eventstream.js";
import { sendRequest } from "./modules/queue.js";

let LOG = (...args) => {_log(true, ...args)}

let controller = new AbortController();
let evtSource;

window.addEventListener("DOMContentLoaded", function initPage(event) {
    if (!document.cookie) {
        const id = Math.random().toString(36).substr(2, 9);
        document.cookie="token=" + id + ";max-age=86400";
        //document.cookie="size=6";
        //document.cookie="confirm=true";
        //document.cookie="servers=";
        //document.cookie="queued=false";
    } else {
        // read settings values from browser cookies
        restoreState();
    }

    // add input listeners
    listenInput();

    sendRequest();

    // render the regional server status page if
    if (window.location.search.split('=')[0] == "?region") {
        serverStatusPage(region());
    } 

});

function listenInput() {
    document.getElementById("queue-size").addEventListener("input", newVal => {
        document.cookie="size=" + newVal.target.value;
    });

    let regionSelect = document.getElementById("settings-region-select").querySelectorAll("button");
    regionSelect[0].onclick = (ev) => {switchStatusPage("eu")}
    regionSelect[1].onclick = (ev) => {switchStatusPage("na")}
}

function restoreState () {
    // restore region selection
    if (region()) {
        const btn = document.querySelector(`div#settings-region-select button[name=${region()}]`);
        btn.setAttribute("disabled","");
        btn.setAttribute("style","cursor:default");
    }

    // restore queue size
    //document.querySelector("div#settings-queue-size input[name='queue_size']").value = cookieVal("size");

    // restore join behavior // can't be done properly on load/DOMContentLoaded listener?
    /*var confirm = !!getCkiVal("confirmJoin");
    console.log(confirm);
    var radio = document.getElementById("join-behavior");
    console.log(radio);
    console.log(radio.join.value);
    console.log(radio.join[1].checked);
    if (confirm) {
        radio.join[0].checked = true;
    } else {
        radio.join[0].checked = false;
        radio.join[1].checked = true;
    }*/
}

function switchStatusPage (region) {
    if (region == "eu") {
        document.querySelector("div#settings-region-select button[name='eu']").setAttribute("disabled","");
        document.querySelector("div#settings-region-select button[name='eu']").setAttribute("style","cursor:default");
        document.querySelector("div#settings-region-select button[name='na']").removeAttribute("disabled","");
        document.querySelector("div#settings-region-select button[name='na']").removeAttribute("style","cursor:default");
        history.pushState(null, 'Deathmatch Auto-Queue', `${window.location.origin}/dm?region=eu`);
    } else if (region == "na") {
        document.querySelector("div#settings-region-select button[name='na']").setAttribute("disabled","");
        document.querySelector("div#settings-region-select button[name='na']").setAttribute("style","cursor:default");
        document.querySelector("div#settings-region-select button[name='eu']").removeAttribute("disabled","");
        document.querySelector("div#settings-region-select button[name='eu']").removeAttribute("style","cursor:default");
        history.pushState(null, 'Deathmatch Auto-Queue', `${window.location.origin}/dm?region=na`);
    } else { 
        return;
    }

    LOG("http protocol: ", performance.getEntriesByType('navigation')[0].nextHopProtocol);
    
    // render the new status page
    serverStatusPage(region);
}

const serverStatusPage = async function(region) {
    let promise = fetchServerPool(region);
    promise.then(response => response.json())
           .then(json => {
                LOG("promise got data ", json)
                renderStatusPage(json);

                // abort the previous event stream
                if (evtSource) evtSource.close(); 

                // listen for new server status update events
                evtSource = listenUpdates(region); 
            })
            .catch(err => {
                LOG("promise error: ", err)
            })
}

function fetchServerPool(region) {
    let pathname = "/api/status/" + region + "/pool";
    let options  =  { headers: { "Accept": "application/json"
                               , "Content-Type": "application/json"
                               }
                    , method: "GET"
                    , cache: "no-cache"
                    }

    return fetch(pathname, options);
}

function renderStatusPage (json) {
    LOG("rendering status page with ", json)

    const main = document.getElementById("main-cell");
    main.innerHTML = "";

    const statusContainer = document.createElement('div');
    statusContainer.id = "server-status-container";
    main.appendChild(statusContainer);

    for (var i = 0; i < json.length; i++) {
        const status = document.createElement('div');
        status.className = "server-status";
        status.setAttribute("name", json[i].addr + ':' + json[i].port)
        statusContainer.appendChild(status);

        const info = document.createElement('div');
        info.className = "server-status-info";
        info.innerHTML = "<span class=\"status-players\"></span>/\
                          <span class=\"status-capacity\"></span>\
                          <span class=\"status-queue\"></span>\
                          <time></time>";
        status.appendChild(info);

        const name = document.createElement('div');
        name.className = "server-name-banner";
        status.appendChild(name);

        updateStatusFromJSON(json[i], status)
    }
}

/* ------  utility functions ------ */

var region = function() {
    return window.location.search.split('=')[1];
}

var cookieVal = function (name) {
    return document.cookie.split("; ").find((key) => key.startsWith(name)).split("=")[1]
}
