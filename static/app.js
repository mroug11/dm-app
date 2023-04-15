import { _log } from "./modules/helper.js";
import { listenUpdates } from "./modules/eventstream.js";

let controller = new AbortController();

window.addEventListener("DOMContentLoaded", (event) => {
    if (!document.cookie) {
        const id = Math.random().toString(36).substr(2, 9);
        document.cookie="token=" + id + ";max-age=86400;path=/dm";
        document.cookie="size=6";
        document.cookie="confirm=true";
        document.cookie="servers=";
        document.cookie="queued=false";
    }

    // read settings values from browser cookies
    restoreState();

    // add input listeners
    document.getElementById("queue-size").addEventListener("input", newVal => {
        document.cookie="size=" + newVal.target.value;
    });

    // render utctime (in iso standard) to local time
    let serverStatus = document.getElementById("server-status-container");
    if (serverStatus) {
        var statusList = serverStatus.getElementsByClassName("server-status-info");
        for (var i = 0; i < statusList.length; i++) {
            statusList[i].innerHTML += `<time>${isoTimeToStr(statusList[i].dataset.time)}</time>`;
        }
    }

    controller = listenUpdates("eu");
    /* poll for updates if we are in one of the status pages
    let reg = region();
    if (reg) { 
        promise = startFetch(reg);
        pump(promise);
    }*/
});

function pump (promise) {
    if (!promise) return;
    
    promise.then(response => {
            const reader = response.body.pipeThrough(new TextDecoderStream()).getReader(); 
            return loop();

            function loop() {
                return reader.read()
                        .then(({done, value}) => {
                            _log("got data ", value);
                            if (!value) return loop();
                            renderPartial(value)
                            return loop();
                        }).catch((err) => { 
                            // most often happens when the connection times out
                            // todo: reactivate on timeout, handle other errors
                            _log("read error ", err); 
                            //let _promise = fullFetch(region());
                            //location.reload();
                            return err;//pump(_promise);
                        });
            }
        
        }).catch((err) => { // triggered when the user clicks too fast between regions
            _log("promise error ", err);
        })
}

function startFetch (reg) {
    if (!reg) return

    controller   = new AbortController(); // controllers are consumed when .abort() is called
    let pathname = '/api/status/' + reg + '/stream';
    let options  =  { headers: { 'Accept': 'application/json'
                               , 'Content-Type': 'application/json'
                               , 'Keep-Alive': 'timeout=999999'
                               }
                    , method: "GET"
                    , cache: "no-cache"
                    , signal: controller.signal
                    }

    return fetch(pathname, options);
}

function restoreState () {
    // restore region selection
    if (region()) {
        const btn = document.querySelector(`div#settings-region-select button[name=${region()}]`);
        btn.setAttribute("disabled","");
        btn.setAttribute("style","cursor:default");
    }

    // restore queue size
    document.querySelector("div#settings-queue-size input[name='queue_size']").value = cookieVal("size");

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

var queryServerPool = function(region) {
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

    _log(performance.getEntriesByType('navigation')[0].nextHopProtocol);
    
    $.ajax(
        { url: '/api/status/' + encodeURIComponent(region) + '/pool'
        , success: renderStatusPage
        , error: throwError
        , type: 'GET'
        , cache: false
        }
    );

    controller.abort(); // abort the previous fetch stream
    promise = startFetch(region);
    pump(promise); // start a new update stream
}

var renderPartial = function(recv) {
    const json = JSON.parse(recv);
    if (!json.addr) return; // probably a keepalive packet

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

    if (json.capacity) { console.log("updating capacity: " + json.capacity); status.getElementsByClassName("status-capacity")[0].innerHTML = json.capacity; }
    if (json.queued) { console.log("updating queued: " + json.queued); status.getElementsByClassName("status-queued")[0].innerHTML = json.queued; }
}

var renderStatusPage = function(data, textStatus, jqXHR) {
    const json = JSON.parse(JSON.stringify(data));

    const mainCell = document.getElementById("main-cell");
    mainCell.innerHTML = "";

    const statusContainer = document.createElement('div');
    statusContainer.id = "server-status-container";
    mainCell.appendChild(statusContainer);

    for (var i = 0; i < data.length; i++) {
        const statusDiv = document.createElement('div');
        statusDiv.className = "server-status";
        statusDiv.setAttribute("name", json[i].serverAddress + ':' + json[i].serverPort)

        const info = document.createElement('div');
        info.innerHTML = `<p>players: 
                          <span class="status-players">${json[i].serverPlayers}</span>/<span class="status-capacity">${json[i].serverCapacity}</span>(<span class="status-queueu">${json[i].serverQueued}</span>)
                          </p>
                          <time>${isoTimeToStr(json[i].serverStarted)}</time>`;
        info.className = "server-status-info";
        info.style = `background-image: url(${mapNameToUri(json[i].serverMap)})`;
        info.data = `${json[i].serverStarted}`;

        const name = document.createElement('div');
        name.className = "server-name-banner";
        name.innerHTML = `<p>${json[i].serverName}</p>`;

        statusContainer.appendChild(statusDiv);
        statusDiv.appendChild(info);
        statusDiv.appendChild(name);
    }
}

/* ------  utility functions ------ */

function isoTimeToStr (isotime) {
    if (isotime) {
        var localTime = new Date(isotime);
    } else {
        var localTime = new Date();
    }
    return localTime.getHours() + ':' + localTime.getMinutes();
}

var region = function() {
    return window.location.search.split('=')[1];
}

var cookieVal = function (name) {
    return document.cookie.split("; ").find((key) => key.startsWith(name)).split("=")[1]
}

function throwError(jqXHR, textStatus, errorThrown) {
    console.log(textStatus);
}

function mapNameToUri (name) {
    switch (name.split('_')[1]) {
        case 'snakewater':
            return 'static/images/230x130px-Snakewater_compressed.jpg';
        case 'gullywash':
            return 'static/images/230x130px-Gullywash_compressed.jpg';
        case 'process':
            return 'static/images/230x130px-Process_compressed.jpg';
        case 'product':
            return 'static/images/230x130px-Product_compressed.jpg';
        case 'villa':
            return 'static/images/230x130px-Villa_compressed.jpg';
        case 'freight':
            return 'static/images/230x130px-Freight_compressed.jpg'
        case 'sunshine':
            return 'static/images/230x130px-Sunshine_compressed.jpg';
        case 'bagel':
            return 'static/images/230x130px-Bagel_compressed.jpg';
        case 'granary':
            return 'static/images/230x130px-Granary_compressed.jpg';
        case 'clearcut':
            return 'static/images/230x130px-ClearCut_compressed.jpg';
        case 'badlands':
            return 'static/images/230x130px-Badlands_compressed.jpg';
        case 'prolands':
            return 'static/images/230x130px-Badlands_compressed.jpg';
        case 'reckoner':
            return 'static/images/230x130px-Reckoner_compressed.jpg';
        case 'turbine':
            return 'static/images/230x130px-Turbine_compressed.jpg';
        case 'metalworks':
            return 'static/images/230x130px-Metalworks_compressed.jpg';
        case 'sultry':
            return 'static/images/230x130px-Sultry_compressed.jpg';
        default:
            return name.split('_')[1];
    }
}
    


/*var streamStatus = new Worker("static/stream.js");

var startStream = function (region) {
    streamStatus.terminate(); // terminate previous worker
    delete streamStatus;

    streamStatus = new Worker("static/stream.js");  // setup a new worker
    streamStatus.onmessage = ((recv) => { renderPartial(recv.data); })

    streamStatus.postMessage(region);  // start the worker
}*/