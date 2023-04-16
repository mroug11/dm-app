export { _log, updateStatusFromJSON } 

/**
 * @param {boolean} enabled 
 * @param  {...any} args 
 */
function _log (enabled, ...args) { if (enabled) console.log(...args); }
let LOG = (...args) => {_log(false, ...args)}

function isoTimeToStr (isotime) {
    if (isotime) {
        var localTime = new Date(isotime);
    } else {
        var localTime = new Date();
    }
    return localTime.getHours() + ':' + localTime.getMinutes();
}

/**
 * Update a part of the server-status-container from JSON data
 * 
 * @param {any} json JSON formatted data containing updated values
 * @param {HTMLDivElement} statusElem Server element to update
 * 
 * @returns 
 */
function updateStatusFromJSON(json, statusElem) {
    if (!json.addr || !json.port) return; // ill-formed update packet; probably a keepalive 

    const statusInfo = statusElem.getElementsByClassName("server-status-info")[0]

    if (json.name) {
        let name = statusElem.getElementsByClassName("server-name-banner")[0];
        name.innerHTML = `<a href="steam://connect/${json.addr}:${json.port}">${json.name}</a>`;
    }

    if (json.map) {  // TODO: make sure the graphic is in cache, or else download and cache it
        statusInfo.setAttribute("style", `background-image: url(${mapNameToUri(json.map)})`);

        let started = statusInfo.getElementsByTagName("time")[0];
        started.innerHTML = isoTimeToStr(json.started);
    }

    let players = statusInfo.getElementsByClassName("status-players")[0];
    if (json.players || !players.innerHTML) { 
        LOG("updating players: " + json.players); 
        players.innerHTML = json.players; 
    }

    let capacity = statusInfo.getElementsByClassName("status-capacity")[0]; 
    if (json.capacity || !capacity.innerHTML) { 
        LOG("updating capacity: " + json.capacity); 
        capacity.innerHTML = `/ ${json.capacity}`;
    }

    let queued = statusInfo.getElementsByClassName("status-queue")[0];
    if (json.queued || !queued.innerHTML) { 
        LOG("updating queued: " + json.queued); 
        queued.innerHTML = json.queued; 
    }
}

const mapNameToUri = function(name) {
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