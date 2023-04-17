export { sendRequest }

import { _log } from "./helper.js";

let LOG = (...args) => {_log(true, ...args)}

function sendRequest() {

    const myHeaders = new Headers();
    myHeaders.append("Content-Type", "application/json");

    let req = new Object();
    req.threshold = 5;
    req.servers = ["104.153.108.21:27015", "202.61.192.41:27045","dm.sappho.io:28315"] //
    req.inqueue = true;

    LOG("sending request body ", req);
    const request = new Request("/api/queue/join", {
        method: "POST",
        headers: myHeaders,
        body: JSON.stringify(req),
    });
    
    fetch(request).then(response => response.json())
                  .then(response => {
                        LOG("request was responded with ", response);
                  })
}