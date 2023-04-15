export {_log}

/**
 * @param {boolean} enabled 
 * @param  {...any} args 
 */
const _log = function(enabled, ...args) { if (enabled) console.log(...args); }