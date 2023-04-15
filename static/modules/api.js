
var getApiStatusByRegionPool = function(region, onSuccess, onError)
{
  $.ajax(
    { url: '/api/status/' + encodeURIComponent(region) + '/pool'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getApiStatusByRegionStream = function(region, onSuccess, onError)
{
  $.ajax(
    { url: '/api/status/' + encodeURIComponent(region) + '/stream'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getApiGraphicsByName = function(name, width, height, onSuccess, onError)
{
  $.ajax(
    { url: '/api/graphics/' + encodeURIComponent(name) + '' + '?width=' + encodeURIComponent(width) + '&height=' + encodeURIComponent(height)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
