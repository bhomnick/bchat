/* Thanks https://github.com/brendonh */

(function($) {

  $.comet = function(key, callback) {
    var timestamp = (new Date()).getTime();
    $.comet.callbacks[key] = callback;
  };

  $.comet.init = function(baseUrl, readyCallback) {
    $.comet.callbacks = {};
    $.comet.baseUrl = baseUrl;
    var timeStamp = new Date().getTime();
    $.getJSON(baseUrl + "/get_client?t=" + timeStamp, function(json) {
      $.comet.cid = json.cid;
      if (readyCallback) { readyCallback(json.cid); }
      $.comet.poll();
    });
  };

  $.comet.poll = function() {
    $.ajax($.comet.baseUrl + "/poll", {
      data: {cid: $.comet.cid}, 
      success: $.comet.messages,
      error: $.comet.pollError,
      type: 'POST',
      dataType: 'json'
    });
  };
  
  $.comet.pollError = function(jqXHR, textStatus, errorThrown) {
    if(jqXHR.status >= 400) {
      if (console && console.debug) {
        console.debug("Poll failed: " + jqXHR.responseText);
        console.debug(errorThrown);
      }
    }
  };

  $.comet.messages = function(data, textStatus, jqXHR) {
    if(jqXHR.status != 204) {
      $.each(data, function(i, obj) {
        var callback = $.comet.callbacks[obj.key];
        if (callback) {
          try { callback(obj); }
          catch (e) { 
            if (console && console.debug) {
              console.debug("Oh noes: " + e);
            }
          }
        }
      });
    }
    $.comet.poll();
  };

})(jQuery);
