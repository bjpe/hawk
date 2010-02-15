
function submitQuery () {
  processQuery();
  return false;
}

function processQuery () {
  var query = $("q").value;
  new Ajax.Request ("/ajax/search?q=" + query,
    {
     method:'get',
     onSuccess: function(transport) {
       c = getContentAs(transport, "json");
       alert('Text:' + c + " " + c.q + "-" + c.offset);
     },
     onFailure: function() { alert('Something went wrong ...'); }
    }
  );
}

function getContentAs(t, type) {
  switch (type) {
    case "text": return t.responseText || null;
    case "xml": return t.responseXML || null;
    case "json": return evalJson(t);
    default: return null;
  }
}

function evalJson(t) {
  try { 
    return eval( '(' + t.responseText + ')' );
  } catch (e) {return null}
}
