
function submitQuery () {
  offsetQuery(0);
  return false;
}

function offsetQuery (offset) {
  processQuery($("q").value, offset);
  return false;
}

function processQuery (query, offset) {
  process(query, offset);
  return false;
}

function process (query, offset) {
  $("throbber").toggle();
  refreshRequired = true;
  new Ajax.Request ("/ajax/search?q=" + encodeURIComponent(query) + "&o=" + offset,
    {
     method:'get',
     onSuccess: function(transport) {
       c = getContentAs(transport, "json");
       $("status").update(c.status);
       $("cloud").update(c.cloud);
       $("documents").update(c.documents);
       $("toppm").update(c.toppm);
       $("pages").update(c.pages);
/*       if (window.location.pathName != "/index/search") {
         window.location.pathName = "/index/search"
         window.location.search = "?q=" + encodeURIComponent(query) + "&o=" + offset;
       }*/
       $("throbber").toggle();
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
