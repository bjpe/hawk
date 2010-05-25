
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
       $("result").update("");
       $("result").insert({bottom: new Element('div', {id: 'status'})});
       $("result").insert({bottom: new Element('div', {id: 'toppm'})});
       $("result").insert({bottom: new Element('div', {id: 'cloud'})});
       $("result").insert({bottom: new Element('div', {id: 'documents'})});
       $("result").insert({bottom: new Element('div', {id: 'pages'})});
       $("status").update(c.status);
       $("toppm").update(c.toppm);
       $("cloud").update(c.cloud);
       $("documents").update(c.documents);
       $("pages").update(c.pages);
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
