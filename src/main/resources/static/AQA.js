// Reload the page when there is new data, indicated by
// a change in Checksum

var checksum = 'none';
var baseUrl = 'empty';
var monthList = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];
var standardDateFormat = '%Y-%m-%dT%H:%M:%S';

/* Wait this number of ms between refreshing the alias values. */
var aliasRefreshTime = 500;
/* Try refreshing the alias values this many times. */
var refreshAliasCount = 20;


/** Support for history charts. */
function formatDate(date) { 
  return (date.getYear() + 1900) + ' ' + monthList[date.getMonth()] + ' ' + date.getDate();
};
    
/** Support for history charts. */
function formatTime(date) {
  return ("0" + date.getHours()).slice(-2) + ':' + ("0" + date.getMinutes()).slice(-2) ;
};

/*
 * As analysis is being performed, periodically update the web page to get the latest status.
 */
var WebRefreshTime = 4000;

function watchChecksum() {
  $.ajax({
    url : baseUrl,
    success : function(result) {
      if (checksum == result) {
        setTimeout(watchChecksum, WebRefreshTime);
      } else {
        location.reload();
      }
    },
    error : function(result) {
      setTimeout(watchChecksum, WebRefreshTime);
    }
  });
}

function reloadOn(outputPK, initialChecksum) {
  baseUrl = "/ViewOutput?outputPK=" + outputPK + "&checksum="
      + initialChecksum;
  checksum = initialChecksum;
  setTimeout(watchChecksum, WebRefreshTime);
}

$(document).ready(function() {
  $.ajaxSetup({
    cache : false
  });
  jQuery('time.timeago').timeago();
});



/*
 * Translate all the aliased names in the current page to real names.
 *  
 * Retrieve the list of aliased entities (machines, users, 
 * etc.) that the current user is allowed to view along with 
 * their real names as a table.  Find all HTML elements with 
 * the special attribute "aqaalias", and replace their contents 
 * (the alias) with the real name.
 *  
 * If the contents of an element do not match a real name, then 
 * ignore it.  This is usually because the user is viewing a 
 * page with entities that they should not be allowed to see. 
 *  
 * Finally, set or append to the title of the element its alias 
 * so the user can easily find it.
 */
var jsonhttp = new XMLHttpRequest();
var aliasToRealUrl = "/AnonymousTranslate/translateTable.json";  // defines the URL for getting the alias to real table

var aliasToRealList;
jsonhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      aliasToRealList = JSON.parse(this.responseText);
      translateAliases();
  }
};

// If this is a reference to the static area, then do not get the translateTable.json .  It is actually a bad thing to get
// it, because public users (not logged in, so no credentials) get an annoying little prompt to log in, which they can
// ignore, but is a little confusing and definitely unnecessary.
if (window.location.pathname.toLowerCase().indexOf("/static/") == -1) {
  jsonhttp.open("GET", aliasToRealUrl, true);
  jsonhttp.send();
}

function translateAliases() {
  var publicList = $( "[aqaalias]" ); // defines the attribute for showing real names

  var arMap = new Map();

  var p;

  for (p = 0; p < aliasToRealList.length; p++) {
    var ar = aliasToRealList[p];
    arMap.set(ar.alias.trim(), ar.real.trim());
  }

  for (p = 0; p < publicList.length; p++) {
    var alias = publicList[p].innerHTML.trim();
    var isInputText = false;
    
    // determine if this is a text form input field.  If so, then set the 'value' attribute instead of the inner HTML
    if (publicList[p].hasAttribute("value") &&
      (publicList[p].tagName.toLowerCase().localeCompare("input") == 0) &&
      publicList[p].hasAttribute("type") &&
      (publicList[p].getAttribute("type").toLowerCase().localeCompare("text") == 0)
      ) {
        alias = publicList[p].getAttribute("value"); 
        isInputText = true;
    }

    var real = arMap.get(alias);
    if (real != null) {
      if (isInputText) {
          publicList[p].setAttribute("value", real);
      } else {
          publicList[p].innerHTML = real;
      }

      // modify the 'title' attribute to show the user what the alias is.  If there is already a title, then append it.
      var title = publicList[p].getAttribute("title");
      var showAlias = "Alias: " + alias;
      if (title == null) {
        publicList[p].setAttribute("title", showAlias);
      }
      else {
        publicList[p].setAttribute("title", title + " " + showAlias);
      }
    }
    
  }

  if (refreshAliasCount > 0) {
    refreshAliasCount = refreshAliasCount - 1;
    setTimeout(translateAliases, aliasRefreshTime);
  }

}


/**
 * If there is an element with dynamic content, then load it.  The element content should be the URL.
 */
function loadDynamicContent(id) {
  var doc = document.getElementById(id);
  if (doc !== null) {

    // To more closely follow HTML guidelines, the URL should be specified in an attribute.  To maintain backwards
    // compatibility the document content is used if the href is not specified.  Going forward, href should be used.
    var dynUrl = doc.innerHTML.trim();
    var href = doc.getAttribute("href");
    if (href !== null) dynUrl = href;

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (this.readyState == 4 && this.status == 200) {
        doc.innerHTML = this.responseText;
      }
    };
    xhttp.open("GET", dynUrl, true);
    xhttp.send();
  }
}

// Try appending 0-9 as the content's id.
$(document).ready(function() {
  for (i = 0; i < 10; i++) {
    loadDynamicContent("DynamicContent" + i.toString());
  }
});



// make a sorted list of all distinct dates in a history chart
function getHistoryChartDateList(chart) {
  var rng = chart.axis.range();
  var dtList = [];
  for (const x of chart.data()) {
      for (const t of x.values) {
        if (dtList.indexOf(t.x) === -1) dtList.push(t.x)
      }
  }
  dtList.sort(function(a, b) {
    return a.getTime() - b.getTime();
  });

  return dtList;
}

// change which part of the history chart to look at
function moveHistoryChart(chart, dateList, start, count) {

  var len = dateList.length;
  var c = (count * ((dateList.length - 4) / 100.0)).toFixed(0);
  c = Math.max(3, c);
  c = Math.min(len-1, c); // actual number of items to show

  // get index of first item to show
  var mn = ((start / 100.0) * (dateList.length-c)).toFixed(0);
  var mn = Math.max(0, mn);

  // get index of last item to show
  var mx = Math.min(mn + c, dateList.length - 1);
  // mn = Math.max(mx - c, 0);

  var rng = chart.axis.range();
  rng.min.x = dateList[mn];
  rng.max.x = dateList[mx];
  // console.log(mn + " : " + mx);
  chart.axis.range(rng);
}
