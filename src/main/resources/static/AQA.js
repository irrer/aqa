// Reload the page when there is new data, indicated by
// a change in Checksum

var checksum = 'none';
var baseUrl = 'empty';
var monthList = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];
var standardDateFormat = '%Y-%m-%dT%H:%M:%S';


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

/* Wait this number of ms between refreshing the alias values. */
var aliasRefreshTime = 10;
/* Try refreshing the alias values this many times. */
var refreshAliasCount = 14;

var jsonhttp = new XMLHttpRequest();
var aliasToRealUrl = "/AnonymousTranslate/translateTable.json";  // defines the URL for getting the alias to real table

var aliasToRealList;
jsonhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      aliasToRealList = JSON.parse(this.responseText);
      translateAliases();
      showOutputHeader();
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
    aliasRefreshTime = aliasRefreshTime * 2;
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
  chart.axis.range(rng);
}

// ------------------------------------------------------------------------------------

/* Provide support for history (data graphed over time to show trends) charts. */

/* Keep track of whether mouse button 1 (leftmost) is up or down.  This is so that when the user
   drags the mouse vertically to move a history graph that it can be determined whether to scroll
   the graph or not.
*/

// True if button 1 is down.
var button1Down = undefined;

function initButton1State() {
  function setButton1State(e) {
    if (e.buttons === undefined) {
      button1Down = e.which === 1;
    }
    else {
      button1Down = e.buttons === 1;
    }
  }

  button1Down = false;
  document.body.onmousedown = setButton1State;
  document.body.onmousemove = setButton1State;
  document.body.onmouseup = setButton1State;
}

// initialize
setTimeout(function () {if (button1Down === undefined) initButton1State();},  500);
setTimeout(function () {if (button1Down === undefined) initButton1State();}, 1000);

/**
 * Modify the HTML where the chart is to be located.  Create the vertical scroll area
 * and the Reset and Show All buttons.  This has to be done before the chart is been
 * generated so that when it is generated it will bind to the proper HTML element.
 *
 * Also note that because the chart has not been generated yet, some of the functionality
 * can not be set up because the data from the chart is required.
 *
 * The following is where the maintenance record details section is inserted.
 *  <div id='@@details'></div>
 */
function insertVertHtml(name) {

  var vertHtml = `
    <div id='@@details'></div>
    <table id='@@Table' style='display: table; width: 100%;'>
      <tr>
        <td align='left'>
          <div id='@@'>Chart goes here</div>
        </td>
        <td width='30px' align='right'>
          <center>
            <div id='@@VertPane' style='width: 50px; height: 50px; border: 1px solid black;'> </div>
            <input id='@@ResetButton' type='button' value='Reset'     style='margin-top: 5px;' onclick='@@Var.vertReset()' title='Reset the Y range back to the original settings.'/>
            <input id='@@AllButton'   type='button' value='Show All'  style='margin-top: 5px; margin-bottom: 5px;' onclick='@@Var.vertAll()'   title='Set the Y range so that all data points can be viewed, including outliers.'/>
          </center>
        </td>
        <td width='30px' align='right'>
        </td>
      </tr>
    </table>
  `;

  var finalHtml = vertHtml.replaceAll("@@", name);
  var origElement = document.getElementById(name);
  origElement.id = name + "Container";
  origElement.innerHTML = finalHtml;
}

/*
 * Construct vertical control struct.  Define the functions to act on the chart.  None of these will work until
 * chart is actually defined.
 *
 * @param minY: Minimum Y value displayed.
 * @param maxY: Maximum Y value displayed.
 * @param name: Chart name.
 */
function constructVertControl(minY, maxY, name) {
  var vertCont = {};
  vertCont.minYOrig = minY;
  vertCont.maxYOrig = maxY;
  vertCont.name = name;
  vertCont.curMinY = minY;
  vertCont.curMaxY = maxY;
  vertCont.chart = undefined;

  // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  /*
   * Respond to a vertical scaling event.  Scaling is centered at the Y position
   * of the mouse in the vertPane box.
   */
  vertCont.vertRescale = function(event) {
    var zoomSave = vertCont.chart.zoom();
    event.preventDefault();
    var direction = 1;
    if (event.deltaY < 0) direction = -direction;

    var offsetY = parseFloat(event.offsetY);
    var elementHeight = parseFloat(event.target.style.height.replaceAll("px", ""));
    var fraction = offsetY / elementHeight;
    var range = vertCont.curMaxY - vertCont.curMinY;
    var change = range * (direction * 0.06);
    vertCont.curMaxY = vertCont.curMaxY + (change * fraction);
    vertCont.curMinY = vertCont.curMinY - (change * (1 - fraction));

    vertCont.chart.axis.max(vertCont.curMaxY);
    vertCont.chart.axis.min(vertCont.curMinY);
    vertCont.chart.zoom(zoomSave);
  }

  // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  /*
   * Respond to a vertical scrolling event.
   */
  vertCont.vertScroll = function(event) {
    if (button1Down && (event.movementY != 0)) {
      var zoomSave = vertCont.chart.zoom();
      var direction = 0.5 * event.movementY;

      var range = vertCont.curMaxY - vertCont.curMinY;
      var change = range * direction * 0.01;
      vertCont.curMaxY = vertCont.curMaxY + change;
      vertCont.curMinY = vertCont.curMinY + change;

      vertCont.chart.axis.max(vertCont.curMaxY);
      vertCont.chart.axis.min(vertCont.curMinY);
      vertCont.chart.zoom(zoomSave);
    }
  }

  // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  /**
   * Set the vertical scale back to the original settings when the chart
   * was loaded.
   */
  vertCont.vertReset = function() {
    vertCont.curMaxY = vertCont.maxYOrig;
    vertCont.curMinY = vertCont.minYOrig;
    vertCont.chart.axis.max(vertCont.curMaxY);
    vertCont.chart.axis.min(vertCont.curMinY);
  }

  // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  /**
   * Set the vertical scale so that all data points in the chart are shown.
   * Sometimes charts are initially scaled so that the outliers are off the
   * screen.  This will show outliers also.  If there are no outliers, then
   * this will have a similar effect to the vertReset function.
   */
  vertCont.vertAll = function() {
    vertCont.curMaxY = vertCont.allMaxY;
    vertCont.curMinY = vertCont.allMinY;
    vertCont.chart.axis.max(vertCont.curMaxY);
    vertCont.chart.axis.min(vertCont.curMinY);
  }

  vertCont.vertImageOff = function(event) {
    vertCont.vertPane.innerHTML = "";
  }

  vertCont.vertImageOn = function(event) {
     vertCont.vertPane.innerHTML = vertCont.vertPaneContent;
  }

  // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  return vertCont;
}

// ------------------------------------------------------------------------------------

/*
 * Initialize the vertCont struct with actual chart values.
 */
function initVertControl(vertCont, chart, name) {
  vertCont.chart = chart;
  var data = chart.data();
  var d = data[0].values[0].value;
  var maximumY = d;
  var minimumY = d;
  for (i = 0; i < data.length-1; i++) {  // the last one is maintenance records
    data[i].values.forEach(v => {
      if (v.value > maximumY) maximumY = v.value;
      if (v.value < minimumY) minimumY = v.value;
    })
  }

  vertCont.allMaxY = maximumY;
  vertCont.allMinY = minimumY;

  var vertPane = document.getElementById(name + "VertPane");
  var js = "vertPane.addEventListener('wheel'," + name + "Var.vertRescale);";
  eval(js);
  js = "vertPane.addEventListener('mousemove'," + name + "Var.vertScroll);";
  eval(js);

  js = "vertPane.addEventListener('mouseenter'," + name + "Var.vertImageOff);";
  eval(js);
  js = "vertPane.addEventListener('mouseleave'," + name + "Var.vertImageOn);";
  eval(js);

  vertPane.style.cursor = "n-resize";
  var table = document.getElementById(name + "Table");
  var resetButton = document.getElementById(name + "ResetButton");
  var allButton = document.getElementById(name + "AllButton");
  var buttonHeight = resetButton.offsetHeight + allButton.offsetHeight;
  var buttonWidth = Math.max(resetButton.offsetWidth, allButton.offsetWidth);

  var height = table.parentElement.offsetHeight - buttonHeight - 20;

  vertPane.style.height = height + "px";
  vertPane.style.width = buttonWidth + "px";
  vertCont.vertPaneContent = "<img style='height:" + (height-5) + "px;' src='/static/images/VerticalAdjust.png'/>";
  vertPane.innerHTML = vertCont.vertPaneContent;
  vertCont.vertPane = vertPane;
}

// ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------

/*
 * The following is the MR (Maintenance Record) section which supports the viewing, hiding, and
 * showing of maintenance records.
 */

function MRsetupVisibleList(info) {
  list = [];
  var i = 0;
  for (i = 0; i < info.maintenanceList.length; i++) {
    if (info.maintenanceList[i].visible) list.push(info.maintenanceList[i]);
  }
  info.visibleList = list;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/**
 * Given the index from C3, return the nth visible maintenance record.
 */
function MRindexToVisible(info, index) {
  var i = Math.min(Math.trunc(index / 2), info.visibleList.length-1);
  var j = info.visibleList[i];
  if (j === undefined) {
    console.log("Error: index: " + index + "  i: " + i);
  }
  return j;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRgetMaintenanceDateList(info) {
    var list = [ 'maintenanceDateList' ];
    var i = 0;
    for (i = 0; i < info.visibleList.length; i++) {
        var m = info.visibleList[i];
        if (m.visible) {
          list.push(m.date);
          list.push(m.date);
        }
    }
    return list;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRgetMaintenanceRecordList(info) {
  var list = [ 'MaintenanceRecord' ];
  var i = 0;
  for (i = 0; i < info.visibleList.length; i++) {
    var m = info.visibleList[i];
    if (m.visible) {
      list.push(info.maintenanceLo);
      list.push(info.maintenanceHi);
    }
  }
  return list;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function getColumns(info) {
  var cols = [];
  var i = 0;
  for (i = 0; i < info.values.length; i++) {
    cols.push(info.values[i]);
  }

  cols.push(MRgetMaintenanceDateList(info));
  cols.push(MRgetMaintenanceRecordList(info));
  return cols;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRprefix(chartId) {
  var content =
  '<div style="border:1px solid lightgray;">\n' +
  '  <table class="table table-responsive" style="table-layout: auto; border-bottom:1px solid lightgray;">\n';
  return content;
}

function MRsearch(chartId) {
  console.log(chartId + " hiya"); // TODO
}

function MRsuffix(chartId) {
  var text =
    '  </table>\n' +
    '    <button onclick=\'MRclose("'+ chartId  + 'details")\' style="margin:10px;">Close</button>\n' +
    '</div>\n';
  return text;
}

function MRhtmlCheckbox(chartId, mr) {
  var func = '"MRhide(' + "'" + chartId + "', " + mr.pk + ')"';
  var oninput = "oninput=" + func;
  var id = 'id="' + chartId + 'showHide' + mr.pk + '"';
  var checked = "checked='" + mr.visible.toString() + "'";
  var text = '<td title="Show/Hide" width="25" style="border-bottom: 1px solid lightgray;"> <input type="checkbox" ' + id + ' ' + oninput + ' ' + checked + '"/> </td>\n';
  return text;
}

function MRhtmlDate(mr) {
    var date = new Date(mr.date);
    var t = formatDate(date) + " " + formatTime(date);
    var text = '<td width="150" style="border-bottom: 1px solid lightgray;">' + t + '</td>\n';
    return text;
}

function MRhtmlSummary(chartId, mr) {
    var id = chartId + "expand" + mr.pk;
    var box = '<b style="background:' + mr.color + ';height:8;width:8;margin:3px;"> &nbsp; &nbsp; </b>'

    var text =
    '<td style="border-bottom: 1px solid lightgray;">\n' +
    '  <button type="button" class="btn btn-info" data-toggle="collapse" data-target="#' + id + '" title="Click to expand details">&#x1F50D;</button> ' + box + " " + mr.category + " : " +  mr.summary + '\n' +
    '  <div id="' + id + '" class="collapse">\n' +
    '    <pre style="overflow:auto; display:flex;">\n' +
           mr.description +
    '    </pre>\n' +
    '  </div>\n' +
    '</td>\n';
    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRfindNear(info, dateText) {
  var interval_ms = 8 * 60 * 60 * 1000;
  var date = new Date(dateText);
  var lo = date.getTime() - interval_ms;
  var hi = date.getTime() + interval_ms;

  var mrList = [];

  var i = 0;
  for (i = 0; i < info.maintenanceList.length; i++) {
    var mr = info.maintenanceList[i];
    var d = (new Date(mr.date)).getTime();
    if ((d > lo) && (d < hi))
      mrList.push(mr);
  }
  return mrList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRmakeDetails(info, d, i) {
  var mr = MRindexToVisible(info, d.index);
  var mrList = MRfindNear(info, mr.date);
  var i = 0;
  var rowHtml = "";
  for (i = 0; i < mrList.length; i++) {
     rowHtml = rowHtml +
       '<tr>\n' +
       MRhtmlCheckbox(info.chartId, mrList[i]) +
       MRhtmlDate(mrList[i]) +
       MRhtmlSummary(info.chartId, mrList[i]) +
       "</tr>\n";
  }
  var text = MRprefix(info.chartId) + rowHtml + MRsuffix(info.chartId);
  return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/**
 * Respond to a button push and toggle the visible state of the maintenance event.
 */
function MRhide(chartId, pkText) {
  var info = eval(chartId + "info;");
  var pk = Number(pkText);
  var mr = undefined;
  var i = 0;
  for (i = 0; i < info.maintenanceList.length; i++) {
    if (info.maintenanceList[i].pk === pk)
      mr = info.maintenanceList[i];
  }

  var id = chartId + 'showHide' + mr.pk
  var checkbox = document.getElementById(id);
  var zoomSave = info.chart.zoom();
  mr.visible = checkbox.checked;
  MRsetupVisibleList(info);
  info.chart.load({columns: getColumns(info)});
  info.chart.zoom(zoomSave);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRselectMaintenanceRecord(info, d, i) {
  var dets = document.getElementById(info.chartId + "details")
  dets.innerHTML = MRmakeDetails(info, d, i);
  dets.style.display = "inline";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

function MRclose(id) {
  document.getElementById(id).style.display = "none";
}

// ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

var currentLocation = window.location;

function showOutputHeader() {
  var attrName = "headerOutputPK";
  var headerData = document.getElementById(attrName);

  if ((headerData != null) && headerData.hasAttribute(attrName)) {
    // headerData.setAttribute("returnUrl", currentLocation);
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (this.readyState == 4 && this.status == 200) {
        headerData.innerHTML = this.responseText;
      }
    };
    var url = headerData.getAttribute(attrName) + "&returnUrl=" + window.location;
    xhttp.open("GET", url, true);
    xhttp.send();
  }
}

