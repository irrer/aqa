// Reload the page when there is new data, indicated by
// a change in Checksum

var WebRefreshTime = 2000;
var checksum = 'none';
var baseUrl = 'empty';
var monthList = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];
var standardDateFormat = '%Y-%m-%dT%H:%M:%S';

function formatDate(date) { 
  return (date.getYear() + 1900) + ' ' + monthList[date.getMonth()] + ' ' + date.getDate();
};
    
function formatTime(date) { 
  return ("0" + date.getHours()).slice(-2) + ':' + ("0" + date.getMinutes()).slice(-2) ;
};
      
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

jsonhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    var aliasToRealList = JSON.parse(this.responseText);
    translateAliases(aliasToRealList);
  }
};
jsonhttp.open("GET", aliasToRealUrl, true);
jsonhttp.send();

function translateAliases(aliasToRealList) {
  var publicList = $( "[aqaalias]" ); // defines the attribute for showing real names

  var arMap = new Map();
  aliasToRealList.map(ar => arMap.set(ar.alias.trim(), ar.real.trim()));

  var p;

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
  

}