// Reload the page when there is new data, indicated by
// a change in Checksum

var WebRefreshTime = 2000;
var checksum = 'none';
var baseUrl = 'empty';
var monthList = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ];
var standardDateFormat: '%Y-%m-%dT%H:%M:%S';

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
