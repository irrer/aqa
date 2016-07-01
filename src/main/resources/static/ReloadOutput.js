// Reload the page when there is new data, indicated by
// a change in Checksum

var WebRefreshTime = 2000;
var checksum = 'none';
var baseUrl = 'empty';

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
