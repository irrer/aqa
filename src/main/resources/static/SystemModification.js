/** Update statistics for SystemModification. */
var sysModUrl='/admin/SystemModificationUpdate?latest=true';
function SetSystemModification() {
$.ajax({url:sysModUrl,
  success:function(result) {
    document.getElementById("SystemModification").innerHTML = result;
  },
  error:function(result){
    ;
  }
});
}
setTimeout(SetSystemModification, 500);
