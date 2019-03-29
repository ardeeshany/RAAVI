$(document).ready(function() {
  // document.domain = "raaviman.com"; 
  
  var userIDVariableName = parent.userID; 
  var userID = document.getElementById("userID"); 
  userID.value = userIDVariableName; 
  
  var usernameVariableName = parent.username; 
  var username = document.getElementById("username"); 
  username.value = usernameVariableName;
  
  var userhashVariableName = parent.userhash; 
  var userhash = document.getElementById("userhash"); 
  userhash.value = userhashVariableName;
});
Shiny.addCustomMessageHandler("redirecturl",
                              function(url) {
                                window.location = url;
                              }
);