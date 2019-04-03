$(document).ready(function() {
  // document.domain = "raaviman.com"; 
  
  var userIDVariableName = parent.userID; 
  var userID = document.getElementById("mod_body-login-userID"); 
  userID.value = userIDVariableName; 
  
  var usernameVariableName = parent.username; 
  var username = document.getElementById("mod_body-login-username"); 
  username.value = usernameVariableName;
  
  var userhashVariableName = parent.userhash; 
  var userhash = document.getElementById("mod_body-login-userhash"); 
  userhash.value = userhashVariableName;
});
Shiny.addCustomMessageHandler("redirecturl",
                              function(url) {
                                window.location = url;
                              }
);