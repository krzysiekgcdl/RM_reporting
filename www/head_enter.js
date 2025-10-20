pressbtn = function(){

    // click the log in button
    document.getElementById('auth-go_auth').click();
};
window.onload = function() {

// password input field
const field = document.getElementById('auth-user_pwd');

// add a function that preempts the enter key press
field.addEventListener('keydown',  
function(e) {

    if (e.keyCode == 13) {
    // prevent sending the key event
    e.preventDefault();
   
    // delay activating the login button for 400 ms. adjust time as needed
    setTimeout(pressbtn,400);
    };

});
}

function newpass(){
  maindiv = document.getElementById("forgotpassdiv");
  maindiv.innerHTML = "";
  
  const p = document.createElement("p");
  p.innerText = "Write your new password in the box below."
  var mi = document.createElement("input");
  mi.setAttribute('type', 'text');
  mi.placeholder = "Write your new password"
  mi.id = "forgotpass_newpass"
  
  var x = document.createElement("button");
  var t = document.createTextNode("Update password");
  x.appendChild(t);
  x.onclick = function(){Shiny.setInputValue("forgotpass_updatepass", document.getElementById("forgotpass_newpass").value);};
  
  
  maindiv.innerHTML = p.outerHTML + mi.outerHTML + '<br>';
  maindiv.appendChild(x);
}

function forgotpass(){
  const myArray = new Uint32Array(1);
  crypto.getRandomValues(myArray);

  Shiny.setInputValue("forgot_pw", myArray[0]);
  
  const p = document.createElement("p");
  p.innerText = "Write your username in the box below, then click send. A code will be sent to the email address associated with the account, then put this code in the appropriate box."
  var mi = document.createElement("input");
  mi.setAttribute('type', 'text');
  mi.placeholder = "Write your username"
  mi.id = "forgotpass_username"
  
  var x = document.createElement("button");
  var t = document.createTextNode("Send code");
  x.appendChild(t);
  x.onclick = function(){Shiny.setInputValue("forgotpass_emailbutton", document.getElementById("forgotpass_username").value);};
  
  var mi2 = document.createElement("input");
  mi2.setAttribute('type', 'text');
  mi2.placeholder = "Write your code"
  mi2.id = "forgotpass_code"
  
  var x2 = document.createElement("button");
  var t2 = document.createTextNode("Verify code");
  x2.appendChild(t2);
  x2.onclick = function(){
    //verify code then update div to allow a new password input
    if (myArray[0] == document.getElementById("forgotpass_code").value) {
      newpass();
    } else {
      console.log("wrong code");
    }
  }
  var br = document.createElement("br");
  var br2 = document.createElement("br");

  maindiv = document.getElementById("forgotpassdiv")
  maindiv.innerHTML = p.outerHTML + mi.outerHTML; //+ '<br>' + mi2.outerHTML + '<br>' + x2.outerHTML;
  maindiv.appendChild(x);
  maindiv.appendChild(br);
  maindiv.appendChild(br2);
  maindiv.appendChild(mi2);
  maindiv.appendChild(x2);
}

