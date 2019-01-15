"use strict";

for(let log of logs){
  console.log(log);
  let e = document.createElement("SPAN");
  e.innerHTML = log.process;
  document.body.appendChild(e);
}
