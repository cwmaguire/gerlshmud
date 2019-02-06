/*
 Copyright (c) 2019, Chris Maguire <cwmaguire@gmail.com>

 Permission to use, copy, modify, and/or distribute this software for any
 purpose with or without fee is hereby granted, provided that the above
 copyright notice and this permission notice appear in all copies.

 THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

var socket;

function send(){
  socket.send(elem_value("command"));
  elem_clear("command");
}

function elem_value(elem_id){
  let elem = document.getElementById(elem_id);
  return elem.value;
}

function elem_clear(elem_id){
  let elem = document.getElementById(elem_id);
  elem.value = "";
}

function load(){
  socket = new WebSocket("ws://localhost:8081/");

  socket.onopen = function (event) {
    let div = document.getElementById('results');
    let canvas_ = document.createElement('CANVAS');
    canvas_.id = 'canvas1';
    div.appendChild(canvas_);
    let ctx = canvas_.getContext('2d');
    canvas_.width = '30';
    canvas_.height = '30';
    ctx.strokeStyle = 'green';
    ctx.fillStyle = 'green';
    ctx.lineWidth = '8px';
    ctx.beginPath();
    ctx.ellipse(15, 15, 14, 14, Math.PI * 2, 0, 2 * Math.PI);
    ctx.fill();
  };

  socket.onmessage = function (event) {
    let results = document.getElementById("results");
    let logDiv = document.createElement('DIV');
    logDiv.innerHTML = event.data;
    results.append(logDiv);
    results.scrollTop = results.scrollHeight;
  };
}
