"use strict";

function load(){
  let handlers = [];

  for(let log of logs){
    handlers.push(add_log_line(log));
  }

  for(let [d, h] of handlers){
    h(d.clientHeight);
  }
}

function add_log_line(log){
  let logDiv = div();
  logDiv.style.border = '1px solid black';

  let eventSpan = span(log.type);
  eventSpan.className = 'event';

  add_stage(logDiv, log);
  add_room(logDiv, log);
  add_source_image(logDiv, log);
  add_target_image(logDiv, log);
  add_pid('process', logDiv, log);
  let heightListener = add_handler(logDiv, log);

  add_pid('source', eventSpan, log);
  add_event_name(eventSpan, log);
  add_pid('target', eventSpan, log);

  add_result(logDiv, log);
  add_subscription(logDiv, log);

  logDiv.appendChild(eventSpan);
  document.body.appendChild(logDiv);

  add_log_text(document.body, log);

  return [logDiv, heightListener];
}

function add_stage(parent, log){
  let svg1 = svg_circle('#FFFFFF', '#000000');
  parent.appendChild(svg1);
}

function add_room(parent, log){
  let roomSpan = span(prop(log, 'room', 'no room'), 'room');
  let roomTitleSpan = span('room', 'room_title');
  parent.appendChild(roomSpan);
  parent.appendChild(roomTitleSpan);
}

function add_source_image(parent, log){
  let sourceImage = img();
  parent.appendChild(sourceImage);
}

function add_target_image(parent, log){
  let targetImage = img();
  parent.appendChild(targetImage);
}

function add_log_process(parent, log){
  let logProcessChar1Span = span(prop(log, 'process', 'P'), 'log_process_1');
  let logProcessChar2Span = span(prop(log, 'process', '?'), 'log_process_2');
  parent.appendChild(logProcessChar1Span);
  parent.appendChild(logProcessChar2Span);
}

function add_handler(parent, log){
  let handler = prop(log, 'module');
  let handlerSpan = span(handler);
  handlerSpan.style.position = 'relative';
  handlerSpan.style.height = '20px';
  handlerSpan.style.border = '2px dashed purple';
  parent.appendChild(handlerSpan);

  let handlerFun;
  if(!handler){
    handlerSpan.style.width = '20px';
    handlerFun =
      function(parentHeight){
        let parentHeightInt = parseInt(parentHeight);
        let heightInt = parseInt(handlerSpan.style.height);
        let top = (parentHeightInt - heightInt) / 2 - 2;
        handlerSpan.style.top = top + 'px';
      }
  }else{
    handlerFun = function(){}
  }

  return handlerFun;
}

function add_pid(typeKey, parent, log){
  let nameKey = typeKey + '_name';
  let pidSpan = span();
  let nameSpan = span(prop(log, nameKey, 'no name'));

  let defaultPid = '<0.0.0>';
  let pid = prop(log, name, defaultPid);
  let [charSpan1, charSpan2] = pid_char_spans(pid);

  pidSpan.className = 'process';

  pidSpan.appendChild(nameSpan);
  pidSpan.appendChild(charSpan1);
  pidSpan.appendChild(charSpan2);

  parent.appendChild(pidSpan);
}

function pid_char_spans(pid){
  let pidNum1 = pid.split('.')[1];
  let pidNum2 = reverse_int(pidNum1);
  let span1 = pid_span(pidNum1);
  let span2 = pid_span(pidNum2);
  span1.className = 'log_process_1';
  span2.className = 'log_process_2';
  return [span1, span2];
}

function pid_span(i){
  let letter_ = letter(i);
  let color = color_from_int(i);
  let span_ = span(letter_);
  span_.style.color = color;
  return span_;
}

function add_event_name(parent, log){
  let event = prop(log, 'type');
  let eventNameSpan = span(event);
  parent.appendChild(eventNameSpan);
}

function add_result(parent, log){
  let resultSpan = span(prop(log, 'stage'));
  parent.appendChild(resultSpan);
}

function add_subscription(parent, log){
  let subscriptionSpan = span(prop(log, 'subscribe'));
  parent.appendChild(subscriptionSpan);
}

function add_log_text(parent, log){
  let logText = serialize(log);
  let plainDiv = div();
  plainDiv.innerText = logText;
  parent.appendChild(plainDiv);
}

function span(html, className){
  let span_ = document.createElement('SPAN');
  if(html){
    span_.innerHTML = html;
  }
  if(className){
    span_.className = className;
  }
  return span_;
}

function img(){
  return document.createElement('IMG');
}

function div(){
  return document.createElement('DIV');
}

function prop(log, key, def = ''){
  if(log.hasOwnProperty(key)){
    return log[key];
  }
  if(log.hasOwnProperty('props')){
    for(let [k, v] of log.props){
      if(k== key){
        return v;
      }
    }
  }
  return def;
}

function serialize(o){
  let str = '';
  for(let k in o){
    str += k + ': ' + o[k] + ', ';
  }
  return str;
}

function svg_circle(fill = '#FFFFFF', stroke){
  var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  var circle = document.createElementNS("http://www.w3.org/2000/svg", 'circle');

  svg.setAttribute("aria-hidden","true");
  svg.setAttribute('viewbox', '0 0 20 20');
  //svg.setAttribute('width', '20px');
  //svg.setAttribute('height', '20px');

  circle.setAttribute('cx', '10');
  circle.setAttribute('cy', '10');
  circle.setAttribute('fill', fill);
  if(stroke){
    circle.setAttribute('r', '7');
    circle.setAttribute('stroke', stroke);
    circle.setAttribute('stroke-width', 2);
  }else{
    circle.setAttribute('r', '9');
  }

  svg.appendChild(circle);
  return svg;
}

function reverse_int(i){
  return parseInt((123 + '').split('').reverse().join(''))
}

function letter(int){
  return String.fromCharCode((int % 26) + 65);
}

function color_from_int(i){
  let red = i % 255;
  let green = i >> 1 % 255
  let blue = i >> 2 % 255

  return '#' + to_hex(red) + to_hex(green) + to_hex(blue);
}

function to_hex(i){
  var str = Math.round(i).toString(16);
  if(str.length == 1){
    return "0" + str;
  }else{
    return str;
  }
}
