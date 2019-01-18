"use strict";

for(let log of logs){
  add_log_line(log);
}

function add_log_line(log){
  let logDiv = div();

  add_stage(logDiv, log);
  add_room(logDiv, log);
  add_source_image(logDiv, log);
  add_target_image(logDiv, log);
  add_log_process(logDiv, log);
  add_handler(logDiv, log);

  let eventSpan = span(log.type);
  add_source(eventSpan, log);
  add_event_name(eventSpan, log);
  add_target(eventSpan, log);

  add_result(logDiv, log);
  add_subscription(logDiv, log);

  logDiv.appendChild(eventSpan);
  document.body.appendChild(logDiv);

  add_log_text(document.body, log);
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
  let handlerSpan = span(prop(log, 'module', 'no handler'));
  parent.appendChild(handlerSpan);
}

function add_source(parent, log){
  let sourceSpan = span();
  let sourceNameSpan = span(prop(log, 'name', 'no name'));
  let sourceProcChar1Span = span(prop(log, 'source', 'no source'));
  let sourceProcChar2Span = span(prop(log, 'source', 'no source'));

  sourceSpan.appendChild(sourceNameSpan);
  sourceSpan.appendChild(sourceProcChar1Span);
  sourceSpan.appendChild(sourceProcChar2Span);

  parent.appendChild(sourceSpan);
}

function add_target(parent, log){
  let targetSpan = span();
  let targetNameSpan = span(prop(log, 'name'));
  let targetProcChar1Span = span(prop(log, 'target'));
  let targetProcChar2Span = span(prop(log, 'target'));

  targetSpan.appendChild(targetNameSpan);
  targetSpan.appendChild(targetProcChar1Span);
  targetSpan.appendChild(targetProcChar2Span);

  parent.appendChild(targetSpan);
}

function add_event_name(parent, log){
  let eventNameSpan = span();
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
