"use strict";

for(let log of logs){
  add_log_line(log);
}

function add_log_line(log){
  let logDiv = div();

  let svg1 = svg_circle('#FFFFFF', '#000000');
  logDiv.appendChild(svg1);

  let roomSpan = span(prop(log, 'room', 'no room'), 'room');

  let roomTitleSpan = span('room', 'room_title');

  let sourceImage = img();
  let targetImage = img();

  let logProcessChar1Span = span(prop(log, 'process', 'P'), 'log_process_1');
  let logProcessChar2Span = span(prop(log, 'process', '?'), 'log_process_2');

  let handlerSpan = span(prop(log, 'module', 'no handler'));

  let eventParentSpan = span(log.type);

  let sourceSpan = span();
  let sourceNameSpan = span(prop(log, 'name', 'no name'));
  let sourceProcChar1Span = span(prop(log, 'source', 'no source'));
  let sourceProcChar2Span = span(prop(log, 'source', 'no source'));

  let eventNameSpan = span();

  let targetSpan = span();
  let targetNameSpan = span(prop(log, 'name'));
  let targetProcChar1Span = span(prop(log, 'target'));
  let targetProcChar2Span = span(prop(log, 'target'));

  let resultSpan = span(prop(log, 'stage'));
  let subscriptionSpan = span(prop(log, 'subscribe'));

  sourceSpan.appendChild(sourceNameSpan);
  sourceSpan.appendChild(sourceProcChar1Span);
  sourceSpan.appendChild(sourceProcChar2Span);

  targetSpan.appendChild(targetNameSpan);
  targetSpan.appendChild(targetProcChar1Span);
  targetSpan.appendChild(targetProcChar2Span);

  eventParentSpan.appendChild(sourceSpan);
  eventParentSpan.appendChild(eventNameSpan);
  eventParentSpan.appendChild(targetSpan);

  logDiv.appendChild(roomSpan);
  logDiv.appendChild(roomTitleSpan);
  logDiv.appendChild(sourceImage);
  logDiv.appendChild(targetImage);
  logDiv.appendChild(logProcessChar1Span);
  logDiv.appendChild(logProcessChar2Span);
  logDiv.appendChild(handlerSpan);
  logDiv.appendChild(eventParentSpan);

  document.body.appendChild(logDiv);

  let plainDiv = div();
  let logText = serialize(log);
  plainDiv.innerText = logText;
  document.body.appendChild(plainDiv);
}

function prop(log, key, def = ''){
  if(log.hasOwnProperty('props')){
    for(let [k, v] of log.props){
      if(k== key){
        //console.log('prop(log, ' + k+ ') -> ' + v);
        //console.log(log.props);
        return v;
      }
    }
  }
  return def;
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
