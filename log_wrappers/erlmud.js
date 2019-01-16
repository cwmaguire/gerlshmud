"use strict";

for(let log of logs){
  add_log_line(log);
}

function add_log_line(log){
  let logDiv = div();
  let roomSpan = span(prop(log, 'room'), 'room');

  let roomTitleSpan = span('room', 'room_title');

  let sourceImage = img();
  let targetImage = img();

  let logProcessChar1Span = span(prop(log, 'process'));
  let logProcessChar2Span = span(prop(log, 'process'));

  let handlerSpan = span('handler: ' + prop(log, 'module'));

  let eventParentSpan = span(log.type);

  let sourceSpan = span();
  let sourceNameSpan = span(prop(log, 'name'));
  let sourceProcChar1Span = span(prop(log, 'source'));
  let sourceProcChar2Span = span(prop(log, 'source'));

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
}

function prop(log, key){
  if(log.hasOwnProperty('props')){
    for(let [key, value] of log.props){
      if(key == key){
        console.log('prop(log, ' + key + ') -> ' + value);
        console.log(log.props);
        return value;
      }
    }
  }
  return '';
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
