"use strict";

const IMAGE_PATH = 'images/';

let filters = (
  [{id: 'cb_has_type',
    label: 'has type',
    filter: has_type,
    initial: true},

   {id: 'cb_not_link',
    label: 'not link',
    filter: not_link,
    initial: true},

   {id: 'cb_not_populate',
    label: 'not populate',
    filter: not_populate,
    initial: true}
  ]
);

let icons = {
  food: 'drumstick_icon.png',
  book: 'book_icon.png',
  body_part: 'hand_icon.png',
  person: 'person_icon.png',
  room: 'room_icon.png',
  clothing: 'shirt_icon.png',
  stat: 'stat_icon.png',
  weapon: 'weapons_icon.png',
  armor: 'armor_icon.png',
  exit: 'exit_icon.png',
  ammo: 'ammo_icon.png',
  technology: 'technology_icon.png',
  none: 'white_icon.png',
  unknown: 'question_mark_icon.png'
};

let eventColors = {
  populate: 'grey',
  link: 'purple'
}

function initial_filters(filters, filter){
  if(filter.initial){
    return filters.concat(filter.id);
  }else{
    return filters;
  }
}

function initial_load(){
  let initial = filters.reduce(initial_filters, []);
  load(initial);
}

function load(filterIds = []){
  let handlers = [];
  let logs = [];

  if(typeof(testLogs) != 'undefined'){
    logs = testLogs;
  }

  add_filters();

  let filtersToApply = filters.filter(f => filterIds.includes(f.id));
  filtersToApply.map(f => elem(f.id).checked = true)
  let filteredLogs = filtersToApply.reduce((logs, f) => logs.filter(f.filter), logs);

  for(let log of filteredLogs){
    handlers.push(add_log_line(log));
  }

  for(let [d, h1, h2] of handlers){
    h1(d.clientHeight);
    h2();
  }
}

function has_type(obj){
  return obj.hasOwnProperty('event_type');
}

function not_link(obj){
  return !obj.hasOwnProperty('event_type') || obj.event_type != 'link';
}

function not_populate(obj){
  return !obj.hasOwnProperty('event_type') || obj.event_type != 'populate';
}

function add_filters(initial){
  let filtersDiv = div();

  for(let filter of filters){
    let filterSpan = span();
    let label = span(filter.label);
    let checkbox = document.createElement('INPUT');
    checkbox.type = 'checkbox';
    checkbox.id = filter.id;
    checkbox.checked = initial && filter.initial;
    checkbox.addEventListener('change', filter_checkbox_change);

    filterSpan.appendChild(checkbox);
    filterSpan.appendChild(label);
    filtersDiv.appendChild(filterSpan);
  }
  document.body.appendChild(filtersDiv);
}

function filter_checkbox_change(event){
  let filterIds = filters.reduce(applied_filters, []);
  reload(filterIds);
}

function applied_filters(applied, filter){
  if(elem(filter.id).checked){
    return applied.concat(filter.id);
  }else{
    return applied;
  }
}

function reload(filterIds){
  clear();
  load(filterIds);
}

function clear(){
  while(document.body.childElementCount > 0){
    document.body.children[0].remove()
  }
}


function add_log_line(log, beforeOrAfter = 'after'){
  let logDiv = div();

  let eventSpan = span(undefined, 'event');

  add_log_text(logDiv, logDiv, log);

  add_stage(logDiv, log);
  let roomWidthListener = add_room(logDiv, log);
  add_image('event_source_icon', logDiv, log);
  add_image('event_target_icon', logDiv, log);
  add_pid('process', logDiv, log);
  let heightListener = add_handler(logDiv, log);

  add_maybe_pid('event_source', eventSpan, log);
  add_event_name(eventSpan, log);
  add_maybe_pid('event_target', eventSpan, log);
  logDiv.appendChild(eventSpan);

  add_result(logDiv, log);
  add_subscription(logDiv, log);
  add_message(logDiv, log);

  if(beforeOrAfter == 'after'){
    document.body.appendChild(logDiv);
  }else{
    let firstLog = document.body.children[4];
    document.body.insertBefore(logDiv, firstLog);

    remove_overflow();
  }

  return [logDiv, heightListener, roomWidthListener];
}

function add_stage(parent, log){
  let svg1;
  let stage = prop(log, 'stage')
  if(stage == 'attempt'){
    svg1 = svg_circle('#1010FF', 'black');
  }else if(stage == 'succeed'){
    svg1 = svg_circle('#10A010', 'black');
  }else{
    svg1 = svg_circle('#FFFFFF', 'black');
  }
  parent.appendChild(svg1);
}

function add_room(parent, log){
  let room = prop(log, 'room', 'n/a');
  if(room = 'undefined'){
    room = 'n/a';
  }
  let roomSpan = span(room, 'room');
  let roomTitleSpan = span('room', 'room_title');
  parent.appendChild(roomSpan);
  parent.appendChild(roomTitleSpan);
  let listener =
    function(){
      let roomWidth = roomSpan.offsetWidth;
      let left = roomWidth - 3;
      roomTitleSpan.style.left = -left;
    };
  return listener;
}

function add_image(key, parent, log){
  let image = img();
  let icon = prop(log, key, 'none');
  let filename;
  filename = icons[icon];
  let path = IMAGE_PATH + filename;
  image.src = path;
  image.style.height = '20px';
  image.style.width = '20px';
  parent.appendChild(image);
}

function add_log_process(parent, log){
  let logProcessChar1Span = span(prop(log, 'process', 'P'), 'log_process_1');
  let logProcessChar2Span = span(prop(log, 'process', '?'), 'log_process_2');
  parent.appendChild(logProcessChar1Span);
  parent.appendChild(logProcessChar2Span);
}

function add_handler(parent, log){
  let handler = prop(log, 'handler');
  let handlerSpan = span(handler, 'module');
  parent.appendChild(handlerSpan);

  let handlerFun;
  if(!handler){
    handlerSpan.innerHTML = 'n/a';
    //handlerSpan.style.width = '120px';
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

function add_maybe_pid(typeKey, parent, log){
  let value = prop(log, typeKey);
  if(is_pid(value)){
    add_pid(typeKey, parent, log);
  }else{
    parent.appendChild(span(value));
  }
}

function add_pid(typeKey, parent, log){
  let idKey = typeKey + '_id';
  let pidSpan = span();
  let idSpan = span(prop(log, idKey, '__'));

  let defaultPid = '<0.0.0>';
  let pid = prop(log, typeKey, defaultPid);
  let [charSpan1, charSpan2] = pid_char_spans(pid);

  pidSpan.className = 'process';

  pidSpan.appendChild(idSpan);
  pidSpan.appendChild(charSpan1);
  pidSpan.appendChild(charSpan2);

  parent.appendChild(pidSpan);
}

function pid_char_spans(pid){
  let span1;
  let span2;
  let pidNum1;
  let pidNum2;

  if(pid == '<0.0.0>'){
    span1 = span('_');
    span1.style.color = 'black';
    span2 = span('_');
    span2.style.color = 'black';
  } else {
    pidNum1 = parseInt(pid.split('.')[1]);
    pidNum2 = reverse_int(pidNum1);
    span1 = pid_span(pidNum1);
    span2 = pid_span(pidNum2);
  }
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
  let event = prop(log, 'event_type');
  let eventNameSpan = span(event);
  let color = eventColors[event];
  eventNameSpan.style.color = color;
  parent.appendChild(eventNameSpan);
}

function add_result(parent, log){
  let result = prop(log, 'result');
  let resultSpan = span(result);
  if(result == 'succeed'){
    resultSpan.className = 'succeed';
  }else if(result == 'fail'){
    resultSpan.className = 'fail';
  }else if(result == 'resend'){
    resultSpan.className = 'resend';
  }else if(result == 'broadcast'){
    resultSpan.className = 'broadcast';
  }
  parent.appendChild(resultSpan);
}

function add_subscription(parent, log){
  let shouldSub = prop(log, 'subscribe');
  let subscriptionSpan = span(shouldSub);
  if(shouldSub || shouldSub == 'true'){
    subscriptionSpan.className = 'should_subscribe';
  }
  parent.appendChild(subscriptionSpan);
}

function add_message(parent, log){
  let msg = message(prop(log, 'message'));
  let newMsg = prop(log, 'new_message');
  if(newMsg){
    msg += ' -> ' + newMsg;
  }
  let msgSpan = span(msg, 'message')
  parent.appendChild(msgSpan);
}

function add_log_text(parent, logDiv, log){
  let logMouseoverSpan = span('Log');
  logMouseoverSpan.style.fontSize = '8pt';
  let logTextDiv = div();
  logTextDiv.style.display = 'none';
  logTextDiv.style.fontSize = '8pt';
  logTextDiv.style.zIndex = '2';
  logTextDiv.style.position = 'fixed';
  logTextDiv.style.left = '55%';
  logTextDiv.style.background = 'white';

  for(let k in log){
    let d = div();
    d.innerText = k + ': ' + log[k];
    logTextDiv.appendChild(d);
  }
  let mouseover = (
    function(event){
      logTextDiv.style.display = 'block';
      logTextDiv.style.top = '10px';
    }
  )
  let mouseout = (
    function(event){
      logTextDiv.style.display = 'none';
    }
  )
  logMouseoverSpan.addEventListener('mouseover', mouseover);
  logMouseoverSpan.addEventListener('mouseout', mouseout);
  logDiv.appendChild(logMouseoverSpan);
  parent.appendChild(logTextDiv);
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
  return parseInt((i + '').split('').reverse().join(''))
}

function letter(int){
  return String.fromCharCode((int % 26) + 65);
}

function color_from_int(i){
  let red = i % 255;
  let green = (i >> 1) % 255
  let blue = (i >> 2) % 255

  return '#' + to_hex(red) + to_hex(green) + to_hex(blue);
}

function to_hex(i){
  var str = Math.round(i).toString(16);
  if(str.length == 1){
    return '0' + str;
  }else{
    return str;
  }
}

function is_pid(maybePid){
  let isPid =  maybePid.startsWith('<') && maybePid.endsWith('>');
  return isPid;
}

function elem(id){
  return document.getElementById(id);
}

function message(msgProp){
  let msg = '';
  if(Array.isArray(msg)){
    for(let e of msgProp){
      msg += e;
    }
  }else{
    msg = msgProp;
  }
  return msg;
}

function websocket_connect(){
  let socket = new WebSocket("ws://localhost:8081/log");

  socket.onopen = function (event) {
    let connectResult = elem('connect_result');
    connectResult.value = 'open'
    console.dir(event);
  };

  socket.onmessage = function (event) {
    let log = JSON.parse(event.data);
    add_log_line(log, 'before');
  };
}

function remove_overflow(){
  let nodes = document.body.childNodes;
  let length = nodes.length;
  if(nodes.length > 100){
    console.log('document.body.childNodes.length > 100');
    for(let i = length - 1; i > 100; i--){
      console.log('removing nodes[' + i + ']');
      nodes[i].remove();
    }
  }
}
