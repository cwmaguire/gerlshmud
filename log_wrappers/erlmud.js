"use strict";

const IMAGE_PATH = 'images/';

let filters = (
  [{id: 'cb_has_type',
    label: 'has type',
    filter: has_type,
    initial: false},

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


function add_log_line(log){
  let logDiv = div();

  let eventSpan = span();
  eventSpan.className = 'event';

  add_stage(logDiv, log);
  let roomWidthListener = add_room(logDiv, log);
  add_image('event_source_icon', logDiv, log);
  add_image('event_target_icon', logDiv, log);
  add_pid('process', logDiv, log);
  let heightListener = add_handler(logDiv, log);

  add_pid('event_source', eventSpan, log);
  add_event_name(eventSpan, log);
  add_pid('event_target', eventSpan, log);
  logDiv.appendChild(eventSpan);

  add_result(logDiv, log);
  add_subscription(logDiv, log);

  document.body.appendChild(logDiv);

  // Keep this here in case I need to see what the props for an event are.
  // I should make this a hover box.
  add_log_text(document.body, log);

  return [logDiv, heightListener, roomWidthListener];
}

function add_stage(parent, log){
  let svg1 = svg_circle('#FFFFFF', '#000000');
  parent.appendChild(svg1);
}

function add_room(parent, log){
  let roomSpan = span(prop(log, 'room', 'n/a'), 'room');
  let roomTitleSpan = span('room', 'room_title');
  parent.appendChild(roomSpan);
  parent.appendChild(roomTitleSpan);
  let listener =
    function(){
      let roomWidth = roomSpan.offsetWidth;
      let left = roomWidth - 5;
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
  let handler = prop(log, 'module');
  let handlerSpan = span(handler, 'module');
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
  let resultSpan = span(prop(log, 'result'));
  parent.appendChild(resultSpan);
}

function add_subscription(parent, log){
  let subscriptionSpan = span(prop(log, 'subscribe'));
  parent.appendChild(subscriptionSpan);
}

function add_log_text(parent, log){
  for(let k in log){
    let d = div();
    d.innerText = k + ': ' + log[k];
    parent.appendChild(d);
  }
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

function elem(id){
  return document.getElementById(id);
}
