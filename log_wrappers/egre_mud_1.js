"use strict";

const IMAGE_PATH = 'images/';

let filters = (
  [{id: 'cb_has_type',
    label: 'has type',
    hover: 'Does this event have the "event_type" field?',
    filter: has_type,
    initial: true},

   {id: 'cb_not_link',
    label: 'not link',
    hover: 'Only show events that are not linking objects to each other.',
    filter: not_link,
    initial: true},

   {id: 'cb_not_populate',
    label: 'not populate',
    hover: 'Only show events that are not "populate" events.',
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
  spell: 'spell_icon.png',
  effect: 'effect_icon.png',
  resource: 'meter_icon.png',
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
  console.clear();
  let handlers = [];
  let logs = [];

  if(typeof(testLogs) != 'undefined'){
    logs = testLogs;
  }

  add_filters();

  let filtersToApply = filters.filter(f => filterIds.includes(f.id));
  filtersToApply.map(f => elem(f.id).checked = true)
  let filteredLogs = filtersToApply.reduce((logs, f) => logs.filter(f.filter), logs);

  let procIds = new Map();

  for(let log of filteredLogs){
    storeProcIds(procIds, log);
  }

  for(let log of filteredLogs){
    handlers.push(add_log_line(procIds, log));
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
    let labelSpan = span(filter.label);
    let checkbox = document.createElement('INPUT');
    checkbox.type = 'checkbox';
    checkbox.id = filter.id;
    checkbox.checked = initial && filter.initial;
    checkbox.addEventListener('change', filter_checkbox_change);

    filterSpan.appendChild(checkbox);
    filterSpan.appendChild(labelSpan);
    filtersDiv.appendChild(hoverSpan(filterSpan, filter.hover));
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

function storeProcIds(procIds, log){
  for(let [k, v] of Object.entries(log)){
    if(k.endsWith('_id')){
      let idKey = k;
      let id = v;
      let pidKey = idKey.substring(0, idKey.length - 3);
      let pid = log[pidKey];
      procIds.set(pid, id);
    }
  }
}


function add_log_line(procIds, log, beforeOrAfter = 'after'){
  let logDiv = div();

  let eventSpan = span(undefined, 'event');

  add_log_text(logDiv, logDiv, log);

  add_stage(logDiv, log);
  let roomWidthListener = add_room(logDiv, log);
  add_image('event_source_icon', logDiv, log);
  add_image('event_target_icon', logDiv, log);
  add_pid('process', logDiv, log);
  let heightListener = add_rules_module(logDiv, log);

  add_result(logDiv, log);
  add_subscription(logDiv, log);
  add_message(logDiv, log, procIds);

  if(beforeOrAfter == 'after'){
    add_to_bottom(logDiv);
  }else{
    add_to_top(logDiv);
  }

  return [logDiv, heightListener, roomWidthListener];
}

function add_to_bottom(elem){
    document.body.appendChild(elem);
}

function add_to_top(elem){
    let firstLog = document.body.children[4];
    document.body.insertBefore(elem, firstLog);
    remove_overflow();
}

function add_stage(parent, log){
  let svg1;
  let hoverText;
  let stage = prop(log, 'stage')
  if(stage == 'attempt'){
    svg1 = svg_circle('#1010FF', 'black');
    hoverText = 'This is an attempt of an event. Event success or failure will be broadcast.';
  }else if(stage == 'succeed'){
    svg1 = svg_circle('#10A010', 'black');
    hoverText = 'This event has succeeded; in other words, the event happened.'
  }else{
    svg1 = svg_circle('#FFFFFF', 'black');
    hoverText = 'This is not an event attempt or event success, so maybe just a manually logged event.'
  }
  //svg1.className = 'hoverable'
  //parent.appendChild(svg1);

  let hoverableSpan = span(undefined, 'hoverable');
  hoverableSpan.appendChild(svg1);
  parent.appendChild(hoverableSpan);

  let hoverSpan = span(hoverText, 'hoverLabel');
  parent.appendChild(hoverSpan);
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

function add_rules_module(parent, log){
  let rawRulesModule = prop(log, 'rules_module');
  let hoverText = 'Raw "rules_module" property: ' + rawRulesModule;
  let rules;

  if(!rawRulesModule){
    rules = 'n/a';
    hoverText = 'no "rules_module" property found in log properties';
  } else if(rawRulesModule == 'no_rules_module'){
    rules = 'none';
  } else {
    rules = rawRulesModule.split('_').slice(1).join(' ');
  }

  let rulesSpan = span(rules, 'module');

  parent.appendChild(hoverSpan(rulesSpan, hoverText));

  let handlerFun =
    function(parentHeight){
      let parentHeightInt = parseInt(parentHeight);
      let heightInt = parseInt(rulesSpan.style.height);
      let top = (parentHeightInt - heightInt) / 2 - 2;
      rulesSpan.style.top = top + 'px';
    };

  return handlerFun;
}

function add_pid(typeKey, parent, log){
  let idKey = typeKey + '_id';
  let pidSpan = span();
  let idSpan = span(prop(log, idKey, '__'));

  let defaultPid = '<0.0.0>';
  let pid = prop(log, typeKey, defaultPid);

  pidSpan.className = 'process';

  pidSpan.appendChild(idSpan);

  parent.appendChild(pidSpan);
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

function add_message(parent, log, procIds){
  const msg = message(prop(log, 'message'));
  const newMsg = prop(log, 'new_message');

  if(msg){
    add_partitioned_message(msg, procIds, parent);
  }else{
    parent.appendChild(span('&lt;no message&gt;', 'message'));
  }

  if(newMsg){
    parent.appendChild(span(' -> '));
    add_partitioned_message(newMsg, procIds, parent);
  }
}

function add_partitioned_message(msg, procIds, elem){
  let msgSpan = span(undefined, 'message')
  augment_pids_with_ids(msg, procIds).map(x => msgSpan.appendChild(span(x)));
  elem.appendChild(msgSpan);
}

function augment_pids_with_ids(msg, procIds){
  return flatten_array(msg).map(x => maybe_pid_id_string(x, procIds));
}

function flatten_array(array){
  let newArray = array.slice();
  while(has_array(newArray)){
    newArray = newArray.flat();
  }
  return newArray;
}

function has_array(array){
  return !array.every(x => !Array.isArray(x));
}

function maybe_pid_id_string(maybePid, procIds){
  const regex = /<\d{1,3}\.\d{1,3}\.\d{1,3}>/;
  if(('' + maybePid).match(regex)){
    let pid = maybePid;
    let id = procIds.get(pid);
    return `${id}<br>${pid}`;
  }else{
    return maybePid;
  }
}

function add_log_text(parent, logDiv, log){
  let logMouseoverSpan = span('log', 'hoverable');
  logMouseoverSpan.style.fontSize = '8pt';
  let logTextDiv = div();
  logTextDiv.className = 'hoverBox';
  logTextDiv.style.fontSize = '12pt';
  //logTextDiv.style.width = '50%';
  logTextDiv.style.top = '10px';
  logTextDiv.style.left = '10%';

  for(let k in log){
    let d = div();
    d.innerText = k + ': ' + log[k];
    if(k == 'pid' || k == 'pid_id'){
      d.style.backgroundColor = '#A0A000';
    }
    logTextDiv.appendChild(d);
  }

  let span1 = span();
  span1.appendChild(logMouseoverSpan);
  span1.appendChild(logTextDiv);
  logDiv.appendChild(span1);
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

function hoverSpan(element, hoverText){
  let hoverableSpan = span(undefined, 'hoverable');
  hoverableSpan.appendChild(element);
  let hoverSpan = span(hoverText, 'hoverLabel');

  let siblingSpan = span();
  siblingSpan.appendChild(hoverableSpan);
  siblingSpan.appendChild(hoverSpan);
  return siblingSpan;
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
  let str = Math.round(i).toString(16);
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
    // TODO actually build up a running list of proc -> ID mappings
    let emptyProcIds = new Map();
    add_log_line(emptyProcIds, log, 'before');
  };
}

function remove_overflow(){
  let maxNodes = 300;
  let nodes = document.body.childNodes;
  let length = nodes.length;
  if(nodes.length > maxNodes){
    console.log('document.body.childNodes.length > ' + maxNodes);
    for(let i = length - 1; i > maxNodes; i--){
      console.log('removing nodes[' + i + ']');
      nodes[i].remove();
    }
  }
}
