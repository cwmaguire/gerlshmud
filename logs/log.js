
function createClassCheckboxes(){
  //alert("Creating class checkboxes");
  var allDivs = nodeListToArray(document.getElementsByTagName("div"));
  var allSpans = nodeListToArray(document.getElementsByTagName("span"));
  var classes_ = concat(classes(allDivs), classes(allSpans));
  var checkboxes = map(checkbox, classes_);
  map(insert, checkboxes);
}

function insert(elem){
  var firstElem = document.body.firstChild;
  document.body.insertBefore(elem, firstElem);
}

function nodeListToArray(nodelist){
  var arr = [];
  for(var i = 0, n; n = nodelist[i]; ++i) arr.push(n);
  return arr
}

function class_(elem){
  return elem.className;
}

function classes(elems){
  return unique(map(class_, elems));
}

function checkbox(className){
  var div = document.createElement("div");
  var cb = document.createElement("input");
  cb.type = "checkbox";
  cb.id = className + "_checkbox";
  cb.checked = true;
  cb.onchange = function(event){
    //window.alert("Toggling visibility for " + className);
    var elems = nodeListToArray(document.getElementsByClassName(className));
    map(toggleVisibility, elems);
  }
  var label = document.createElement("label");
  label.innerHTML = className;
  div.appendChild(cb);
  div.appendChild(label);
  return div;
}

function toggleVisibility(elem){
  //alert("elem: " + elem.className + " display is: " + elem.style.display);
  if(elem.style.display == "none"){
    elem.style.display = "initial";
  }else{
    elem.style.display = "none";
  }
}
