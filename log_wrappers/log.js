var isCheckboxesCreated = false;

function createClassCheckboxes(){
  if(isCheckboxesCreated){
    return;
  }
  //alert("Creating class checkboxes");
  let allDivs = Array.from(document.getElementsByTagName("div"));
  let allSpans = Array.from(document.getElementsByTagName("span"));
  let classes_ = classes(allDivs).concat(classes(allSpans));
  let checkboxes = map(checkbox, classes_);
  checkboxes.forEach(insert);
  isCheckboxesCreated = true;
}

function insert(elem){
  let firstElem = document.body.firstElementChild;
  firstElem.appendChild(elem);
}

function class_(elem){
  return elem.className;
}

function classes(elems){
  return Array.from(new Set(elems.map(class_).filter(not_empty)))
}

function not_empty(str){
  return !str.trim().length == 0;
}

function checkbox(className){
  let div = document.createElement("div");
  let cb = document.createElement("input");
  cb.type = "checkbox";
  cb.id = className + "_checkbox";
  cb.checked = true;
  cb.onchange = function(event){
    let elems = Array.from(document.getElementsByClassName(className));
    elems.forEach(toggleVisibility);
  }
  let label = document.createElement("label");
  label.innerHTML = className;
  div.appendChild(cb);
  div.appendChild(label);
  div.className = "checkbox";
  return div;
}

function toggleVisibility(elem){
  if(elem.style.display == "none"){
    elem.style.display = "block";
  }else{
    elem.style.display = "none";
  }
}
