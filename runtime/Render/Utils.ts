module ElmRuntime.Render.Utils {

export function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
}

export function addTo(container, elem) {
    container.appendChild(elem);
}

export function extract(c) {
    if (c._3 === 1) { return 'rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')'; }
    return 'rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')';
}

export function addTransform(style, trans) {
  style.transform       = trans;
  style.msTransform     = trans;
  style.MozTransform    = trans;
  style.webkitTransform = trans;
  style.OTransform      = trans;
}

export function removeTransform(style) {
  style.transform       = 'none';
  style.msTransform     = 'none';
  style.MozTransform    = 'none';
  style.webkitTransform = 'none';
  style.OTransform      = 'none';
}

var List = Elm.Native.List.make({});
export var fromList = List.toArray;
export var fromString = function(s) { return s; };
export var toString = function(s) { return s; };
export var eq = Elm.Native.Utils.make({}).eq;

}
