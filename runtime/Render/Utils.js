var ElmRuntime;
(function (ElmRuntime) {
    (function (Render) {
        (function (Utils) {
            function newElement(elementType) {
                var e = document.createElement(elementType);
                e.style.padding = "0";
                e.style.margin = "0";
                return e;
            }
            Utils.newElement = newElement;

            function addTo(container, elem) {
                container.appendChild(elem);
            }
            Utils.addTo = addTo;

            function extract(c) {
                if (c._3 === 1) {
                    return 'rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')';
                }
                return 'rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')';
            }
            Utils.extract = extract;

            function addTransform(style, trans) {
                style.transform = trans;
                style.msTransform = trans;
                style.MozTransform = trans;
                style.webkitTransform = trans;
                style.OTransform = trans;
            }
            Utils.addTransform = addTransform;

            function removeTransform(style) {
                style.transform = 'none';
                style.msTransform = 'none';
                style.MozTransform = 'none';
                style.webkitTransform = 'none';
                style.OTransform = 'none';
            }
            Utils.removeTransform = removeTransform;

            var List = Elm.Native.List.make({});
            Utils.fromList = List.toArray;
            Utils.fromString = function (s) {
                return s;
            };
            Utils.toString = function (s) {
                return s;
            };
            Utils.eq = Elm.Native.Utils.make({}).eq;
        })(Render.Utils || (Render.Utils = {}));
        var Utils = Render.Utils;
    })(ElmRuntime.Render || (ElmRuntime.Render = {}));
    var Render = ElmRuntime.Render;
})(ElmRuntime || (ElmRuntime = {}));
