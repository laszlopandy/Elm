module ElmRuntime {
'use strict';

export var Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

export var counter = 0;
export function guid() { return counter++; }

export function use(M) {
    if (typeof M === 'function') M = M();
    return M;
}

function isAlive(input) {
    if (!('defaultNumberOfKids' in input)) return true;
    var len = input.kids.length;
    if (len === 0) return false;
    if (len > input.defaultNumberOfKids) return true;
    var alive = false;
    for (var i = len; i--; ) {
        alive = alive || isAlive(input.kids[i]);
    }
    return alive;
}

export function filterDeadInputs(inputs) {
    var temp = [];
    for (var i = inputs.length; i--; ) {
        if (isAlive(inputs[i])) temp.push(inputs[i]);
    }
    return temp;
}

// define the draw function
var vendors = ['ms', 'moz', 'webkit', 'o'];
for (var i = 0; i < vendors.length && !window.requestAnimationFrame; ++i) {
    window.requestAnimationFrame = window[vendors[i]+'RequestAnimationFrame'];
    window.cancelAnimationFrame  = window[vendors[i]+'CancelAnimationFrame'] ||
                                   window[vendors[i]+'CancelRequestAnimationFrame'];
}

export var draw;
if (window.requestAnimationFrame && window.cancelAnimationFrame) {
    var previous = 0;
    draw = function(callback) {
        window.cancelAnimationFrame(previous);
        previous = window.requestAnimationFrame(callback);
    };
} else {
    draw = function(callback) { callback(); };
}

}