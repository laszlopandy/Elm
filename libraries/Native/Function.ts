module Elm{

export interface F2Func {
  (a:any) : { (b:any): any };
  func: { (a:any, b:any): any };
  arity: number;
}

export function F2(fun: (a:any, b:any) => any):F2Func {
  var wrapper:any = function(a) { return function(b) { return fun(a,b) } }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

export function F3(fun: (a:any, b:any, c:any) => any) {
  var wrapper:any = function(a) {
    return function(b) { return function(c) { return fun(a,b,c) }}
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

export function F4(fun: (a:any, b:any, c:any, d:any) => any) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a,b,c,d) }}}
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

export function F5(fun) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

export function F6(fun) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return fun(a,b,c,d,e,f) }}}}}
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

export function F7(fun) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

export function F8(fun) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) {return fun(a,b,c,d,e,f,g,h)}}}}}}}
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

export function F9(fun) {
  var wrapper:any = function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) { return function(i) {
        return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

export function A2(fun:F2Func,a,b) {
  return fun.arity === 2 ? fun.func(a,b) : fun(a)(b);
}
export function A3(fun,a,b,c) {
  return fun.arity === 3 ? fun.func(a,b,c) : fun(a)(b)(c);
}
export function A4(fun,a,b,c,d) {
  return fun.arity === 4 ? fun.func(a,b,c,d) : fun(a)(b)(c)(d);
}
export function A5(fun,a,b,c,d,e) {
  return fun.arity === 5 ? fun.func(a,b,c,d,e) : fun(a)(b)(c)(d)(e);
}
export function A6(fun,a,b,c,d,e,f) {
  return fun.arity === 6 ? fun.func(a,b,c,d,e,f) : fun(a)(b)(c)(d)(e)(f);
}
export function A7(fun,a,b,c,d,e,f,g) {
  return fun.arity === 7 ? fun.func(a,b,c,d,e,f,g) : fun(a)(b)(c)(d)(e)(f)(g);
}
export function A8(fun,a,b,c,d,e,f,g,h) {
  return fun.arity === 8 ? fun.func(a,b,c,d,e,f,g,h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
export function A9(fun,a,b,c,d,e,f,g,h,i) {
  return fun.arity === 9 ? fun.func(a,b,c,d,e,f,g,h,i)
                         : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
}