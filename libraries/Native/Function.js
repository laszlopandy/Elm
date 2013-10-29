var Elm;
(function (Elm) {
    function F2(fun) {
        var wrapper = function (a) {
            return function (b) {
                return fun(a, b);
            };
        };
        wrapper.arity = 2;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F2 = F2;

    function F3(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return fun(a, b, c);
                };
            };
        };
        wrapper.arity = 3;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F3 = F3;

    function F4(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return fun(a, b, c, d);
                    };
                };
            };
        };
        wrapper.arity = 4;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F4 = F4;

    function F5(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return fun(a, b, c, d, e);
                        };
                    };
                };
            };
        };
        wrapper.arity = 5;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F5 = F5;

    function F6(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return fun(a, b, c, d, e, f);
                            };
                        };
                    };
                };
            };
        };
        wrapper.arity = 6;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F6 = F6;

    function F7(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return fun(a, b, c, d, e, f, g);
                                };
                            };
                        };
                    };
                };
            };
        };
        wrapper.arity = 7;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F7 = F7;

    function F8(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return function (h) {
                                        return fun(a, b, c, d, e, f, g, h);
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
        wrapper.arity = 8;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F8 = F8;

    function F9(fun) {
        var wrapper = function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return function (h) {
                                        return function (i) {
                                            return fun(a, b, c, d, e, f, g, h, i);
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
        wrapper.arity = 9;
        wrapper.func = fun;
        return wrapper;
    }
    Elm.F9 = F9;

    function A2(fun, a, b) {
        return fun.arity === 2 ? fun.func(a, b) : fun(a)(b);
    }
    Elm.A2 = A2;
    function A3(fun, a, b, c) {
        return fun.arity === 3 ? fun.func(a, b, c) : fun(a)(b)(c);
    }
    Elm.A3 = A3;
    function A4(fun, a, b, c, d) {
        return fun.arity === 4 ? fun.func(a, b, c, d) : fun(a)(b)(c)(d);
    }
    Elm.A4 = A4;
    function A5(fun, a, b, c, d, e) {
        return fun.arity === 5 ? fun.func(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
    }
    Elm.A5 = A5;
    function A6(fun, a, b, c, d, e, f) {
        return fun.arity === 6 ? fun.func(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
    }
    Elm.A6 = A6;
    function A7(fun, a, b, c, d, e, f, g) {
        return fun.arity === 7 ? fun.func(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
    }
    Elm.A7 = A7;
    function A8(fun, a, b, c, d, e, f, g, h) {
        return fun.arity === 8 ? fun.func(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
    }
    Elm.A8 = A8;
    function A9(fun, a, b, c, d, e, f, g, h, i) {
        return fun.arity === 9 ? fun.func(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
    }
    Elm.A9 = A9;
})(Elm || (Elm = {}));
