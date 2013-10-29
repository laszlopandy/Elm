var Elm;
(function (Elm) {
    (function (List) {
    })(Elm.List || (Elm.List = {}));
    var List = Elm.List;
})(Elm || (Elm = {}));

var Elm;
(function (Elm) {
    (function (Native) {
        (function (List) {
            List.values;
            function make(elm) {
                elm.Native = elm.Native || {};
                elm.Native.List = elm.Native.List || {};
                if (elm.Native.List.values)
                    return elm.Native.List.values;
                if ('values' in Elm.Native.List)
                    return elm.Native.List.values = Elm.Native.List.values;

                var Utils = Elm.Native.Utils.make(elm);

                // TODO: Improve Nil handling
                // We can change places like:  if (xs.ctor === '[]') ... to if (xs === Nil) ...
                // but only if we're confident Nil can only be defined once.
                // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
                // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
                // So, this can only be fixed when modules initialisation is also fixed.
                // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
                // (on firefox 19)
                var Nil = { ctor: '[]' };

                // using freeze for every cons would be nice but is a huge (9x on firefox 19)
                // performance penalty
                function Cons(hd, tl) {
                    return { ctor: "::", _0: hd, _1: tl };
                }

                function throwError(f) {
                    throw new Error("Function '" + f + "' expects a non-empty list!");
                }

                function toArray(xs) {
                    var out = [];
                    while (xs.ctor !== '[]') {
                        out.push(xs._0);
                        xs = xs._1;
                    }
                    return out;
                }

                function fromArray(arr) {
                    var out = Nil;
                    for (var i = arr.length; i--;) {
                        out = Cons(arr[i], out);
                    }
                    return out;
                }

                function range(lo, hi) {
                    var lst = Nil;
                    if (lo <= hi) {
                        do {
                            lst = Cons(hi, lst);
                        } while(hi-- > lo);
                    }
                    return lst;
                }

                function append(xs, ys) {
                    if (xs.isText)
                        return Utils.txt(xs.concat(ys));
                    if (typeof xs === "string")
                        return xs.concat(ys);
                    if (xs.ctor === '[]') {
                        return ys;
                    }
                    var root = Cons(xs._0, Nil);
                    var curr = root;
                    xs = xs._1;
                    while (xs.ctor !== '[]') {
                        curr._1 = Cons(xs._0, Nil);
                        xs = xs._1;
                        curr = curr._1;
                    }
                    curr._1 = ys;
                    return root;
                }

                function head(v) {
                    return v.ctor === '[]' ? throwError('head') : v._0;
                }
                function tail(v) {
                    return v.ctor === '[]' ? throwError('tail') : v._1;
                }

                function last(xs) {
                    if (xs.ctor === '[]') {
                        throwError('last');
                    }
                    var out = xs._0;
                    while (xs.ctor !== '[]') {
                        out = xs._0;
                        xs = xs._1;
                    }
                    return out;
                }

                function map(f, xs) {
                    var arr = [];
                    while (xs.ctor !== '[]') {
                        arr.push(f(xs._0));
                        xs = xs._1;
                    }
                    return fromArray(arr);
                }

                // f defined similarly for both foldl and foldr (NB: different from Haskell)
                // ie, foldl : (a -> b -> b) -> b -> [a] -> b
                function foldl(f, b, xs) {
                    var acc = b;
                    while (xs.ctor !== '[]') {
                        acc = Elm.A2(f, xs._0, acc);
                        xs = xs._1;
                    }
                    return acc;
                }

                function foldr(f, b, xs) {
                    var arr = toArray(xs);
                    var acc = b;
                    for (var i = arr.length; i--;) {
                        acc = Elm.A2(f, arr[i], acc);
                    }
                    return acc;
                }

                function foldl1(f, xs) {
                    return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
                }

                function foldr1(f, xs) {
                    if (xs.ctor === '[]') {
                        throwError('foldr1');
                    }
                    var arr = toArray(xs);
                    var acc = arr.pop();
                    for (var i = arr.length; i--;) {
                        acc = Elm.A2(f, arr[i], acc);
                    }
                    return acc;
                }

                function scanl(f, b, xs) {
                    var arr = toArray(xs);
                    arr.unshift(b);
                    var len = arr.length;
                    for (var i = 1; i < len; ++i) {
                        arr[i] = Elm.A2(f, arr[i], arr[i - 1]);
                    }
                    return fromArray(arr);
                }

                function scanl1(f, xs) {
                    if (xs.ctor === '[]') {
                        throwError('scanl1');
                    }
                    return scanl(f, xs._0, xs._1);
                }

                function filter(pred, xs) {
                    var arr = [];
                    while (xs.ctor !== '[]') {
                        if (pred(xs._0)) {
                            arr.push(xs._0);
                        }
                        xs = xs._1;
                    }
                    return fromArray(arr);
                }

                function length(xs) {
                    var out = 0;
                    while (xs.ctor !== '[]') {
                        out += 1;
                        xs = xs._1;
                    }
                    return out;
                }

                function member(x, xs) {
                    while (xs.ctor !== '[]') {
                        if (Utils.eq(x, xs._0))
                            return true;
                        xs = xs._1;
                    }
                    return false;
                }

                function reverse(xs) {
                    return fromArray(toArray(xs).reverse());
                }

                function concat(xss) {
                    if (xss.ctor === '[]')
                        return xss;
                    var arr = toArray(xss);
                    var xs = arr[arr.length - 1];
                    for (var i = arr.length - 1; i--;) {
                        xs = append(arr[i], xs);
                    }
                    return xs;
                }

                function all(pred, xs) {
                    while (xs.ctor !== '[]') {
                        if (!pred(xs._0))
                            return false;
                        xs = xs._1;
                    }
                    return true;
                }

                function any(pred, xs) {
                    while (xs.ctor !== '[]') {
                        if (pred(xs._0))
                            return true;
                        xs = xs._1;
                    }
                    return false;
                }

                function zipWith(f, xs, ys) {
                    var arr = [];
                    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
                        arr.push(Elm.A2(f, xs._0, ys._0));
                        xs = xs._1;
                        ys = ys._1;
                    }
                    return fromArray(arr);
                }

                function zip(xs, ys) {
                    var arr = [];
                    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
                        arr.push(Utils.Tuple2(xs._0, ys._0));
                        xs = xs._1;
                        ys = ys._1;
                    }
                    return fromArray(arr);
                }

                function sort(xs) {
                    function cmp(a, b) {
                        var ord = Elm.A2(Utils.compare, a, b).ctor;
                        return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
                    }
                    return fromArray(toArray(xs).sort(cmp));
                }

                function nth(xs, n) {
                    return toArray(xs)[n];
                }

                function take(n, xs) {
                    var arr = [];
                    while (xs.ctor !== '[]' && n > 0) {
                        arr.push(xs._0);
                        xs = xs._1;
                        --n;
                    }
                    return fromArray(arr);
                }

                function drop(n, xs) {
                    while (xs.ctor !== '[]' && n > 0) {
                        xs = xs._1;
                        --n;
                    }
                    return xs;
                }

                function repeat(n, x) {
                    var arr = [];
                    var pattern = [x];
                    while (n > 0) {
                        if (n & 1)
                            arr = arr.concat(pattern);
                        n >>= 1, pattern = pattern.concat(pattern);
                    }
                    return fromArray(arr);
                }

                function join(sep, xss) {
                    if (sep.isText)
                        return Utils.txt(toArray(xss).join(sep));
                    if (typeof sep === 'string')
                        return toArray(xss).join(sep);
                    if (xss.ctor === '[]')
                        return Nil;
                    var s = toArray(sep);
                    var out = toArray(xss._0);
                    xss = xss._1;
                    while (xss.ctor !== '[]') {
                        out = out.concat(s, toArray(xss._0));
                        xss = xss._1;
                    }
                    return fromArray(out);
                }

                Elm.Native.List.values = {
                    Nil: Nil,
                    Cons: Cons,
                    cons: Elm.F2(Cons),
                    toArray: toArray,
                    fromArray: fromArray,
                    range: range,
                    append: append,
                    head: head,
                    tail: tail,
                    last: last,
                    map: Elm.F2(map),
                    foldl: Elm.F3(foldl),
                    foldr: Elm.F3(foldr),
                    foldl1: Elm.F2(foldl1),
                    foldr1: Elm.F2(foldr1),
                    scanl: Elm.F3(scanl),
                    scanl1: Elm.F2(scanl1),
                    filter: Elm.F2(filter),
                    length: length,
                    member: Elm.F2(member),
                    reverse: reverse,
                    concat: concat,
                    all: Elm.F2(all),
                    any: Elm.F2(any),
                    zipWith: Elm.F3(zipWith),
                    zip: Elm.F2(zip),
                    sort: sort,
                    nth: Elm.F2(nth),
                    take: Elm.F2(take),
                    drop: Elm.F2(drop),
                    repeat: Elm.F2(repeat),
                    join: Elm.F2(join)
                };
                return elm.Native.List.values = Elm.Native.List.values;
            }
            List.make = make;
        })(Native.List || (Native.List = {}));
        var List = Native.List;
    })(Elm.Native || (Elm.Native = {}));
    var Native = Elm.Native;
})(Elm || (Elm = {}));
