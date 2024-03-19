// output/Config/foreign.js
var apiServer = globalThis.apiServer;

// output/Control.Semigroupoid/index.js
var semigroupoidFn = {
  compose: function(f) {
    return function(g) {
      return function(x) {
        return f(g(x));
      };
    };
  }
};

// output/Control.Category/index.js
var identity = function(dict) {
  return dict.identity;
};
var categoryFn = {
  identity: function(x) {
    return x;
  },
  Semigroupoid0: function() {
    return semigroupoidFn;
  }
};

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};
var $$const = function(a) {
  return function(v) {
    return a;
  };
};

// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Type.Proxy/index.js
var $$Proxy = /* @__PURE__ */ function() {
  function $$Proxy2() {
  }
  ;
  $$Proxy2.value = new $$Proxy2();
  return $$Proxy2;
}();

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var $$void = function(dictFunctor) {
  return map(dictFunctor)($$const(unit));
};
var functorArray = {
  map: arrayMap
};

// output/Control.Apply/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
var apply = function(dict) {
  return dict.apply;
};
var applySecond = function(dictApply) {
  var apply1 = apply(dictApply);
  var map4 = map(dictApply.Functor0());
  return function(a) {
    return function(b) {
      return apply1(map4($$const(identity2))(a))(b);
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var when = function(dictApplicative) {
  var pure12 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (v) {
        return v1;
      }
      ;
      if (!v) {
        return pure12(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var liftA1 = function(dictApplicative) {
  var apply2 = apply(dictApplicative.Apply0());
  var pure12 = pure(dictApplicative);
  return function(f) {
    return function(a) {
      return apply2(pure12(f))(a);
    };
  };
};

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
};
var bind = function(dict) {
  return dict.bind;
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};

// output/Data.Bounded/foreign.js
var topInt = 2147483647;
var bottomInt = -2147483648;
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq3) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq3 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqBooleanImpl = refEq;
var eqIntImpl = refEq;

// output/Data.Symbol/index.js
var reflectSymbol = function(dict) {
  return dict.reflectSymbol;
};

// output/Record.Unsafe/foreign.js
var unsafeGet = function(label4) {
  return function(rec) {
    return rec[label4];
  };
};

// output/Data.Eq/index.js
var eqRowNil = {
  eqRecord: function(v) {
    return function(v1) {
      return function(v2) {
        return true;
      };
    };
  }
};
var eqRecord = function(dict) {
  return dict.eqRecord;
};
var eqRec = function() {
  return function(dictEqRecord) {
    return {
      eq: eqRecord(dictEqRecord)($$Proxy.value)
    };
  };
};
var eqInt = {
  eq: eqIntImpl
};
var eqBoolean = {
  eq: eqBooleanImpl
};
var eq = function(dict) {
  return dict.eq;
};
var eq2 = /* @__PURE__ */ eq(eqBoolean);
var eqRowCons = function(dictEqRecord) {
  var eqRecord1 = eqRecord(dictEqRecord);
  return function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictEq) {
        var eq3 = eq(dictEq);
        return {
          eqRecord: function(v) {
            return function(ra) {
              return function(rb) {
                var tail = eqRecord1($$Proxy.value)(ra)(rb);
                var key = reflectSymbol2($$Proxy.value);
                var get2 = unsafeGet(key);
                return eq3(get2(ra))(get2(rb)) && tail;
              };
            };
          }
        };
      };
    };
  };
};
var notEq = function(dictEq) {
  var eq3 = eq(dictEq);
  return function(x) {
    return function(y) {
      return eq2(eq3(x)(y))(false);
    };
  };
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();
var eqOrdering = {
  eq: function(v) {
    return function(v1) {
      if (v instanceof LT && v1 instanceof LT) {
        return true;
      }
      ;
      if (v instanceof GT && v1 instanceof GT) {
        return true;
      }
      ;
      if (v instanceof EQ && v1 instanceof EQ) {
        return true;
      }
      ;
      return false;
    };
  }
};

// output/Data.Ord/index.js
var eqRec2 = /* @__PURE__ */ eqRec();
var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
var ordRecordNil = {
  compareRecord: function(v) {
    return function(v1) {
      return function(v2) {
        return EQ.value;
      };
    };
  },
  EqRecord0: function() {
    return eqRowNil;
  }
};
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var compareRecord = function(dict) {
  return dict.compareRecord;
};
var ordRecord = function() {
  return function(dictOrdRecord) {
    var eqRec1 = eqRec2(dictOrdRecord.EqRecord0());
    return {
      compare: compareRecord(dictOrdRecord)($$Proxy.value),
      Eq0: function() {
        return eqRec1;
      }
    };
  };
};
var compare = function(dict) {
  return dict.compare;
};
var ordRecordCons = function(dictOrdRecord) {
  var compareRecord1 = compareRecord(dictOrdRecord);
  var eqRowCons2 = eqRowCons(dictOrdRecord.EqRecord0())();
  return function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      var eqRowCons1 = eqRowCons2(dictIsSymbol);
      return function(dictOrd) {
        var compare3 = compare(dictOrd);
        var eqRowCons22 = eqRowCons1(dictOrd.Eq0());
        return {
          compareRecord: function(v) {
            return function(ra) {
              return function(rb) {
                var key = reflectSymbol2($$Proxy.value);
                var left = compare3(unsafeGet(key)(ra))(unsafeGet(key)(rb));
                var $95 = notEq2(left)(EQ.value);
                if ($95) {
                  return left;
                }
                ;
                return compareRecord1($$Proxy.value)(ra)(rb);
              };
            };
          },
          EqRecord0: function() {
            return eqRowCons22;
          }
        };
      };
    };
  };
};

// output/Data.Bounded/index.js
var top = function(dict) {
  return dict.top;
};
var boundedInt = {
  top: topInt,
  bottom: bottomInt,
  Ord0: function() {
    return ordInt;
  }
};
var bottom = function(dict) {
  return dict.bottom;
};

// output/Data.Semigroup/foreign.js
var concatArray = function(xs) {
  return function(ys) {
    if (xs.length === 0)
      return ys;
    if (ys.length === 0)
      return xs;
    return xs.concat(ys);
  };
};

// output/Data.Semigroup/index.js
var semigroupArray = {
  append: concatArray
};
var append = function(dict) {
  return dict.append;
};

// output/Data.Monoid/index.js
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var snd = function(v) {
  return v.value1;
};
var fst = function(v) {
  return v.value0;
};

// output/Control.Monad.State.Class/index.js
var state = function(dict) {
  return dict.state;
};
var put = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(s) {
    return state1(function(v) {
      return new Tuple(unit, s);
    });
  };
};
var modify_ = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(f) {
    return state1(function(s) {
      return new Tuple(unit, f(s));
    });
  };
};
var get = function(dictMonadState) {
  return state(dictMonadState)(function(s) {
    return new Tuple(s, s);
  });
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  var bind2 = bind(dictMonad.Bind1());
  var pure3 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a) {
      return bind2(f)(function(f$prime) {
        return bind2(a)(function(a$prime) {
          return pure3(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Data.Maybe/index.js
var identity3 = /* @__PURE__ */ identity(categoryFn);
var Nothing = /* @__PURE__ */ function() {
  function Nothing2() {
  }
  ;
  Nothing2.value = new Nothing2();
  return Nothing2;
}();
var Just = /* @__PURE__ */ function() {
  function Just2(value0) {
    this.value0 = value0;
  }
  ;
  Just2.create = function(value0) {
    return new Just2(value0);
  };
  return Just2;
}();
var maybe = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Nothing) {
        return v;
      }
      ;
      if (v2 instanceof Just) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
var fromMaybe = function(a) {
  return maybe(a)(identity3);
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};

// output/Effect/foreign.js
var pureE = function(a) {
  return function() {
    return a;
  };
};
var bindE = function(a) {
  return function(f) {
    return function() {
      return f(a())();
    };
  };
};

// output/Effect/index.js
var $runtime_lazy = function(name15, moduleName, init) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init();
    state3 = 2;
    return val;
  };
};
var monadEffect = {
  Applicative0: function() {
    return applicativeEffect;
  },
  Bind1: function() {
    return bindEffect;
  }
};
var bindEffect = {
  bind: bindE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var applicativeEffect = {
  pure: pureE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
  return {
    map: liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
  return {
    apply: ap(monadEffect),
    Functor0: function() {
      return $lazy_functorEffect(0);
    }
  };
});
var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref) {
  return function() {
    return ref.value;
  };
};
var write = function(val) {
  return function(ref) {
    return function() {
      ref.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

// output/Control.Monad.Trans.Class/index.js
var lift = function(dict) {
  return dict.lift;
};

// output/Effect.Class/index.js
var monadEffectEffect = {
  liftEffect: /* @__PURE__ */ identity(categoryFn),
  Monad0: function() {
    return monadEffect;
  }
};
var liftEffect = function(dict) {
  return dict.liftEffect;
};

// output/Control.Monad.State.Trans/index.js
var runStateT = function(v) {
  return v;
};
var monadTransStateT = {
  lift: function(dictMonad) {
    var bind2 = bind(dictMonad.Bind1());
    var pure3 = pure(dictMonad.Applicative0());
    return function(m) {
      return function(s) {
        return bind2(m)(function(x) {
          return pure3(new Tuple(x, s));
        });
      };
    };
  }
};
var lift3 = /* @__PURE__ */ lift(monadTransStateT);
var functorStateT = function(dictFunctor) {
  var map4 = map(dictFunctor);
  return {
    map: function(f) {
      return function(v) {
        return function(s) {
          return map4(function(v1) {
            return new Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    }
  };
};
var monadStateT = function(dictMonad) {
  return {
    Applicative0: function() {
      return applicativeStateT(dictMonad);
    },
    Bind1: function() {
      return bindStateT(dictMonad);
    }
  };
};
var bindStateT = function(dictMonad) {
  var bind2 = bind(dictMonad.Bind1());
  return {
    bind: function(v) {
      return function(f) {
        return function(s) {
          return bind2(v(s))(function(v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var applyStateT = function(dictMonad) {
  var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: ap(monadStateT(dictMonad)),
    Functor0: function() {
      return functorStateT1;
    }
  };
};
var applicativeStateT = function(dictMonad) {
  var pure3 = pure(dictMonad.Applicative0());
  return {
    pure: function(a) {
      return function(s) {
        return pure3(new Tuple(a, s));
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var monadEffectState = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  return {
    liftEffect: function() {
      var $197 = lift3(Monad0);
      var $198 = liftEffect(dictMonadEffect);
      return function($199) {
        return $197($198($199));
      };
    }(),
    Monad0: function() {
      return monadStateT1;
    }
  };
};
var monadStateStateT = function(dictMonad) {
  var pure3 = pure(dictMonad.Applicative0());
  var monadStateT1 = monadStateT(dictMonad);
  return {
    state: function(f) {
      return function($200) {
        return pure3(f($200));
      };
    },
    Monad0: function() {
      return monadStateT1;
    }
  };
};

// output/Data.Array/foreign.js
var rangeImpl = function(start2, end) {
  var step2 = start2 > end ? -1 : 1;
  var result = new Array(step2 * (end - start2) + 1);
  var i = start2, n = 0;
  while (i !== end) {
    result[n++] = i;
    i += step2;
  }
  result[n] = i;
  return result;
};
var replicateFill = function(count, value12) {
  if (count < 1) {
    return [];
  }
  var result = new Array(count);
  return result.fill(value12);
};
var replicatePolyfill = function(count, value12) {
  var result = [];
  var n = 0;
  for (var i = 0; i < count; i++) {
    result[n++] = value12;
  }
  return result;
};
var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var length = function(xs) {
  return xs.length;
};
var unconsImpl = function(empty3, next, xs) {
  return xs.length === 0 ? empty3({}) : next(xs[0])(xs.slice(1));
};
var indexImpl = function(just, nothing, xs, i) {
  return i < 0 || i >= xs.length ? nothing : just(xs[i]);
};
var concat = function(xss) {
  if (xss.length <= 1e4) {
    return Array.prototype.concat.apply([], xss);
  }
  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init) {
    return function(xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init) {
    return function(xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Data.Foldable/index.js
var foldr = function(dict) {
  return dict.foldr;
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append4 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldr2(function(x) {
        return function(acc) {
          return append4(f(x))(acc);
        };
      })(mempty2);
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};

// output/Data.Function.Uncurried/foreign.js
var runFn2 = function(fn) {
  return function(a) {
    return function(b) {
      return fn(a, b);
    };
  };
};
var runFn3 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return fn(a, b, c);
      };
    };
  };
};
var runFn4 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

// output/Data.FunctorWithIndex/foreign.js
var mapWithIndexArray = function(f) {
  return function(xs) {
    var l = xs.length;
    var result = Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(i)(xs[i]);
    }
    return result;
  };
};

// output/Data.FunctorWithIndex/index.js
var mapWithIndex = function(dict) {
  return dict.mapWithIndex;
};
var functorWithIndexArray = {
  mapWithIndex: mapWithIndexArray,
  Functor0: function() {
    return functorArray;
  }
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = /* @__PURE__ */ function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map4) {
      return function(pure3) {
        return function(f) {
          return function(array) {
            function go2(bot, top3) {
              switch (top3 - bot) {
                case 0:
                  return pure3([]);
                case 1:
                  return map4(array1)(f(array[bot]));
                case 2:
                  return apply2(map4(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map4(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                  return apply2(map4(concat2)(go2(bot, pivot)))(go2(pivot, top3));
              }
            }
            return go2(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Traversable/index.js
var identity4 = /* @__PURE__ */ identity(categoryFn);
var traverse = function(dict) {
  return dict.traverse;
};
var sequenceDefault = function(dictTraversable) {
  var traverse2 = traverse(dictTraversable);
  return function(dictApplicative) {
    return traverse2(dictApplicative)(identity4);
  };
};
var traversableArray = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
  },
  sequence: function(dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  },
  Functor0: function() {
    return functorArray;
  },
  Foldable1: function() {
    return foldableArray;
  }
};
var $$for = function(dictApplicative) {
  return function(dictTraversable) {
    var traverse2 = traverse(dictTraversable)(dictApplicative);
    return function(x) {
      return function(f) {
        return traverse2(f)(x);
      };
    };
  };
};

// output/Data.Unfoldable/foreign.js
var unfoldrArrayImpl = function(isNothing2) {
  return function(fromJust4) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value12 = b;
            while (true) {
              var maybe2 = f(value12);
              if (isNothing2(maybe2))
                return result;
              var tuple = fromJust4(maybe2);
              result.push(fst2(tuple));
              value12 = snd2(tuple);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/foreign.js
var unfoldr1ArrayImpl = function(isNothing2) {
  return function(fromJust4) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value12 = b;
            while (true) {
              var tuple = f(value12);
              result.push(fst2(tuple));
              var maybe2 = snd2(tuple);
              if (isNothing2(maybe2))
                return result;
              value12 = fromJust4(maybe2);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/index.js
var fromJust2 = /* @__PURE__ */ fromJust();
var unfoldable1Array = {
  unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
};

// output/Data.Unfoldable/index.js
var fromJust3 = /* @__PURE__ */ fromJust();
var unfoldr = function(dict) {
  return dict.unfoldr;
};
var unfoldableArray = {
  unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
  Unfoldable10: function() {
    return unfoldable1Array;
  }
};

// output/Data.Array/index.js
var append2 = /* @__PURE__ */ append(semigroupArray);
var uncons = /* @__PURE__ */ function() {
  return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
    return function(xs) {
      return new Just({
        head: x,
        tail: xs
      });
    };
  });
}();
var range2 = /* @__PURE__ */ runFn2(rangeImpl);
var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
var index = /* @__PURE__ */ function() {
  return runFn4(indexImpl)(Just.create)(Nothing.value);
}();
var head = function(xs) {
  return index(xs)(0);
};
var cons = function(x) {
  return function(xs) {
    return append2([x])(xs);
  };
};

// output/Data.Int/foreign.js
var fromNumberImpl = function(just) {
  return function(nothing) {
    return function(n) {
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};
var toNumber = function(n) {
  return n;
};
var rem = function(x) {
  return function(y) {
    return x % y;
  };
};

// output/Data.Number/foreign.js
var isFiniteImpl = isFinite;
var ceil = Math.ceil;
var floor = Math.floor;

// output/Data.Int/index.js
var top2 = /* @__PURE__ */ top(boundedInt);
var bottom2 = /* @__PURE__ */ bottom(boundedInt);
var fromNumber = /* @__PURE__ */ function() {
  return fromNumberImpl(Just.create)(Nothing.value);
}();
var unsafeClamp = function(x) {
  if (!isFiniteImpl(x)) {
    return 0;
  }
  ;
  if (x >= toNumber(top2)) {
    return top2;
  }
  ;
  if (x <= toNumber(bottom2)) {
    return bottom2;
  }
  ;
  if (otherwise) {
    return fromMaybe(0)(fromNumber(x));
  }
  ;
  throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
};
var floor2 = function($39) {
  return unsafeClamp(floor($39));
};
var ceil2 = function($40) {
  return unsafeClamp(ceil($40));
};

// output/Data.Map.Internal/index.js
var $runtime_lazy2 = function(name15, moduleName, init) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init();
    state3 = 2;
    return val;
  };
};
var Leaf = /* @__PURE__ */ function() {
  function Leaf2() {
  }
  ;
  Leaf2.value = new Leaf2();
  return Leaf2;
}();
var Node = /* @__PURE__ */ function() {
  function Node2(value0, value1, value22, value32, value42, value52) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
    this.value3 = value32;
    this.value4 = value42;
    this.value5 = value52;
  }
  ;
  Node2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return function(value32) {
          return function(value42) {
            return function(value52) {
              return new Node2(value0, value1, value22, value32, value42, value52);
            };
          };
        };
      };
    };
  };
  return Node2;
}();
var IterLeaf = /* @__PURE__ */ function() {
  function IterLeaf2() {
  }
  ;
  IterLeaf2.value = new IterLeaf2();
  return IterLeaf2;
}();
var IterEmit = /* @__PURE__ */ function() {
  function IterEmit2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  IterEmit2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new IterEmit2(value0, value1, value22);
      };
    };
  };
  return IterEmit2;
}();
var IterNode = /* @__PURE__ */ function() {
  function IterNode2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  IterNode2.create = function(value0) {
    return function(value1) {
      return new IterNode2(value0, value1);
    };
  };
  return IterNode2;
}();
var Split = /* @__PURE__ */ function() {
  function Split2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  Split2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new Split2(value0, value1, value22);
      };
    };
  };
  return Split2;
}();
var SplitLast = /* @__PURE__ */ function() {
  function SplitLast2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  SplitLast2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new SplitLast2(value0, value1, value22);
      };
    };
  };
  return SplitLast2;
}();
var unsafeNode = function(k, v, l, r) {
  if (l instanceof Leaf) {
    if (r instanceof Leaf) {
      return new Node(1, 1, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 680, column 5 - line 684, column 39): " + [r.constructor.name]);
  }
  ;
  if (l instanceof Node) {
    if (r instanceof Leaf) {
      return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + function() {
        var $277 = l.value0 > r.value0;
        if ($277) {
          return l.value0;
        }
        ;
        return r.value0;
      }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 686, column 5 - line 690, column 68): " + [r.constructor.name]);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 678, column 32 - line 690, column 68): " + [l.constructor.name]);
};
var toMapIter = /* @__PURE__ */ function() {
  return flip(IterNode.create)(IterLeaf.value);
}();
var stepWith = function(f) {
  return function(next) {
    return function(done) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof IterLeaf) {
            $tco_done = true;
            return done(unit);
          }
          ;
          if (v instanceof IterEmit) {
            $tco_done = true;
            return next(v.value0, v.value1, v.value2);
          }
          ;
          if (v instanceof IterNode) {
            $copy_v = f(v.value1)(v.value0);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 918, column 8 - line 924, column 20): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
};
var singleton3 = function(k) {
  return function(v) {
    return new Node(1, 1, k, v, Leaf.value, Leaf.value);
  };
};
var unsafeBalancedNode = /* @__PURE__ */ function() {
  var height8 = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Node) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 735, column 12 - line 737, column 26): " + [v.constructor.name]);
  };
  var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
    if (rl instanceof Node && rl.value0 > height8(rr)) {
      return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
    }
    ;
    return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
  };
  var rotateRight = function(k, v, lk, lv, ll, lr, r) {
    if (lr instanceof Node && height8(ll) <= lr.value0) {
      return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
    }
    ;
    return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
  };
  return function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return singleton3(k)(v);
      }
      ;
      if (r instanceof Node && r.value0 > 1) {
        return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Node) {
        if (r.value0 > (l.value0 + 1 | 0)) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        if (l.value0 > (r.value0 + 1 | 0)) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
      }
      ;
      if (r instanceof Leaf && l.value0 > 1) {
        return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 695, column 40 - line 716, column 34): " + [l.constructor.name]);
  };
}();
var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy2("unsafeSplit", "Data.Map.Internal", function() {
  return function(comp, k, m) {
    if (m instanceof Leaf) {
      return new Split(Nothing.value, Leaf.value, Leaf.value);
    }
    ;
    if (m instanceof Node) {
      var v = comp(k)(m.value2);
      if (v instanceof LT) {
        var v1 = $lazy_unsafeSplit(771)(comp, k, m.value4);
        return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
      }
      ;
      if (v instanceof GT) {
        var v1 = $lazy_unsafeSplit(774)(comp, k, m.value5);
        return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
      }
      ;
      if (v instanceof EQ) {
        return new Split(new Just(m.value3), m.value4, m.value5);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 769, column 5 - line 777, column 30): " + [v.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 765, column 34 - line 777, column 30): " + [m.constructor.name]);
  };
});
var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(764);
var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy2("unsafeSplitLast", "Data.Map.Internal", function() {
  return function(k, v, l, r) {
    if (r instanceof Leaf) {
      return new SplitLast(k, v, l);
    }
    ;
    if (r instanceof Node) {
      var v1 = $lazy_unsafeSplitLast(757)(r.value2, r.value3, r.value4, r.value5);
      return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 754, column 37 - line 758, column 57): " + [r.constructor.name]);
  };
});
var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(753);
var unsafeJoinNodes = function(v, v1) {
  if (v instanceof Leaf) {
    return v1;
  }
  ;
  if (v instanceof Node) {
    var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
    return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 742, column 25 - line 746, column 38): " + [v.constructor.name, v1.constructor.name]);
};
var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy2("unsafeUnionWith", "Data.Map.Internal", function() {
  return function(comp, app, l, r) {
    if (l instanceof Leaf) {
      return r;
    }
    ;
    if (r instanceof Leaf) {
      return l;
    }
    ;
    if (r instanceof Node) {
      var v = unsafeSplit(comp, r.value2, l);
      var l$prime = $lazy_unsafeUnionWith(787)(comp, app, v.value1, r.value4);
      var r$prime = $lazy_unsafeUnionWith(788)(comp, app, v.value2, r.value5);
      if (v.value0 instanceof Just) {
        return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
      }
      ;
      if (v.value0 instanceof Nothing) {
        return unsafeBalancedNode(r.value2, r.value3, l$prime, r$prime);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 789, column 5 - line 793, column 46): " + [v.value0.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 782, column 42 - line 793, column 46): " + [l.constructor.name, r.constructor.name]);
  };
});
var unsafeUnionWith = /* @__PURE__ */ $lazy_unsafeUnionWith(781);
var unionWith = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(app) {
    return function(m1) {
      return function(m2) {
        return unsafeUnionWith(compare2, app, m1, m2);
      };
    };
  };
};
var union = function(dictOrd) {
  return unionWith(dictOrd)($$const);
};
var lookup = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare2(k)(v.value2);
          if (v1 instanceof LT) {
            $copy_v = v.value4;
            return;
          }
          ;
          if (v1 instanceof GT) {
            $copy_v = v.value5;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return new Just(v.value3);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 281, column 7 - line 284, column 22): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 278, column 8 - line 284, column 22): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return go2;
  };
};
var iterMapL = /* @__PURE__ */ function() {
  var go2 = function($copy_iter) {
    return function($copy_v) {
      var $tco_var_iter = $copy_iter;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(iter, v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return iter;
        }
        ;
        if (v instanceof Node) {
          if (v.value5 instanceof Leaf) {
            $tco_var_iter = new IterEmit(v.value2, v.value3, iter);
            $copy_v = v.value4;
            return;
          }
          ;
          $tco_var_iter = new IterEmit(v.value2, v.value3, new IterNode(v.value5, iter));
          $copy_v = v.value4;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 929, column 13 - line 936, column 48): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_iter, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  return go2;
}();
var stepAscCps = /* @__PURE__ */ stepWith(iterMapL);
var stepUnfoldr = /* @__PURE__ */ function() {
  var step2 = function(k, v, next) {
    return new Just(new Tuple(new Tuple(k, v), next));
  };
  return stepAscCps(step2)(function(v) {
    return Nothing.value;
  });
}();
var toUnfoldable = function(dictUnfoldable) {
  var $767 = unfoldr(dictUnfoldable)(stepUnfoldr);
  return function($768) {
    return $767(toMapIter($768));
  };
};
var isEmpty = function(v) {
  if (v instanceof Leaf) {
    return true;
  }
  ;
  return false;
};
var insert = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go2 = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton3(k)(v);
        }
        ;
        if (v1 instanceof Node) {
          var v2 = compare2(k)(v1.value2);
          if (v2 instanceof LT) {
            return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
          }
          ;
          if (v2 instanceof GT) {
            return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
          }
          ;
          if (v2 instanceof EQ) {
            return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 469, column 7 - line 472, column 35): " + [v2.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 466, column 8 - line 472, column 35): " + [v1.constructor.name]);
      };
      return go2;
    };
  };
};
var filterWithKey = function(dictOrd) {
  return function(f) {
    var go2 = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        if (f(v.value2)(v.value3)) {
          return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), go2(v.value5));
        }
        ;
        if (otherwise) {
          return unsafeJoinNodes(go2(v.value4), go2(v.value5));
        }
        ;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 625, column 8 - line 631, column 47): " + [v.constructor.name]);
    };
    return go2;
  };
};
var filter = function(dictOrd) {
  var $769 = filterWithKey(dictOrd);
  return function($770) {
    return $769($$const($770));
  };
};
var empty2 = /* @__PURE__ */ function() {
  return Leaf.value;
}();
var fromFoldable = function(dictOrd) {
  var insert1 = insert(dictOrd);
  return function(dictFoldable) {
    return foldl(dictFoldable)(function(m) {
      return function(v) {
        return insert1(v.value0)(v.value1)(m);
      };
    })(empty2);
  };
};
var unions = function(dictOrd) {
  var union1 = union(dictOrd);
  return function(dictFoldable) {
    return foldl(dictFoldable)(union1)(empty2);
  };
};
var $$delete = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    var go2 = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        var v1 = compare2(k)(v.value2);
        if (v1 instanceof LT) {
          return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
        }
        ;
        if (v1 instanceof GT) {
          return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
        }
        ;
        if (v1 instanceof EQ) {
          return unsafeJoinNodes(v.value4, v.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 496, column 7 - line 499, column 43): " + [v1.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 493, column 8 - line 499, column 43): " + [v.constructor.name]);
    };
    return go2;
  };
};

// output/Data.String.CodeUnits/foreign.js
var toCharArray = function(s) {
  return s.split("");
};
var singleton4 = function(c) {
  return c;
};
var _charAt = function(just) {
  return function(nothing) {
    return function(i) {
      return function(s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};
var length2 = function(s) {
  return s.length;
};

// output/Data.String.CodeUnits/index.js
var charAt2 = /* @__PURE__ */ function() {
  return _charAt(Just.create)(Nothing.value);
}();

// output/Effect.Console/foreign.js
var log2 = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Effect.Random/foreign.js
var random = Math.random;

// output/Effect.Random/index.js
var randomInt = function(low2) {
  return function(high2) {
    return function __do3() {
      var n = random();
      var asNumber = (toNumber(high2) - toNumber(low2) + 1) * n + toNumber(low2);
      return floor2(asNumber);
    };
  };
};

// output/Graphics.Canvas/foreign.js
function getCanvasElementByIdImpl(id, Just2, Nothing2) {
  return function() {
    var el = document.getElementById(id);
    if (el && el instanceof HTMLCanvasElement) {
      return Just2(el);
    } else {
      return Nothing2;
    }
  };
}
function getContext2D(c) {
  return function() {
    return c.getContext("2d");
  };
}
function setCanvasWidth(canvas) {
  return function(width8) {
    return function() {
      canvas.width = width8;
    };
  };
}
function setCanvasHeight(canvas) {
  return function(height8) {
    return function() {
      canvas.height = height8;
    };
  };
}
function clearRect(ctx) {
  return function(r) {
    return function() {
      ctx.clearRect(r.x, r.y, r.width, r.height);
    };
  };
}
function setFont(ctx) {
  return function(fontspec) {
    return function() {
      ctx.font = fontspec;
    };
  };
}

// output/Graphics.Canvas/index.js
var applySecond2 = /* @__PURE__ */ applySecond(applyEffect);
var setCanvasDimensions = function(ce) {
  return function(d) {
    return applySecond2(setCanvasHeight(ce)(d.height))(setCanvasWidth(ce)(d.width));
  };
};
var getCanvasElementById = function(elId) {
  return getCanvasElementByIdImpl(elId, Just.create, Nothing.value);
};

// output/Web.Event.EventTarget/foreign.js
function eventListener(fn) {
  return function() {
    return function(event) {
      return fn(event)();
    };
  };
}
function addEventListener(type) {
  return function(listener) {
    return function(useCapture) {
      return function(target5) {
        return function() {
          return target5.addEventListener(type, listener, useCapture);
        };
      };
    };
  };
}

// output/Web.HTML/foreign.js
var windowImpl = function() {
  return window;
};

// output/Web.Internal.FFI/foreign.js
function _unsafeReadProtoTagged(nothing, just, name15, value12) {
  if (typeof window !== "undefined") {
    var ty = window[name15];
    if (ty != null && value12 instanceof ty) {
      return just(value12);
    }
  }
  var obj = value12;
  while (obj != null) {
    var proto = Object.getPrototypeOf(obj);
    var constructorName = proto.constructor.name;
    if (constructorName === name15) {
      return just(value12);
    } else if (constructorName === "Object") {
      return nothing;
    }
    obj = proto;
  }
  return nothing;
}

// output/Web.Internal.FFI/index.js
var unsafeReadProtoTagged = function(name15) {
  return function(value12) {
    return _unsafeReadProtoTagged(Nothing.value, Just.create, name15, value12);
  };
};

// output/Data.Nullable/foreign.js
function nullable(a, r, f) {
  return a == null ? r : f(a);
}

// output/Data.Nullable/index.js
var toMaybe = function(n) {
  return nullable(n, Nothing.value, Just.create);
};

// output/Web.HTML.Window/foreign.js
function innerWidth(window2) {
  return function() {
    return window2.innerWidth;
  };
}
function innerHeight(window2) {
  return function() {
    return window2.innerHeight;
  };
}
function _open(url) {
  return function(name15) {
    return function(features) {
      return function(window2) {
        return function() {
          return window2.open(url, name15, features);
        };
      };
    };
  };
}
function requestAnimationFrame(fn) {
  return function(window2) {
    return function() {
      return window2.requestAnimationFrame(fn);
    };
  };
}

// output/Web.HTML.Window/index.js
var map2 = /* @__PURE__ */ map(functorEffect);
var toEventTarget = unsafeCoerce2;
var open = function(url$prime) {
  return function(name15) {
    return function(features) {
      return function(window2) {
        return map2(toMaybe)(_open(url$prime)(name15)(features)(window2));
      };
    };
  };
};

// output/Web.UIEvent.MouseEvent/foreign.js
function pageX(e) {
  return e.pageX;
}
function pageY(e) {
  return e.pageY;
}

// output/Web.UIEvent.MouseEvent/index.js
var fromEvent = /* @__PURE__ */ unsafeReadProtoTagged("MouseEvent");

// output/Main/index.js
var ordRecord2 = /* @__PURE__ */ ordRecord()(/* @__PURE__ */ ordRecordCons(/* @__PURE__ */ ordRecordCons(ordRecordNil)()({
  reflectSymbol: function() {
    return "y";
  }
})(ordInt))()({
  reflectSymbol: function() {
    return "x";
  }
})(ordInt));
var filter2 = /* @__PURE__ */ filter(ordRecord2);
var toUnfoldable2 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
var pure2 = /* @__PURE__ */ pure(applicativeEffect);
var insert2 = /* @__PURE__ */ insert(ordRecord2);
var $$delete2 = /* @__PURE__ */ $$delete(ordRecord2);
var map3 = /* @__PURE__ */ map(functorEffect);
var discard2 = /* @__PURE__ */ discard(discardUnit);
var fromFoldable2 = /* @__PURE__ */ fromFoldable(ordRecord2)(foldableArray);
var $$void2 = /* @__PURE__ */ $$void(functorEffect);
var unions2 = /* @__PURE__ */ unions(ordRecord2)(foldableArray);
var map1 = /* @__PURE__ */ map(functorArray);
var union2 = /* @__PURE__ */ union(ordRecord2);
var append3 = /* @__PURE__ */ append(semigroupArray);
var lookup2 = /* @__PURE__ */ lookup(ordRecord2);
var when2 = /* @__PURE__ */ when(applicativeEffect);
var discard22 = /* @__PURE__ */ discard2(/* @__PURE__ */ bindStateT(monadEffect));
var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadEffect);
var monadEffectState2 = /* @__PURE__ */ monadEffectState(monadEffectEffect);
var applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadEffect);
var pure1 = /* @__PURE__ */ pure(applicativeStateT2);
var when1 = /* @__PURE__ */ when(applicativeStateT2);
var void1 = /* @__PURE__ */ $$void(/* @__PURE__ */ functorStateT(functorEffect));
var $$for2 = /* @__PURE__ */ $$for(applicativeStateT2)(traversableArray);
var GfxWhiteout = /* @__PURE__ */ function() {
  function GfxWhiteout2() {
  }
  ;
  GfxWhiteout2.value = new GfxWhiteout2();
  return GfxWhiteout2;
}();
var EventGfx = /* @__PURE__ */ function() {
  function EventGfx2(value0) {
    this.value0 = value0;
  }
  ;
  EventGfx2.create = function(value0) {
    return new EventGfx2(value0);
  };
  return EventGfx2;
}();
var EventMouseClick = /* @__PURE__ */ function() {
  function EventMouseClick2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  EventMouseClick2.create = function(value0) {
    return function(value1) {
      return new EventMouseClick2(value0, value1);
    };
  };
  return EventMouseClick2;
}();
var EventMouseMove = /* @__PURE__ */ function() {
  function EventMouseMove2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  EventMouseMove2.create = function(value0) {
    return function(value1) {
      return new EventMouseMove2(value0, value1);
    };
  };
  return EventMouseMove2;
}();
var tick = function(dictMonadState) {
  return modify_(dictMonadState)(function(st) {
    var $110 = {};
    for (var $111 in st) {
      if ({}.hasOwnProperty.call(st, $111)) {
        $110[$111] = st[$111];
      }
      ;
    }
    ;
    $110.tick = st.tick + 1 | 0;
    $110.redraw = false;
    $110.transitions = function() {
      var v1 = uncons(st.transitions);
      if (v1 instanceof Nothing) {
        return st.transitions;
      }
      ;
      if (v1 instanceof Just) {
        var $106 = isEmpty(v1.value0.head.cells);
        if ($106) {
          return v1.value0.tail;
        }
        ;
        return st.transitions;
      }
      ;
      throw new Error("Failed pattern match at Main (line 239, column 19 - line 241, column 79): " + [v1.constructor.name]);
    }();
    $110.inverse = filter2(function(v1) {
      return st.tick <= v1;
    })(st.inverse);
    return $110;
  });
};
var tick1 = /* @__PURE__ */ tick(monadStateStateT2);
var pickRandom = function(cells2) {
  var arr = toUnfoldable2(cells2);
  return function __do3() {
    var idx = randomInt(0)(length(arr) - 1 | 0)();
    var v = index(arr)(idx);
    if (v instanceof Nothing) {
      return Nothing.value;
    }
    ;
    if (v instanceof Just) {
      return new Just(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Main (line 139, column 3 - line 141, column 28): " + [v.constructor.name]);
  };
};
var pickCell = function(dictMonadState) {
  var get2 = get(dictMonadState);
  var put2 = put(dictMonadState);
  return function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind1 = bind(Monad0.Bind1());
    var pure22 = pure(Monad0.Applicative0());
    var liftEffect2 = liftEffect(dictMonadEffect);
    return bind1(get2)(function(st) {
      var v = uncons(st.transitions);
      if (v instanceof Nothing) {
        return pure22(unit);
      }
      ;
      if (v instanceof Just) {
        return bind1(liftEffect2(v.value0.head.picker(v.value0.head.cells)))(function(v1) {
          if (v1 instanceof Nothing) {
            return pure22(unit);
          }
          ;
          if (v1 instanceof Just) {
            return put2(function() {
              var $120 = {};
              for (var $121 in st) {
                if ({}.hasOwnProperty.call(st, $121)) {
                  $120[$121] = st[$121];
                }
                ;
              }
              ;
              $120.redraw = true;
              $120.cells = insert2(v1.value0.value0)(v1.value0.value1)(st.cells);
              $120.transitions = cons(function() {
                var $117 = {};
                for (var $118 in v.value0.head) {
                  if ({}.hasOwnProperty.call(v.value0.head, $118)) {
                    $117[$118] = v["value0"]["head"][$118];
                  }
                  ;
                }
                ;
                $117.cells = $$delete2(v1.value0.value0)(v.value0.head.cells);
                return $117;
              }())(v.value0.tail);
              return $120;
            }());
          }
          ;
          throw new Error("Failed pattern match at Main (line 271, column 49 - line 274, column 135): " + [v1.constructor.name]);
        });
      }
      ;
      throw new Error("Failed pattern match at Main (line 268, column 3 - line 274, column 135): " + [v.constructor.name]);
    });
  };
};
var pickCell1 = /* @__PURE__ */ pickCell(monadStateStateT2)(monadEffectState2);
var newContext = function __do() {
  var w = windowImpl();
  var v = getCanvasElementById("lcolonq-canvas")();
  if (v instanceof Nothing) {
    return Nothing.value;
  }
  ;
  if (v instanceof Just) {
    var width8 = map3(toNumber)(innerWidth(w))();
    var height8 = map3(toNumber)(innerHeight(w))();
    setCanvasDimensions(v.value0)({
      width: width8,
      height: height8
    })();
    var render = getContext2D(v.value0)();
    setFont(render)("bold 0.8vw Iosevka Comfy")();
    var cellWidth = toNumber(ceil2(width8 / 200));
    var cellHeight = cellWidth * 2;
    return new Just({
      window: w,
      canvas: v.value0,
      render,
      width: width8,
      height: height8,
      cellHeight,
      cellWidth,
      widthCells: ceil2(200),
      heightCells: ceil2(height8 / cellHeight),
      events: [new EventGfx(GfxWhiteout.value)]
    });
  }
  ;
  throw new Error("Failed pattern match at Main (line 79, column 45 - line 101, column 10): " + [v.constructor.name]);
};
var initialState = {
  tick: 0,
  cells: empty2,
  inverse: empty2,
  transitions: [],
  redraw: true
};
var gfxTransitions = function(ctx) {
  return function(v) {
    var linkRight = function(y) {
      return function(fgc) {
        return function(bgc) {
          return function(str) {
            return function(url) {
              return fromFoldable2(mapWithIndex2(function(i) {
                return function(c) {
                  return new Tuple({
                    x: ((ctx.widthCells - length2(str) | 0) - 8 | 0) + i | 0,
                    y
                  }, {
                    bg: bgc,
                    fg: fgc,
                    "char": singleton4(c),
                    click: function(st) {
                      return function __do3() {
                        $$void2(open(url)("_blank")("")(ctx.window))();
                        return st;
                      };
                    }
                  });
                };
              })(toCharArray(str)));
            };
          };
        };
      };
    };
    var link = function(y) {
      return function(fgc) {
        return function(bgc) {
          return function(str) {
            return function(url) {
              return fromFoldable2(mapWithIndex2(function(i) {
                return function(c) {
                  return new Tuple({
                    x: i,
                    y
                  }, {
                    bg: bgc,
                    fg: fgc,
                    "char": singleton4(c),
                    click: function(st) {
                      return function __do3() {
                        $$void2(open(url)("_blank")("")(ctx.window))();
                        return st;
                      };
                    }
                  });
                };
              })(toCharArray(str)));
            };
          };
        };
      };
    };
    var fg = unions2([link(0)("purple")("white")("twitch.tv/lcolonq")("https://twitch.tv/lcolonq"), link(1)("blue")("white")("twitter.com/lcolonq")("https://twitter.com/lcolonq"), link(ctx.heightCells - 1 | 0)("white")("black")("the previous one")("https://pub.colonq.computer/~llll/cgi-bin/ring?me=llll&offset=-1"), linkRight(ctx.heightCells - 1 | 0)("white")("black")("the next one")("https://pub.colonq.computer/~llll/cgi-bin/ring?me=llll&offset=1")]);
    var bg = fromFoldable2(concat(flip(map1)(range2(0)(ctx.widthCells))(function(x) {
      return flip(map1)(range2(0)(ctx.heightCells))(function(y) {
        return new Tuple({
          x,
          y
        }, {
          bg: "white",
          fg: "black",
          click: pure2,
          "char": function() {
            var v1 = charAt2(rem(x + y | 0)(length2("LCOLONQ")))("LCOLONQ");
            if (v1 instanceof Nothing) {
              return "?";
            }
            ;
            if (v1 instanceof Just) {
              return singleton4(v1.value0);
            }
            ;
            throw new Error("Failed pattern match at Main (line 188, column 12 - line 190, column 39): " + [v1.constructor.name]);
          }()
        });
      });
    })));
    var cells2 = union2(fg)(bg);
    return [{
      cells: cells2,
      speed: 20,
      cadence: 1,
      picker: pickRandom
    }];
  };
};
var pullEvents = function(dictMonadState) {
  var get2 = get(dictMonadState);
  var put2 = put(dictMonadState);
  return function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var Bind1 = Monad0.Bind1();
    var bind1 = bind(Bind1);
    var liftEffect2 = liftEffect(dictMonadEffect);
    var pure22 = pure(Monad0.Applicative0());
    var discard3 = discard2(Bind1);
    return function(rc) {
      return bind1(liftEffect2(read(rc)))(function(ctx) {
        return bind1(get2)(function(st) {
          return bind1(function() {
            var v = uncons(ctx.events);
            if (v instanceof Nothing) {
              return pure22(st);
            }
            ;
            if (v instanceof Just) {
              return discard3(liftEffect2(write({
                canvas: ctx.canvas,
                cellHeight: ctx.cellHeight,
                cellWidth: ctx.cellWidth,
                height: ctx.height,
                heightCells: ctx.heightCells,
                render: ctx.render,
                width: ctx.width,
                widthCells: ctx.widthCells,
                window: ctx.window,
                events: v.value0.tail
              })(rc)))(function() {
                if (v.value0.head instanceof EventGfx) {
                  return pure22({
                    cells: st.cells,
                    inverse: st.inverse,
                    redraw: st.redraw,
                    tick: st.tick,
                    transitions: append3(st.transitions)(gfxTransitions(ctx)(v.value0.head.value0))
                  });
                }
                ;
                if (v.value0.head instanceof EventMouseClick) {
                  var v1 = lookup2({
                    x: v.value0.head.value0,
                    y: v.value0.head.value1
                  })(st.cells);
                  if (v1 instanceof Nothing) {
                    return pure22(st);
                  }
                  ;
                  if (v1 instanceof Just) {
                    return liftEffect2(v1.value0.click(st));
                  }
                  ;
                  throw new Error("Failed pattern match at Main (line 256, column 11 - line 258, column 53): " + [v1.constructor.name]);
                }
                ;
                if (v.value0.head instanceof EventMouseMove) {
                  var inv = fromFoldable2(concat(flip(map1)(range2(v.value0.head.value0 - 1 | 0)(v.value0.head.value0 + 1 | 0))(function(x) {
                    return flip(map1)(range2(v.value0.head.value1 - 1 | 0)(v.value0.head.value1 + 1 | 0))(function(y) {
                      return new Tuple({
                        x,
                        y
                      }, st.tick + 30 | 0);
                    });
                  })));
                  return pure22({
                    cells: st.cells,
                    redraw: st.redraw,
                    tick: st.tick,
                    transitions: st.transitions,
                    inverse: union2(inv)(st.inverse)
                  });
                }
                ;
                throw new Error("Failed pattern match at Main (line 253, column 7 - line 262, column 53): " + [v.value0.head.constructor.name]);
              });
            }
            ;
            throw new Error("Failed pattern match at Main (line 249, column 10 - line 262, column 53): " + [v.constructor.name]);
          }())(function(st$prime) {
            return put2(st$prime);
          });
        });
      });
    };
  };
};
var pullEvents1 = /* @__PURE__ */ pullEvents(monadStateStateT2)(monadEffectState2);
var loop2 = function(rc) {
  return function(st) {
    return function __do3() {
      var ctx = read(rc)();
      when2(st.redraw)(clearRect(ctx.render)({
        x: 0,
        y: 0,
        width: ctx.width,
        height: ctx.height
      }))();
      var v = flip(runStateT)(st)(discard22(tick1)(function() {
        return discard22(pullEvents1(rc))(function() {
          var v2 = head(st.transitions);
          if (v2 instanceof Nothing) {
            return pure1(unit);
          }
          ;
          if (v2 instanceof Just) {
            return when1(rem(st.tick)(v2.value0.cadence) === 0)(void1($$for2(range2(0)(v2.value0.speed))(function(v1) {
              return pickCell1;
            })));
          }
          ;
          throw new Error("Failed pattern match at Main (line 287, column 5 - line 291, column 58): " + [v2.constructor.name]);
        });
      }))();
      return $$void2(requestAnimationFrame(loop2(rc)(v.value1))(ctx.window))();
    };
  };
};
var main = function __do2() {
  log2(apiServer)();
  var w = windowImpl();
  (function __do3() {
    var v = newContext();
    if (v instanceof Nothing) {
      return log2("failed to find canvas")();
    }
    ;
    if (v instanceof Just) {
      var rc = $$new(v.value0)();
      var lresize = eventListener(function(_e) {
        return function __do4() {
          var v1 = newContext();
          if (v1 instanceof Nothing) {
            return log2("failed to find canvas")();
          }
          ;
          if (v1 instanceof Just) {
            return write(v1.value0)(rc)();
          }
          ;
          throw new Error("Failed pattern match at Main (line 58, column 24 - line 60, column 41): " + [v1.constructor.name]);
        };
      })();
      addEventListener("resize")(lresize)(false)(toEventTarget(w))();
      var lmouse = function(h) {
        return eventListener(function(e) {
          var v1 = fromEvent(e);
          if (v1 instanceof Nothing) {
            return pure2(unit);
          }
          ;
          if (v1 instanceof Just) {
            var px = toNumber(pageX(v1.value0));
            var py = toNumber(pageY(v1.value0));
            return function __do4() {
              var ctx = read(rc)();
              return write({
                canvas: ctx.canvas,
                cellHeight: ctx.cellHeight,
                cellWidth: ctx.cellWidth,
                height: ctx.height,
                heightCells: ctx.heightCells,
                render: ctx.render,
                width: ctx.width,
                widthCells: ctx.widthCells,
                window: ctx.window,
                events: cons(h(floor2(px / ctx.cellWidth))(floor2(py / ctx.cellHeight)))(ctx.events)
              })(rc)();
            };
          }
          ;
          throw new Error("Failed pattern match at Main (line 62, column 42 - line 68, column 121): " + [v1.constructor.name]);
        });
      };
      var lmouseclick = lmouse(EventMouseClick.create)();
      addEventListener("click")(lmouseclick)(false)(toEventTarget(w))();
      var lmousemove = lmouse(EventMouseMove.create)();
      addEventListener("mousemove")(lmousemove)(false)(toEventTarget(w))();
      return loop2(rc)(initialState)();
    }
    ;
    throw new Error("Failed pattern match at Main (line 53, column 18 - line 73, column 27): " + [v.constructor.name]);
  })();
  return unit;
};

// <stdin>
main();
