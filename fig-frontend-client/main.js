// output/Audio/foreign.js
var initialized = false;
var ctx = null;
var voiceTracks = null;
function initializeCtx() {
  if (!initialized) {
    try {
      initialized = true;
      ctx = new window.AudioContext();
      voiceTracks = [
        document.getElementById("lcolonq-audio-voice-0"),
        document.getElementById("lcolonq-audio-voice-1"),
        document.getElementById("lcolonq-audio-voice-2"),
        document.getElementById("lcolonq-audio-voice-3"),
        document.getElementById("lcolonq-audio-voice-4"),
        document.getElementById("lcolonq-audio-voice-5"),
        document.getElementById("lcolonq-audio-voice-6")
      ];
    } catch (e) {
      initialized = false;
    }
  }
}
var _playVoice = (b) => (i) => () => {
  if (b)
    initializeCtx();
  try {
    if (initialized)
      voiceTracks[i].play();
  } catch (e) {
  }
};

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
var voidRight = function(dictFunctor) {
  var map13 = map(dictFunctor);
  return function(x) {
    return map13($$const(x));
  };
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
  var map7 = map(dictApply.Functor0());
  return function(a) {
    return function(b) {
      return apply1(map7($$const(identity2))(a))(b);
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
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
var bindFlipped = function(dictBind) {
  return flip(bind(dictBind));
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  var bind5 = bind(dictMonad.Bind1());
  var pure4 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a) {
      return bind5(f)(function(f$prime) {
        return bind5(a)(function(a$prime) {
          return pure4(f$prime(a$prime));
        });
      });
    };
  };
};

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
var unsafeSet = function(label4) {
  return function(value12) {
    return function(rec) {
      var copy = {};
      for (var key in rec) {
        if ({}.hasOwnProperty.call(rec, key)) {
          copy[key] = rec[key];
        }
      }
      copy[label4] = value12;
      return copy;
    };
  };
};
var unsafeDelete = function(label4) {
  return function(rec) {
    var copy = {};
    for (var key in rec) {
      if (key !== label4 && {}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }
    return copy;
  };
};

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqStringImpl = refEq;

// output/Data.Eq/index.js
var eqString = {
  eq: eqStringImpl
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

// output/Data.Semigroup/foreign.js
var concatString = function(s1) {
  return function(s2) {
    return s1 + s2;
  };
};
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
var semigroupString = {
  append: concatString
};
var semigroupArray = {
  append: concatArray
};
var append = function(dict) {
  return dict.append;
};

// output/Data.Monoid/index.js
var monoidString = {
  mempty: "",
  Semigroup0: function() {
    return semigroupString;
  }
};
var monoidArray = {
  mempty: [],
  Semigroup0: function() {
    return semigroupArray;
  }
};
var mempty = function(dict) {
  return dict.mempty;
};

// output/Effect/index.js
var $runtime_lazy = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
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

// output/Audio/index.js
var playVoice = function(dictMonadEffect) {
  var liftEffect6 = liftEffect(dictMonadEffect);
  return function(b) {
    return function(i) {
      return liftEffect6(_playVoice(b)(i));
    };
  };
};

// output/Auth/foreign.js
function generateNonce() {
  var arr = new Uint8Array(20);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, (b) => b.toString(16).padStart(2, "0")).join("");
}
var _startTwitchAuth = (clientID2) => (redirectURL) => () => {
  const nonce = generateNonce();
  document.cookie = `authnonce=${nonce}; path=/; max-age=3000`;
  window.location.href = `https://id.twitch.tv/oauth2/authorize?response_type=id_token&client_id=${clientID2}&redirect_uri=${redirectURL}&scope=openid&nonce=${nonce}&claims=${JSON.stringify({ id_token: { preferred_username: null } })}`;
};
function getFragmentQuery() {
  let query2 = /* @__PURE__ */ new Map();
  const hashQuery = document.location.hash.slice(1).split("&");
  for (let equals of hashQuery) {
    const pair = equals.split("=");
    query2.set(decodeURIComponent(pair[0]), decodeURIComponent(pair[1]));
  }
  return query2;
}
var _getToken = (Just2) => (Nothing2) => (pair) => () => {
  const frag = getFragmentQuery();
  const token = frag.get("id_token");
  if (token) {
    document.cookie = `id_token=${token}; path=/; SameSite=Strict`;
  }
  let id_token = null;
  let authnonce = null;
  for (let c of document.cookie.split("; ")) {
    const [k, v] = c.split("=");
    if (k === "id_token")
      id_token = v;
    else if (k === "authnonce")
      authnonce = v;
  }
  if (id_token && authnonce)
    return Just2(pair(id_token)(authnonce));
  return Nothing2;
};

// output/Config/foreign.js
var mode = globalThis.mode;
var apiServer = globalThis.apiServer;
var clientID = globalThis.clientID;
var authRedirectURL = globalThis.authRedirectURL;

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
var indexImpl = function(just, nothing, xs, i) {
  return i < 0 || i >= xs.length ? nothing : just(xs[i]);
};

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Data.Bounded/foreign.js
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq2) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq2 : gt;
        };
      };
    };
  };
};
var ordStringImpl = unsafeCompareImpl;

// output/Data.Ord/index.js
var ordString = /* @__PURE__ */ function() {
  return {
    compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqString;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};

// output/Data.Show/index.js
var showInt = {
  show: showIntImpl
};
var show = function(dict) {
  return dict.show;
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
var maybe$prime = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Nothing) {
        return v(unit);
      }
      ;
      if (v2 instanceof Just) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 250, column 1 - line 250, column 62): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var fromMaybe$prime = function(a) {
  return maybe$prime(a)(identity3);
};
var altMaybe = {
  alt: function(v) {
    return function(v1) {
      if (v instanceof Nothing) {
        return v1;
      }
      ;
      return v;
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();
var functorEither = {
  map: function(f) {
    return function(m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }
      ;
      if (m instanceof Right) {
        return new Right(f(m.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
var either = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }
      ;
      if (v2 instanceof Right) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var hush = /* @__PURE__ */ function() {
  return either($$const(Nothing.value))(Just.create);
}();

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
  }
};

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init2) {
    return function(xs) {
      var acc = init2;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init2) {
    return function(xs) {
      var acc = init2;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Control.Plus/index.js
var empty = function(dict) {
  return dict.empty;
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

// output/Data.Bifunctor/index.js
var identity4 = /* @__PURE__ */ identity(categoryFn);
var bimap = function(dict) {
  return dict.bimap;
};
var lmap = function(dictBifunctor) {
  var bimap1 = bimap(dictBifunctor);
  return function(f) {
    return bimap1(f)(identity4);
  };
};
var bifunctorTuple = {
  bimap: function(f) {
    return function(g) {
      return function(v) {
        return new Tuple(f(v.value0), g(v.value1));
      };
    };
  }
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Safe.Coerce/index.js
var coerce = function() {
  return unsafeCoerce2;
};

// output/Data.Newtype/index.js
var coerce2 = /* @__PURE__ */ coerce();
var unwrap = function() {
  return coerce2;
};

// output/Data.Foldable/index.js
var identity5 = /* @__PURE__ */ identity(categoryFn);
var foldr = function(dict) {
  return dict.foldr;
};
var traverse_ = function(dictApplicative) {
  var applySecond2 = applySecond(dictApplicative.Apply0());
  var pure4 = pure(dictApplicative);
  return function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(f) {
      return foldr22(function($454) {
        return applySecond2(f($454));
      })(pure4(unit));
    };
  };
};
var for_ = function(dictApplicative) {
  var traverse_1 = traverse_(dictApplicative);
  return function(dictFoldable) {
    return flip(traverse_1(dictFoldable));
  };
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr22 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append2 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return foldr22(function(x) {
        return function(acc) {
          return append2(f(x))(acc);
        };
      })(mempty3);
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
var foldMap = function(dict) {
  return dict.foldMap;
};
var fold = function(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function(dictMonoid) {
    return foldMap2(dictMonoid)(identity5);
  };
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

// output/Data.Array/index.js
var fold1 = /* @__PURE__ */ fold(foldableArray);
var range2 = /* @__PURE__ */ runFn2(rangeImpl);
var index = /* @__PURE__ */ function() {
  return runFn4(indexImpl)(Just.create)(Nothing.value);
}();
var head = function(xs) {
  return index(xs)(0);
};
var fold2 = function(dictMonoid) {
  return fold1(dictMonoid);
};

// output/Auth/index.js
var fold3 = /* @__PURE__ */ fold2(monoidString);
var startTwitchAuth = function(dictMonadEffect) {
  return liftEffect(dictMonadEffect)(_startTwitchAuth(clientID)(authRedirectURL));
};
var getToken = function(dictMonadEffect) {
  return liftEffect(dictMonadEffect)(_getToken(Just.create)(Nothing.value)(Tuple.create));
};
var authHeader = function(v) {
  return fold3(['FIG-TWITCH token="', v.value0, '", nonce="', v.value1, '"']);
};

// output/Effect.Aff/foreign.js
var Aff = function() {
  var EMPTY = {};
  var PURE = "Pure";
  var THROW = "Throw";
  var CATCH = "Catch";
  var SYNC = "Sync";
  var ASYNC = "Async";
  var BIND = "Bind";
  var BRACKET = "Bracket";
  var FORK = "Fork";
  var SEQ = "Sequential";
  var MAP = "Map";
  var APPLY = "Apply";
  var ALT = "Alt";
  var CONS = "Cons";
  var RESUME = "Resume";
  var RELEASE = "Release";
  var FINALIZER = "Finalizer";
  var FINALIZED = "Finalized";
  var FORKED = "Forked";
  var FIBER = "Fiber";
  var THUNK = "Thunk";
  function Aff2(tag, _1, _2, _3) {
    this.tag = tag;
    this._1 = _1;
    this._2 = _2;
    this._3 = _3;
  }
  function AffCtr(tag) {
    var fn = function(_1, _2, _3) {
      return new Aff2(tag, _1, _2, _3);
    };
    fn.tag = tag;
    return fn;
  }
  function nonCanceler2(error3) {
    return new Aff2(PURE, void 0);
  }
  function runEff(eff) {
    try {
      eff();
    } catch (error3) {
      setTimeout(function() {
        throw error3;
      }, 0);
    }
  }
  function runSync(left, right, eff) {
    try {
      return right(eff());
    } catch (error3) {
      return left(error3);
    }
  }
  function runAsync(left, eff, k) {
    try {
      return eff(k)();
    } catch (error3) {
      k(left(error3))();
      return nonCanceler2;
    }
  }
  var Scheduler = function() {
    var limit = 1024;
    var size3 = 0;
    var ix = 0;
    var queue = new Array(limit);
    var draining = false;
    function drain() {
      var thunk;
      draining = true;
      while (size3 !== 0) {
        size3--;
        thunk = queue[ix];
        queue[ix] = void 0;
        ix = (ix + 1) % limit;
        thunk();
      }
      draining = false;
    }
    return {
      isDraining: function() {
        return draining;
      },
      enqueue: function(cb) {
        var i, tmp;
        if (size3 === limit) {
          tmp = draining;
          drain();
          draining = tmp;
        }
        queue[(ix + size3) % limit] = cb;
        size3++;
        if (!draining) {
          drain();
        }
      }
    };
  }();
  function Supervisor(util) {
    var fibers = {};
    var fiberId = 0;
    var count = 0;
    return {
      register: function(fiber) {
        var fid = fiberId++;
        fiber.onComplete({
          rethrow: true,
          handler: function(result) {
            return function() {
              count--;
              delete fibers[fid];
            };
          }
        })();
        fibers[fid] = fiber;
        count++;
      },
      isEmpty: function() {
        return count === 0;
      },
      killAll: function(killError, cb) {
        return function() {
          if (count === 0) {
            return cb();
          }
          var killCount = 0;
          var kills = {};
          function kill(fid) {
            kills[fid] = fibers[fid].kill(killError, function(result) {
              return function() {
                delete kills[fid];
                killCount--;
                if (util.isLeft(result) && util.fromLeft(result)) {
                  setTimeout(function() {
                    throw util.fromLeft(result);
                  }, 0);
                }
                if (killCount === 0) {
                  cb();
                }
              };
            })();
          }
          for (var k in fibers) {
            if (fibers.hasOwnProperty(k)) {
              killCount++;
              kill(k);
            }
          }
          fibers = {};
          fiberId = 0;
          count = 0;
          return function(error3) {
            return new Aff2(SYNC, function() {
              for (var k2 in kills) {
                if (kills.hasOwnProperty(k2)) {
                  kills[k2]();
                }
              }
            });
          };
        };
      }
    };
  }
  var SUSPENDED = 0;
  var CONTINUE = 1;
  var STEP_BIND = 2;
  var STEP_RESULT = 3;
  var PENDING = 4;
  var RETURN = 5;
  var COMPLETED = 6;
  function Fiber(util, supervisor, aff) {
    var runTick = 0;
    var status2 = SUSPENDED;
    var step2 = aff;
    var fail2 = null;
    var interrupt = null;
    var bhead = null;
    var btail = null;
    var attempts = null;
    var bracketCount = 0;
    var joinId = 0;
    var joins = null;
    var rethrow = true;
    function run3(localRunTick) {
      var tmp, result, attempt;
      while (true) {
        tmp = null;
        result = null;
        attempt = null;
        switch (status2) {
          case STEP_BIND:
            status2 = CONTINUE;
            try {
              step2 = bhead(step2);
              if (btail === null) {
                bhead = null;
              } else {
                bhead = btail._1;
                btail = btail._2;
              }
            } catch (e) {
              status2 = RETURN;
              fail2 = util.left(e);
              step2 = null;
            }
            break;
          case STEP_RESULT:
            if (util.isLeft(step2)) {
              status2 = RETURN;
              fail2 = step2;
              step2 = null;
            } else if (bhead === null) {
              status2 = RETURN;
            } else {
              status2 = STEP_BIND;
              step2 = util.fromRight(step2);
            }
            break;
          case CONTINUE:
            switch (step2.tag) {
              case BIND:
                if (bhead) {
                  btail = new Aff2(CONS, bhead, btail);
                }
                bhead = step2._2;
                status2 = CONTINUE;
                step2 = step2._1;
                break;
              case PURE:
                if (bhead === null) {
                  status2 = RETURN;
                  step2 = util.right(step2._1);
                } else {
                  status2 = STEP_BIND;
                  step2 = step2._1;
                }
                break;
              case SYNC:
                status2 = STEP_RESULT;
                step2 = runSync(util.left, util.right, step2._1);
                break;
              case ASYNC:
                status2 = PENDING;
                step2 = runAsync(util.left, step2._1, function(result2) {
                  return function() {
                    if (runTick !== localRunTick) {
                      return;
                    }
                    runTick++;
                    Scheduler.enqueue(function() {
                      if (runTick !== localRunTick + 1) {
                        return;
                      }
                      status2 = STEP_RESULT;
                      step2 = result2;
                      run3(runTick);
                    });
                  };
                });
                return;
              case THROW:
                status2 = RETURN;
                fail2 = util.left(step2._1);
                step2 = null;
                break;
              case CATCH:
                if (bhead === null) {
                  attempts = new Aff2(CONS, step2, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status2 = CONTINUE;
                step2 = step2._1;
                break;
              case BRACKET:
                bracketCount++;
                if (bhead === null) {
                  attempts = new Aff2(CONS, step2, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status2 = CONTINUE;
                step2 = step2._1;
                break;
              case FORK:
                status2 = STEP_RESULT;
                tmp = Fiber(util, supervisor, step2._2);
                if (supervisor) {
                  supervisor.register(tmp);
                }
                if (step2._1) {
                  tmp.run();
                }
                step2 = util.right(tmp);
                break;
              case SEQ:
                status2 = CONTINUE;
                step2 = sequential2(util, supervisor, step2._1);
                break;
            }
            break;
          case RETURN:
            bhead = null;
            btail = null;
            if (attempts === null) {
              status2 = COMPLETED;
              step2 = interrupt || fail2 || step2;
            } else {
              tmp = attempts._3;
              attempt = attempts._1;
              attempts = attempts._2;
              switch (attempt.tag) {
                case CATCH:
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    status2 = RETURN;
                  } else if (fail2) {
                    status2 = CONTINUE;
                    step2 = attempt._2(util.fromLeft(fail2));
                    fail2 = null;
                  }
                  break;
                case RESUME:
                  if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                    status2 = RETURN;
                  } else {
                    bhead = attempt._1;
                    btail = attempt._2;
                    status2 = STEP_BIND;
                    step2 = util.fromRight(step2);
                  }
                  break;
                case BRACKET:
                  bracketCount--;
                  if (fail2 === null) {
                    result = util.fromRight(step2);
                    attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                    if (interrupt === tmp || bracketCount > 0) {
                      status2 = CONTINUE;
                      step2 = attempt._3(result);
                    }
                  }
                  break;
                case RELEASE:
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                  status2 = CONTINUE;
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    step2 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                  } else if (fail2) {
                    step2 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                  } else {
                    step2 = attempt._1.completed(util.fromRight(step2))(attempt._2);
                  }
                  fail2 = null;
                  bracketCount++;
                  break;
                case FINALIZER:
                  bracketCount++;
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail2), attempts, interrupt);
                  status2 = CONTINUE;
                  step2 = attempt._1;
                  break;
                case FINALIZED:
                  bracketCount--;
                  status2 = RETURN;
                  step2 = attempt._1;
                  fail2 = attempt._2;
                  break;
              }
            }
            break;
          case COMPLETED:
            for (var k in joins) {
              if (joins.hasOwnProperty(k)) {
                rethrow = rethrow && joins[k].rethrow;
                runEff(joins[k].handler(step2));
              }
            }
            joins = null;
            if (interrupt && fail2) {
              setTimeout(function() {
                throw util.fromLeft(fail2);
              }, 0);
            } else if (util.isLeft(step2) && rethrow) {
              setTimeout(function() {
                if (rethrow) {
                  throw util.fromLeft(step2);
                }
              }, 0);
            }
            return;
          case SUSPENDED:
            status2 = CONTINUE;
            break;
          case PENDING:
            return;
        }
      }
    }
    function onComplete(join3) {
      return function() {
        if (status2 === COMPLETED) {
          rethrow = rethrow && join3.rethrow;
          join3.handler(step2)();
          return function() {
          };
        }
        var jid = joinId++;
        joins = joins || {};
        joins[jid] = join3;
        return function() {
          if (joins !== null) {
            delete joins[jid];
          }
        };
      };
    }
    function kill(error3, cb) {
      return function() {
        if (status2 === COMPLETED) {
          cb(util.right(void 0))();
          return function() {
          };
        }
        var canceler = onComplete({
          rethrow: false,
          handler: function() {
            return cb(util.right(void 0));
          }
        })();
        switch (status2) {
          case SUSPENDED:
            interrupt = util.left(error3);
            status2 = COMPLETED;
            step2 = interrupt;
            run3(runTick);
            break;
          case PENDING:
            if (interrupt === null) {
              interrupt = util.left(error3);
            }
            if (bracketCount === 0) {
              if (status2 === PENDING) {
                attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error3)), attempts, interrupt);
              }
              status2 = RETURN;
              step2 = null;
              fail2 = null;
              run3(++runTick);
            }
            break;
          default:
            if (interrupt === null) {
              interrupt = util.left(error3);
            }
            if (bracketCount === 0) {
              status2 = RETURN;
              step2 = null;
              fail2 = null;
            }
        }
        return canceler;
      };
    }
    function join2(cb) {
      return function() {
        var canceler = onComplete({
          rethrow: false,
          handler: cb
        })();
        if (status2 === SUSPENDED) {
          run3(runTick);
        }
        return canceler;
      };
    }
    return {
      kill,
      join: join2,
      onComplete,
      isSuspended: function() {
        return status2 === SUSPENDED;
      },
      run: function() {
        if (status2 === SUSPENDED) {
          if (!Scheduler.isDraining()) {
            Scheduler.enqueue(function() {
              run3(runTick);
            });
          } else {
            run3(runTick);
          }
        }
      }
    };
  }
  function runPar(util, supervisor, par, cb) {
    var fiberId = 0;
    var fibers = {};
    var killId = 0;
    var kills = {};
    var early = new Error("[ParAff] Early exit");
    var interrupt = null;
    var root = EMPTY;
    function kill(error3, par2, cb2) {
      var step2 = par2;
      var head2 = null;
      var tail = null;
      var count = 0;
      var kills2 = {};
      var tmp, kid;
      loop:
        while (true) {
          tmp = null;
          switch (step2.tag) {
            case FORKED:
              if (step2._3 === EMPTY) {
                tmp = fibers[step2._1];
                kills2[count++] = tmp.kill(error3, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head2 === null) {
                break loop;
              }
              step2 = head2._2;
              if (tail === null) {
                head2 = null;
              } else {
                head2 = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step2 = step2._2;
              break;
            case APPLY:
            case ALT:
              if (head2) {
                tail = new Aff2(CONS, head2, tail);
              }
              head2 = step2;
              step2 = step2._1;
              break;
          }
        }
      if (count === 0) {
        cb2(util.right(void 0))();
      } else {
        kid = 0;
        tmp = count;
        for (; kid < tmp; kid++) {
          kills2[kid] = kills2[kid]();
        }
      }
      return kills2;
    }
    function join2(result, head2, tail) {
      var fail2, step2, lhs, rhs, tmp, kid;
      if (util.isLeft(result)) {
        fail2 = result;
        step2 = null;
      } else {
        step2 = result;
        fail2 = null;
      }
      loop:
        while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head2 === null) {
            cb(fail2 || step2)();
            return;
          }
          if (head2._3 !== EMPTY) {
            return;
          }
          switch (head2.tag) {
            case MAP:
              if (fail2 === null) {
                head2._3 = util.right(head2._1(util.fromRight(step2)));
                step2 = head2._3;
              } else {
                head2._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (fail2) {
                head2._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail2 === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(fail2, null, null);
                    } else {
                      join2(fail2, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step2 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head2._3 = step2;
              }
              break;
            case ALT:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step2 === lhs ? rhs : lhs;
                step2 = null;
                head2._3 = fail2;
              } else {
                head2._3 = step2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, step2 === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join2(step2, null, null);
                    } else {
                      join2(step2, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail === null) {
            head2 = null;
          } else {
            head2 = tail._1;
            tail = tail._2;
          }
        }
    }
    function resolve5(fiber) {
      return function(result) {
        return function() {
          delete fibers[fiber._1];
          fiber._3 = result;
          join2(result, fiber._2._1, fiber._2._2);
        };
      };
    }
    function run3() {
      var status2 = CONTINUE;
      var step2 = par;
      var head2 = null;
      var tail = null;
      var tmp, fid;
      loop:
        while (true) {
          tmp = null;
          fid = null;
          switch (status2) {
            case CONTINUE:
              switch (step2.tag) {
                case MAP:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                  step2 = step2._2;
                  break;
                case APPLY:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                case ALT:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                  step2 = step2._1;
                  break;
                default:
                  fid = fiberId++;
                  status2 = RETURN;
                  tmp = step2;
                  step2 = new Aff2(FORKED, fid, new Aff2(CONS, head2, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve5(step2)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head2 === null) {
                break loop;
              }
              if (head2._1 === EMPTY) {
                head2._1 = step2;
                status2 = CONTINUE;
                step2 = head2._2;
                head2._2 = EMPTY;
              } else {
                head2._2 = step2;
                step2 = head2;
                if (tail === null) {
                  head2 = null;
                } else {
                  head2 = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
      root = step2;
      for (fid = 0; fid < fiberId; fid++) {
        fibers[fid].run();
      }
    }
    function cancel(error3, cb2) {
      interrupt = util.left(error3);
      var innerKills;
      for (var kid in kills) {
        if (kills.hasOwnProperty(kid)) {
          innerKills = kills[kid];
          for (kid in innerKills) {
            if (innerKills.hasOwnProperty(kid)) {
              innerKills[kid]();
            }
          }
        }
      }
      kills = null;
      var newKills = kill(error3, root, cb2);
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            for (var kid2 in newKills) {
              if (newKills.hasOwnProperty(kid2)) {
                newKills[kid2]();
              }
            }
            return nonCanceler2;
          };
        });
      };
    }
    run3();
    return function(killError) {
      return new Aff2(ASYNC, function(killCb) {
        return function() {
          return cancel(killError, killCb);
        };
      });
    };
  }
  function sequential2(util, supervisor, par) {
    return new Aff2(ASYNC, function(cb) {
      return function() {
        return runPar(util, supervisor, par, cb);
      };
    });
  }
  Aff2.EMPTY = EMPTY;
  Aff2.Pure = AffCtr(PURE);
  Aff2.Throw = AffCtr(THROW);
  Aff2.Catch = AffCtr(CATCH);
  Aff2.Sync = AffCtr(SYNC);
  Aff2.Async = AffCtr(ASYNC);
  Aff2.Bind = AffCtr(BIND);
  Aff2.Bracket = AffCtr(BRACKET);
  Aff2.Fork = AffCtr(FORK);
  Aff2.Seq = AffCtr(SEQ);
  Aff2.ParMap = AffCtr(MAP);
  Aff2.ParApply = AffCtr(APPLY);
  Aff2.ParAlt = AffCtr(ALT);
  Aff2.Fiber = Fiber;
  Aff2.Supervisor = Supervisor;
  Aff2.Scheduler = Scheduler;
  Aff2.nonCanceler = nonCanceler2;
  return Aff2;
}();
var _pure = Aff.Pure;
var _throwError = Aff.Throw;
function _map(f) {
  return function(aff) {
    if (aff.tag === Aff.Pure.tag) {
      return Aff.Pure(f(aff._1));
    } else {
      return Aff.Bind(aff, function(value12) {
        return Aff.Pure(f(value12));
      });
    }
  };
}
function _bind(aff) {
  return function(k) {
    return Aff.Bind(aff, k);
  };
}
var _liftEffect = Aff.Sync;
function _parAffMap(f) {
  return function(aff) {
    return Aff.ParMap(f, aff);
  };
}
function _parAffApply(aff1) {
  return function(aff2) {
    return Aff.ParApply(aff1, aff2);
  };
}
var makeAff = Aff.Async;
function _makeFiber(util, aff) {
  return function() {
    return Aff.Fiber(util, null, aff);
  };
}
var _sequential = Aff.Seq;

// output/Effect.Exception/foreign.js
function error(msg) {
  return new Error(msg);
}
function throwException(e) {
  return function() {
    throw e;
  };
}

// output/Effect.Exception/index.js
var $$throw = function($4) {
  return throwException(error($4));
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};

// output/Control.Monad.Except.Trans/index.js
var map2 = /* @__PURE__ */ map(functorEither);
var ExceptT = function(x) {
  return x;
};
var runExceptT = function(v) {
  return v;
};
var mapExceptT = function(f) {
  return function(v) {
    return f(v);
  };
};
var functorExceptT = function(dictFunctor) {
  var map13 = map(dictFunctor);
  return {
    map: function(f) {
      return mapExceptT(map13(map2(f)));
    }
  };
};
var monadExceptT = function(dictMonad) {
  return {
    Applicative0: function() {
      return applicativeExceptT(dictMonad);
    },
    Bind1: function() {
      return bindExceptT(dictMonad);
    }
  };
};
var bindExceptT = function(dictMonad) {
  var bind5 = bind(dictMonad.Bind1());
  var pure4 = pure(dictMonad.Applicative0());
  return {
    bind: function(v) {
      return function(k) {
        return bind5(v)(either(function($187) {
          return pure4(Left.create($187));
        })(function(a) {
          var v1 = k(a);
          return v1;
        }));
      };
    },
    Apply0: function() {
      return applyExceptT(dictMonad);
    }
  };
};
var applyExceptT = function(dictMonad) {
  var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: ap(monadExceptT(dictMonad)),
    Functor0: function() {
      return functorExceptT1;
    }
  };
};
var applicativeExceptT = function(dictMonad) {
  return {
    pure: function() {
      var $188 = pure(dictMonad.Applicative0());
      return function($189) {
        return ExceptT($188(Right.create($189)));
      };
    }(),
    Apply0: function() {
      return applyExceptT(dictMonad);
    }
  };
};
var monadThrowExceptT = function(dictMonad) {
  var monadExceptT1 = monadExceptT(dictMonad);
  return {
    throwError: function() {
      var $198 = pure(dictMonad.Applicative0());
      return function($199) {
        return ExceptT($198(Left.create($199)));
      };
    }(),
    Monad0: function() {
      return monadExceptT1;
    }
  };
};

// output/Control.Parallel.Class/index.js
var sequential = function(dict) {
  return dict.sequential;
};
var parallel = function(dict) {
  return dict.parallel;
};

// output/Control.Parallel/index.js
var identity6 = /* @__PURE__ */ identity(categoryFn);
var parTraverse_ = function(dictParallel) {
  var sequential2 = sequential(dictParallel);
  var parallel3 = parallel(dictParallel);
  return function(dictApplicative) {
    var traverse_2 = traverse_(dictApplicative);
    return function(dictFoldable) {
      var traverse_1 = traverse_2(dictFoldable);
      return function(f) {
        var $51 = traverse_1(function($53) {
          return parallel3(f($53));
        });
        return function($52) {
          return sequential2($51($52));
        };
      };
    };
  };
};
var parSequence_ = function(dictParallel) {
  var parTraverse_1 = parTraverse_(dictParallel);
  return function(dictApplicative) {
    var parTraverse_2 = parTraverse_1(dictApplicative);
    return function(dictFoldable) {
      return parTraverse_2(dictFoldable)(identity6);
    };
  };
};

// output/Partial.Unsafe/foreign.js
var _unsafePartial = function(f) {
  return f();
};

// output/Partial/foreign.js
var _crashWith = function(msg) {
  throw new Error(msg);
};

// output/Partial/index.js
var crashWith = function() {
  return _crashWith;
};

// output/Partial.Unsafe/index.js
var crashWith2 = /* @__PURE__ */ crashWith();
var unsafePartial = _unsafePartial;
var unsafeCrashWith = function(msg) {
  return unsafePartial(function() {
    return crashWith2(msg);
  });
};

// output/Effect.Aff/index.js
var $runtime_lazy2 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var $$void2 = /* @__PURE__ */ $$void(functorEffect);
var Canceler = function(x) {
  return x;
};
var functorParAff = {
  map: _parAffMap
};
var functorAff = {
  map: _map
};
var ffiUtil = /* @__PURE__ */ function() {
  var unsafeFromRight = function(v) {
    if (v instanceof Right) {
      return v.value0;
    }
    ;
    if (v instanceof Left) {
      return unsafeCrashWith("unsafeFromRight: Left");
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
  };
  var unsafeFromLeft = function(v) {
    if (v instanceof Left) {
      return v.value0;
    }
    ;
    if (v instanceof Right) {
      return unsafeCrashWith("unsafeFromLeft: Right");
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
  };
  var isLeft = function(v) {
    if (v instanceof Left) {
      return true;
    }
    ;
    if (v instanceof Right) {
      return false;
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
  };
  return {
    isLeft,
    fromLeft: unsafeFromLeft,
    fromRight: unsafeFromRight,
    left: Left.create,
    right: Right.create
  };
}();
var makeFiber = function(aff) {
  return _makeFiber(ffiUtil, aff);
};
var launchAff = function(aff) {
  return function __do() {
    var fiber = makeFiber(aff)();
    fiber.run();
    return fiber;
  };
};
var launchAff_ = function($75) {
  return $$void2(launchAff($75));
};
var applyParAff = {
  apply: _parAffApply,
  Functor0: function() {
    return functorParAff;
  }
};
var monadAff = {
  Applicative0: function() {
    return applicativeAff;
  },
  Bind1: function() {
    return bindAff;
  }
};
var bindAff = {
  bind: _bind,
  Apply0: function() {
    return $lazy_applyAff(0);
  }
};
var applicativeAff = {
  pure: _pure,
  Apply0: function() {
    return $lazy_applyAff(0);
  }
};
var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
  return {
    apply: ap(monadAff),
    Functor0: function() {
      return functorAff;
    }
  };
});
var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
var pure2 = /* @__PURE__ */ pure(applicativeAff);
var parallelAff = {
  parallel: unsafeCoerce2,
  sequential: _sequential,
  Apply0: function() {
    return applyAff;
  },
  Apply1: function() {
    return applyParAff;
  }
};
var parallel2 = /* @__PURE__ */ parallel(parallelAff);
var applicativeParAff = {
  pure: function($76) {
    return parallel2(pure2($76));
  },
  Apply0: function() {
    return applyParAff;
  }
};
var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableArray);
var semigroupCanceler = {
  append: function(v) {
    return function(v1) {
      return function(err) {
        return parSequence_2([v(err), v1(err)]);
      };
    };
  }
};
var monadEffectAff = {
  liftEffect: _liftEffect,
  Monad0: function() {
    return monadAff;
  }
};
var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
var effectCanceler = function($77) {
  return Canceler($$const(liftEffect2($77)));
};
var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure2(unit));
var monoidCanceler = {
  mempty: nonCanceler,
  Semigroup0: function() {
    return semigroupCanceler;
  }
};

// output/Effect.Console/foreign.js
var log = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Data.String.Common/foreign.js
var toLower = function(s) {
  return s.toLowerCase();
};

// output/Data.NonEmpty/index.js
var NonEmpty = /* @__PURE__ */ function() {
  function NonEmpty2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  NonEmpty2.create = function(value0) {
    return function(value1) {
      return new NonEmpty2(value0, value1);
    };
  };
  return NonEmpty2;
}();
var singleton2 = function(dictPlus) {
  var empty4 = empty(dictPlus);
  return function(a) {
    return new NonEmpty(a, empty4);
  };
};

// output/Data.List.Types/index.js
var Nil = /* @__PURE__ */ function() {
  function Nil2() {
  }
  ;
  Nil2.value = new Nil2();
  return Nil2;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons2.create = function(value0) {
    return function(value1) {
      return new Cons2(value0, value1);
    };
  };
  return Cons2;
}();
var NonEmptyList = function(x) {
  return x;
};
var listMap = function(f) {
  var chunkedRevMap = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
          $tco_var_v = new Cons(v1, v);
          $copy_v1 = v1.value1.value1.value1;
          return;
        }
        ;
        var unrolledMap = function(v2) {
          if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
            return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
          }
          ;
          if (v2 instanceof Cons && v2.value1 instanceof Nil) {
            return new Cons(f(v2.value0), Nil.value);
          }
          ;
          return Nil.value;
        };
        var reverseUnrolledMap = function($copy_v2) {
          return function($copy_v3) {
            var $tco_var_v2 = $copy_v2;
            var $tco_done1 = false;
            var $tco_result2;
            function $tco_loop2(v2, v3) {
              if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                $tco_var_v2 = v2.value1;
                $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                return;
              }
              ;
              $tco_done1 = true;
              return v3;
            }
            ;
            while (!$tco_done1) {
              $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
            }
            ;
            return $tco_result2;
          };
        };
        $tco_done = true;
        return reverseUnrolledMap(v)(unrolledMap(v1));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return chunkedRevMap(Nil.value);
};
var functorList = {
  map: listMap
};
var foldableList = {
  foldr: function(f) {
    return function(b) {
      var rev3 = function() {
        var go2 = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v;
              }
              ;
              if (v1 instanceof Cons) {
                $tco_var_v = new Cons(v1.value0, v);
                $copy_v1 = v1.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go2(Nil.value);
      }();
      var $284 = foldl(foldableList)(flip(f))(b);
      return function($285) {
        return $284(rev3($285));
      };
    };
  },
  foldl: function(f) {
    var go2 = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2;
  },
  foldMap: function(dictMonoid) {
    var append2 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $286 = append2(acc);
        return function($287) {
          return $286(f($287));
        };
      })(mempty3);
    };
  }
};
var foldr2 = /* @__PURE__ */ foldr(foldableList);
var semigroupList = {
  append: function(xs) {
    return function(ys) {
      return foldr2(Cons.create)(ys)(xs);
    };
  }
};
var append1 = /* @__PURE__ */ append(semigroupList);
var altList = {
  alt: append1,
  Functor0: function() {
    return functorList;
  }
};
var plusList = /* @__PURE__ */ function() {
  return {
    empty: Nil.value,
    Alt0: function() {
      return altList;
    }
  };
}();

// output/Data.Map.Internal/index.js
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
var insert = function(dictOrd) {
  var compare3 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go2 = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton3(k)(v);
        }
        ;
        if (v1 instanceof Node) {
          var v2 = compare3(k)(v1.value2);
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

// output/Data.String.CaseInsensitive/index.js
var compare2 = /* @__PURE__ */ compare(ordString);
var CaseInsensitiveString = function(x) {
  return x;
};
var eqCaseInsensitiveString = {
  eq: function(v) {
    return function(v1) {
      return toLower(v) === toLower(v1);
    };
  }
};
var ordCaseInsensitiveString = {
  compare: function(v) {
    return function(v1) {
      return compare2(toLower(v))(toLower(v1));
    };
  },
  Eq0: function() {
    return eqCaseInsensitiveString;
  }
};

// output/JS.Fetch.Headers/foreign.js
function unsafeFromRecord(r) {
  return new Headers(r);
}
function _toArray(tuple, headers2) {
  return Array.from(headers2.entries(), function(pair) {
    return tuple(pair[0])(pair[1]);
  });
}

// output/JS.Fetch.Headers/index.js
var toArray = /* @__PURE__ */ function() {
  return runFn2(_toArray)(Tuple.create);
}();
var fromRecord = function() {
  return unsafeFromRecord;
};

// output/Fetch.Internal.Headers/index.js
var toHeaders = /* @__PURE__ */ function() {
  var $7 = fromFoldable(ordCaseInsensitiveString)(foldableArray);
  var $8 = map(functorArray)(lmap(bifunctorTuple)(CaseInsensitiveString));
  return function($9) {
    return $7($8(toArray($9)));
  };
}();

// output/JS.Fetch.Request/foreign.js
function _unsafeNew(url3, options2) {
  try {
    return new Request(url3, options2);
  } catch (e) {
    console.error(e);
    throw e;
  }
}

// output/Effect.Uncurried/foreign.js
var mkEffectFn1 = function mkEffectFn12(fn) {
  return function(x) {
    return fn(x)();
  };
};
var runEffectFn2 = function runEffectFn22(fn) {
  return function(a) {
    return function(b) {
      return function() {
        return fn(a, b);
      };
    };
  };
};

// output/Record/index.js
var insert2 = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  return function() {
    return function() {
      return function(l) {
        return function(a) {
          return function(r) {
            return unsafeSet(reflectSymbol2(l))(a)(r);
          };
        };
      };
    };
  };
};
var get = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  return function() {
    return function(l) {
      return function(r) {
        return unsafeGet(reflectSymbol2(l))(r);
      };
    };
  };
};
var $$delete = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  return function() {
    return function() {
      return function(l) {
        return function(r) {
          return unsafeDelete(reflectSymbol2(l))(r);
        };
      };
    };
  };
};

// output/Fetch.Internal.Request/index.js
var fromRecord2 = /* @__PURE__ */ fromRecord();
var toCoreRequestOptionsHelpe = {
  convertHelper: function(v) {
    return function(v1) {
      return {};
    };
  }
};
var toCoreRequestOptionsConve8 = function() {
  return {
    convertImpl: function(v) {
      return fromRecord2;
    }
  };
};
var $$new2 = function() {
  return function(url3) {
    return function(options2) {
      return function() {
        return _unsafeNew(url3, options2);
      };
    };
  };
};
var convertImpl = function(dict) {
  return dict.convertImpl;
};
var convertHelper = function(dict) {
  return dict.convertHelper;
};
var toCoreRequestOptionsHelpe1 = function(dictToCoreRequestOptionsConverter) {
  var convertImpl1 = convertImpl(dictToCoreRequestOptionsConverter);
  return function() {
    return function() {
      return function() {
        return function(dictIsSymbol) {
          var $$delete2 = $$delete(dictIsSymbol)()();
          var get2 = get(dictIsSymbol)();
          var insert3 = insert2(dictIsSymbol)()();
          return function(dictToCoreRequestOptionsHelper) {
            var convertHelper1 = convertHelper(dictToCoreRequestOptionsHelper);
            return function() {
              return function() {
                return {
                  convertHelper: function(v) {
                    return function(r) {
                      var tail = convertHelper1($$Proxy.value)($$delete2($$Proxy.value)(r));
                      var head2 = convertImpl1($$Proxy.value)(get2($$Proxy.value)(r));
                      return insert3($$Proxy.value)(head2)(tail);
                    };
                  }
                };
              };
            };
          };
        };
      };
    };
  };
};
var toCoreRequestOptionsRowRo = function() {
  return function() {
    return function(dictToCoreRequestOptionsHelper) {
      return {
        convert: convertHelper(dictToCoreRequestOptionsHelper)($$Proxy.value)
      };
    };
  };
};
var convert = function(dict) {
  return dict.convert;
};

// output/JS.Fetch.Response/foreign.js
function headers(resp) {
  return resp.headers;
}
function ok(resp) {
  return resp.ok;
}
function redirected(resp) {
  return resp.redirected;
}
function status(resp) {
  return resp.status;
}
function statusText(resp) {
  return resp.statusText;
}
function url(resp) {
  return resp.url;
}
function body(resp) {
  return function() {
    return resp.body;
  };
}
function arrayBuffer(resp) {
  return function() {
    return resp.arrayBuffer();
  };
}
function blob(resp) {
  return function() {
    return resp.blob();
  };
}
function text(resp) {
  return function() {
    return resp.text();
  };
}
function json(resp) {
  return function() {
    return resp.json();
  };
}

// output/Control.Monad.Except/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var runExcept = function($3) {
  return unwrap2(runExceptT($3));
};

// output/Foreign/foreign.js
function tagOf(value12) {
  return Object.prototype.toString.call(value12).slice(8, -1);
}
var isArray = Array.isArray || function(value12) {
  return Object.prototype.toString.call(value12) === "[object Array]";
};

// output/Data.List.NonEmpty/index.js
var singleton4 = /* @__PURE__ */ function() {
  var $200 = singleton2(plusList);
  return function($201) {
    return NonEmptyList($200($201));
  };
}();

// output/Foreign/index.js
var TypeMismatch = /* @__PURE__ */ function() {
  function TypeMismatch2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  TypeMismatch2.create = function(value0) {
    return function(value1) {
      return new TypeMismatch2(value0, value1);
    };
  };
  return TypeMismatch2;
}();
var unsafeToForeign = unsafeCoerce2;
var unsafeFromForeign = unsafeCoerce2;
var fail = function(dictMonad) {
  var $153 = throwError(monadThrowExceptT(dictMonad));
  return function($154) {
    return $153(singleton4($154));
  };
};
var unsafeReadTagged = function(dictMonad) {
  var pure12 = pure(applicativeExceptT(dictMonad));
  var fail1 = fail(dictMonad);
  return function(tag) {
    return function(value12) {
      if (tagOf(value12) === tag) {
        return pure12(unsafeFromForeign(value12));
      }
      ;
      if (otherwise) {
        return fail1(new TypeMismatch(tag, tagOf(value12)));
      }
      ;
      throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value12.constructor.name]);
    };
  };
};
var readString = function(dictMonad) {
  return unsafeReadTagged(dictMonad)("String");
};

// output/Promise.Internal/foreign.js
function thenOrCatch(k, c, p) {
  return p.then(k, c);
}
function resolve(a) {
  return Promise.resolve(a);
}

// output/Promise.Rejection/foreign.js
function _toError(just, nothing, ref) {
  if (ref instanceof Error) {
    return just(ref);
  }
  return nothing;
}

// output/Promise.Rejection/index.js
var toError = /* @__PURE__ */ function() {
  return runFn3(_toError)(Just.create)(Nothing.value);
}();

// output/Promise/index.js
var thenOrCatch2 = function() {
  return function(k) {
    return function(c) {
      return function(p) {
        return function() {
          return thenOrCatch(mkEffectFn1(k), mkEffectFn1(c), p);
        };
      };
    };
  };
};
var resolve2 = function() {
  return resolve;
};

// output/Promise.Aff/index.js
var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
var mempty2 = /* @__PURE__ */ mempty(monoidCanceler);
var thenOrCatch3 = /* @__PURE__ */ thenOrCatch2();
var map3 = /* @__PURE__ */ map(functorEffect);
var resolve3 = /* @__PURE__ */ resolve2();
var alt2 = /* @__PURE__ */ alt(altMaybe);
var map1 = /* @__PURE__ */ map(functorMaybe);
var readString2 = /* @__PURE__ */ readString(monadIdentity);
var bind2 = /* @__PURE__ */ bind(bindAff);
var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
var toAff$prime = function(customCoerce) {
  return function(p) {
    return makeAff(function(cb) {
      return voidRight2(mempty2)(thenOrCatch3(function(a) {
        return map3(resolve3)(cb(new Right(a)));
      })(function(e) {
        return map3(resolve3)(cb(new Left(customCoerce(e))));
      })(p));
    });
  };
};
var coerce3 = function(rej) {
  return fromMaybe$prime(function(v) {
    return error("Promise failed, couldn't extract JS Error or String");
  })(alt2(toError(rej))(map1(error)(hush(runExcept(readString2(unsafeToForeign(rej)))))));
};
var toAff = /* @__PURE__ */ toAff$prime(coerce3);
var toAffE = function(f) {
  return bind2(liftEffect3(f))(toAff);
};

// output/Fetch.Internal.Response/index.js
var text2 = function(response) {
  return toAffE(text(response));
};
var json2 = function(response) {
  return toAffE(json(response));
};
var blob2 = function(response) {
  return toAffE(blob(response));
};
var arrayBuffer2 = function(response) {
  return toAffE(arrayBuffer(response));
};
var convert2 = function(response) {
  return {
    headers: toHeaders(headers(response)),
    ok: ok(response),
    redirected: redirected(response),
    status: status(response),
    statusText: statusText(response),
    url: url(response),
    text: text2(response),
    json: json2(response),
    body: body(response),
    arrayBuffer: arrayBuffer2(response),
    blob: blob2(response)
  };
};

// output/JS.Fetch/foreign.js
function _fetch(a, b) {
  return fetch(a, b);
}

// output/JS.Fetch/index.js
var fetchWithOptions = function() {
  return runEffectFn2(_fetch);
};

// output/JS.Fetch.AbortController/foreign.js
var newImpl2 = function() {
  return new AbortController();
};
function abort(controller) {
  return function() {
    return controller.abort();
  };
}
function signal(controller) {
  return controller.signal;
}

// output/Fetch/index.js
var $$void3 = /* @__PURE__ */ $$void(functorEffect);
var thenOrCatch4 = /* @__PURE__ */ thenOrCatch2();
var map4 = /* @__PURE__ */ map(functorEffect);
var resolve4 = /* @__PURE__ */ resolve2();
var bind3 = /* @__PURE__ */ bind(bindAff);
var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
var $$new4 = /* @__PURE__ */ $$new2();
var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindAff);
var fetchWithOptions2 = /* @__PURE__ */ fetchWithOptions();
var pure1 = /* @__PURE__ */ pure(applicativeAff);
var toAbortableAff = function(abortController) {
  return function(p) {
    return makeAff(function(cb) {
      return function __do() {
        $$void3(thenOrCatch4(function(a) {
          return map4(resolve4)(cb(new Right(a)));
        })(function(e) {
          return map4(resolve4)(cb(new Left(coerce3(e))));
        })(p))();
        return effectCanceler(abort(abortController));
      };
    });
  };
};
var fetch2 = function() {
  return function() {
    return function(dictToCoreRequestOptions) {
      var convert3 = convert(dictToCoreRequestOptions);
      return function(url3) {
        return function(r) {
          return bind3(liftEffect4($$new4(url3)(convert3(r))))(function(request) {
            return bind3(liftEffect4(newImpl2))(function(abortController) {
              var signal2 = signal(abortController);
              return bind3(bindFlipped2(toAbortableAff(abortController))(liftEffect4(fetchWithOptions2(request)({
                signal: signal2
              }))))(function(cResponse) {
                return pure1(convert2(cResponse));
              });
            });
          });
        };
      };
    };
  };
};

// output/Model/foreign.js
var canvas = document.getElementById("lcolonq-canvas");
var socket = null;
var currentFrame = null;
async function decompress(blob3) {
  let ds = new DecompressionStream("gzip");
  let stream = blob3.stream();
  let out = await new Response(stream.pipeThrough(ds));
  return out.arrayBuffer();
}
function readCell(dv, base) {
  let cell = {};
  let o = base;
  if (dv.getUint8(o) == 0) {
    return [{ type: "bg" }, o + 1];
  } else {
    cell.type = "fg";
    cell.custom = dv.getUint8(o + 1);
    cell.r = dv.getUint8(o + 2);
    cell.g = dv.getUint8(o + 3);
    cell.b = dv.getUint8(o + 4);
    cell.g0 = dv.getUint32(o + 5);
    if (dv.getUint8(o + 9) == 0) {
      return [cell, o + 10];
    } else {
      cell.g1 = dv.getUint32(o + 10);
      return [cell, o + 14];
    }
  }
}
function readKeyframe(dv, base) {
  let ret = [];
  let o = base;
  for (let idx = 0; idx < 64 * 64; ++idx) {
    let res = readCell(dv, o);
    ret.push(res[0]);
    o = res[1];
  }
  currentFrame = ret;
}
function readDiff(dv, base) {
  if (currentFrame) {
    let len = dv.getUint32(base);
    let o = base + 4;
    for (let idx = 0; idx < len; ++idx) {
      let x = dv.getUint8(o);
      let y = dv.getUint8(o + 1);
      let c = readCell(dv, o + 2);
      currentFrame[x + y * 64] = c[0];
      o = c[1];
    }
  }
}
function readPacket(dv) {
  if (dv.getUint8(0) == 0) {
    readKeyframe(dv, 1);
  } else {
    readDiff(dv, 1);
  }
}
function renderCellCanvas(ctx2, x, y, c) {
  if (c && c.type === "fg") {
    let msg = c.g1 ? String.fromCodePoint(c.g0, c.g1) : String.fromCodePoint(c.g0);
    if (msg.trim().length) {
      ctx2.fillStyle = "black";
      ctx2.fillRect(13 * y, 13 * x, 13, 13);
      ctx2.fillStyle = `rgba(${c.r}, ${c.g}, ${c.b}, 1.0)`;
      ctx2.fillText(msg, 13 * y, 13 * x + 10);
    }
  }
}
function renderCanvas() {
  if (canvas.width != canvas.clientWidth) {
    canvas.width = canvas.clientWidth;
  }
  if (canvas.height != canvas.clientHeight) {
    canvas.height = canvas.clientHeight;
  }
  if (currentFrame) {
    let ctx2 = canvas.getContext("2d");
    ctx2.clearRect(0, 0, canvas.width, canvas.height);
    ctx2.font = "12px Iosevka Comfy";
    for (let y = 0; y < 64; ++y) {
      for (let x = 0; x < 64; ++x) {
        renderCellCanvas(ctx2, x, y, currentFrame[x * 64 + y]);
      }
    }
  }
}
var _startModel = () => {
  socket = new WebSocket("wss://colonq.computer/bullfrog/api/channel/listen/model");
  socket.addEventListener("open", (ev) => {
    console.log("connected");
  });
  socket.addEventListener("message", async (ev) => {
    let arr = await decompress(ev.data);
    let view = new DataView(arr);
    readPacket(view);
    renderCanvas();
  });
};

// output/Model/index.js
var startModel = function(dictMonadEffect) {
  return liftEffect(dictMonadEffect)(_startModel);
};

// output/UI/foreign.js
var _setInterval = (delay) => (f) => () => setInterval(f, delay);

// output/UI/index.js
var setInterval2 = function(dictMonadEffect) {
  var liftEffect6 = liftEffect(dictMonadEffect);
  return function(d) {
    return function(f) {
      return liftEffect6(_setInterval(d)(f));
    };
  };
};

// output/Web.DOM.DOMTokenList/foreign.js
function add2(list) {
  return function(token) {
    return function() {
      return list.add(token);
    };
  };
}

// output/Data.Nullable/foreign.js
function nullable(a, r, f) {
  return a == null ? r : f(a);
}

// output/Data.Nullable/index.js
var toMaybe = function(n) {
  return nullable(n, Nothing.value, Just.create);
};

// output/Web.DOM.Document/foreign.js
var getEffProp = function(name15) {
  return function(doc) {
    return function() {
      return doc[name15];
    };
  };
};
var url2 = getEffProp("URL");
var documentURI = getEffProp("documentURI");
var origin = getEffProp("origin");
var compatMode = getEffProp("compatMode");
var characterSet = getEffProp("characterSet");
var contentType = getEffProp("contentType");
var _documentElement = getEffProp("documentElement");
function createElement(localName2) {
  return function(doc) {
    return function() {
      return doc.createElement(localName2);
    };
  };
}
function createTextNode(data) {
  return function(doc) {
    return function() {
      return doc.createTextNode(data);
    };
  };
}

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

// output/Web.DOM.Document/index.js
var toParentNode = unsafeCoerce2;
var toNonElementParentNode = unsafeCoerce2;

// output/Web.DOM.Element/foreign.js
var getProp = function(name15) {
  return function(doctype) {
    return doctype[name15];
  };
};
var _namespaceURI = getProp("namespaceURI");
var _prefix = getProp("prefix");
var localName = getProp("localName");
var tagName = getProp("tagName");
function classList(element) {
  return function() {
    return element.classList;
  };
}

// output/Web.DOM.ParentNode/foreign.js
var getEffProp2 = function(name15) {
  return function(node) {
    return function() {
      return node[name15];
    };
  };
};
var children = getEffProp2("children");
var _firstElementChild = getEffProp2("firstElementChild");
var _lastElementChild = getEffProp2("lastElementChild");
var childElementCount = getEffProp2("childElementCount");
function querySelectorAll(selector) {
  return function(node) {
    return function() {
      return node.querySelectorAll(selector);
    };
  };
}

// output/Web.DOM.Element/index.js
var toNode = unsafeCoerce2;
var toEventTarget = unsafeCoerce2;
var fromNode = /* @__PURE__ */ unsafeReadProtoTagged("Element");

// output/Web.DOM.Node/foreign.js
var getEffProp3 = function(name15) {
  return function(node) {
    return function() {
      return node[name15];
    };
  };
};
var baseURI = getEffProp3("baseURI");
var _ownerDocument = getEffProp3("ownerDocument");
var _parentNode = getEffProp3("parentNode");
var _parentElement = getEffProp3("parentElement");
var childNodes = getEffProp3("childNodes");
var _firstChild = getEffProp3("firstChild");
var _lastChild = getEffProp3("lastChild");
var _previousSibling = getEffProp3("previousSibling");
var _nextSibling = getEffProp3("nextSibling");
var _nodeValue = getEffProp3("nodeValue");
var textContent = getEffProp3("textContent");
function setTextContent(value12) {
  return function(node) {
    return function() {
      node.textContent = value12;
    };
  };
}
function appendChild(node) {
  return function(parent2) {
    return function() {
      parent2.appendChild(node);
    };
  };
}

// output/Web.DOM.NodeList/foreign.js
function toArray2(list) {
  return function() {
    return [].slice.call(list);
  };
}

// output/Web.DOM.NonElementParentNode/foreign.js
function _getElementById(id2) {
  return function(node) {
    return function() {
      return node.getElementById(id2);
    };
  };
}

// output/Web.DOM.NonElementParentNode/index.js
var map5 = /* @__PURE__ */ map(functorEffect);
var getElementById = function(eid) {
  var $2 = map5(toMaybe);
  var $3 = _getElementById(eid);
  return function($4) {
    return $2($3($4));
  };
};

// output/Web.DOM.Text/index.js
var toNode2 = unsafeCoerce2;

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

// output/Web.HTML.HTMLDocument/index.js
var toDocument = unsafeCoerce2;

// output/Web.HTML.Window/foreign.js
function document2(window2) {
  return function() {
    return window2.document;
  };
}

// output/Main/index.js
var map6 = /* @__PURE__ */ map(functorEffect);
var fold4 = /* @__PURE__ */ fold(foldableArray)(monoidArray);
var map12 = /* @__PURE__ */ map(functorArray);
var startModel2 = /* @__PURE__ */ startModel(monadEffectAff);
var bind4 = /* @__PURE__ */ bind(bindAff);
var fetch3 = /* @__PURE__ */ fetch2()();
var toCoreRequestOptionsRowRo2 = /* @__PURE__ */ toCoreRequestOptionsRowRo()();
var fetch1 = /* @__PURE__ */ fetch3(/* @__PURE__ */ toCoreRequestOptionsRowRo2(/* @__PURE__ */ toCoreRequestOptionsHelpe1(/* @__PURE__ */ toCoreRequestOptionsConve8())()()()({
  reflectSymbol: function() {
    return "headers";
  }
})(toCoreRequestOptionsHelpe)()()));
var fetch22 = /* @__PURE__ */ fetch3(/* @__PURE__ */ toCoreRequestOptionsRowRo2(toCoreRequestOptionsHelpe));
var discard2 = /* @__PURE__ */ discard(discardUnit);
var discard1 = /* @__PURE__ */ discard2(bindAff);
var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
var getToken2 = /* @__PURE__ */ getToken(monadEffectAff);
var pure3 = /* @__PURE__ */ pure(applicativeAff);
var startTwitchAuth2 = /* @__PURE__ */ startTwitchAuth(monadEffectEffect);
var for_2 = /* @__PURE__ */ for_(applicativeAff)(foldableArray);
var show2 = /* @__PURE__ */ show(showInt);
var playVoice2 = /* @__PURE__ */ playVoice(monadEffectEffect);
var setInterval3 = /* @__PURE__ */ setInterval2(monadEffectAff);
var setText5 = function(dictMonadEffect) {
  var liftEffect1 = liftEffect(dictMonadEffect);
  return function(e) {
    return function(s) {
      return liftEffect1(setTextContent(s)(toNode(e)));
    };
  };
};
var setText1 = /* @__PURE__ */ setText5(monadEffectAff);
var maybeToArray = function(v) {
  if (v instanceof Just) {
    return [v.value0];
  }
  ;
  if (v instanceof Nothing) {
    return [];
  }
  ;
  throw new Error("Failed pattern match at Main (line 38, column 1 - line 38, column 45): " + [v.constructor.name]);
};
var queryAll = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var bind22 = bind(Monad0.Bind1());
  var liftEffect1 = liftEffect(dictMonadEffect);
  var pure12 = pure(Monad0.Applicative0());
  return function(q) {
    return bind22(liftEffect1(windowImpl))(function(w) {
      return bind22(liftEffect1(map6(toDocument)(document2(w))))(function(d) {
        return bind22(liftEffect1(querySelectorAll(q)(toParentNode(d))))(function(nl) {
          return bind22(liftEffect1(toArray2(nl)))(function(ns) {
            return pure12(fold4(map12(function($118) {
              return maybeToArray(fromNode($118));
            })(ns)));
          });
        });
      });
    });
  };
};
var query = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var bind22 = bind(Monad0.Bind1());
  var queryAll1 = queryAll(dictMonadEffect);
  var liftEffect1 = liftEffect(dictMonadEffect);
  var pure12 = pure(Monad0.Applicative0());
  return function(q) {
    return bind22(queryAll1(q))(function($119) {
      return function(v) {
        if (v instanceof Nothing) {
          return liftEffect1($$throw("could not find element matching query: " + q));
        }
        ;
        if (v instanceof Just) {
          return pure12(v.value0);
        }
        ;
        throw new Error("Failed pattern match at Main (line 60, column 27 - line 62, column 21): " + [v.constructor.name]);
      }(head($119));
    });
  };
};
var query1 = /* @__PURE__ */ query(monadEffectEffect);
var mainObs = /* @__PURE__ */ launchAff_(startModel2);
var listen2 = function(dictMonadEffect) {
  var bind22 = bind(dictMonadEffect.Monad0().Bind1());
  var liftEffect1 = liftEffect(dictMonadEffect);
  return function(e) {
    return function(ev) {
      return function(f) {
        return bind22(liftEffect1(eventListener(f)))(function(l) {
          return liftEffect1(addEventListener(ev)(l)(false)(toEventTarget(e)));
        });
      };
    };
  };
};
var listen1 = /* @__PURE__ */ listen2(monadEffectAff);
var checkAuth = function(auth) {
  return bind4(fetch1(apiServer + "/check")({
    headers: {
      Authorization: authHeader(auth)
    }
  }))(function(v) {
    return v.text;
  });
};
var byId = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var bind22 = bind(Monad0.Bind1());
  var liftEffect1 = liftEffect(dictMonadEffect);
  var pure12 = pure(Monad0.Applicative0());
  return function(i) {
    return bind22(liftEffect1(windowImpl))(function(w) {
      return bind22(liftEffect1(map6(toDocument)(document2(w))))(function(d) {
        return bind22(liftEffect1(getElementById(i)(toNonElementParentNode(d))))(function(v) {
          if (v instanceof Nothing) {
            return liftEffect1($$throw("could not find element with id: " + i));
          }
          ;
          if (v instanceof Just) {
            return pure12(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Main (line 46, column 80 - line 48, column 21): " + [v.constructor.name]);
        });
      });
    });
  };
};
var byId1 = /* @__PURE__ */ byId(monadEffectAff);
var updateSubtitle = /* @__PURE__ */ bind4(/* @__PURE__ */ byId1("lcolonq-subtitle"))(function(subtitle) {
  return bind4(fetch22(apiServer + "/catchphrase")({}))(function(v) {
    return bind4(v.text)(setText1(subtitle));
  });
});
var mainHomepage = /* @__PURE__ */ launchAff_(/* @__PURE__ */ discard1(/* @__PURE__ */ liftEffect5(/* @__PURE__ */ log("hi")))(function() {
  return discard1(startModel2)(function() {
    return bind4(byId1("lcolonq-marquee"))(function(marq) {
      return bind4(fetch22(apiServer + "/motd")({}))(function(v) {
        return discard1(bind4(v.text)(setText1(marq)))(function() {
          return discard1(bind4(getToken2)(function(v1) {
            if (v1 instanceof Just) {
              return discard1(liftEffect5(log(v1.value0.value0)))(function() {
                return discard1(liftEffect5(log(v1.value0.value1)))(function() {
                  return bind4(checkAuth(v1.value0))(function($120) {
                    return liftEffect5(log($120));
                  });
                });
              });
            }
            ;
            return pure3(unit);
          }))(function() {
            return discard1(updateSubtitle)(function() {
              return bind4(byId1("lcolonq-subtitle"))(function(subtitle) {
                return discard1(listen1(subtitle)("click")(function(_ev) {
                  return function __do() {
                    startTwitchAuth2();
                    return launchAff_(updateSubtitle)();
                  };
                }))(function() {
                  return for_2(range2(0)(6))(function(i) {
                    return bind4(byId1("lcolonq-letter-" + show2(i)))(function(letter) {
                      return discard1(listen1(letter)("click")(function(_ev) {
                        return playVoice2(true)(i);
                      }))(function() {
                        return listen1(letter)("mouseover")(function(_ev) {
                          return playVoice2(false)(i);
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
}));
var appendText = function(dictMonadEffect) {
  var bind22 = bind(dictMonadEffect.Monad0().Bind1());
  var liftEffect1 = liftEffect(dictMonadEffect);
  return function(parent2) {
    return function(s) {
      return bind22(liftEffect1(windowImpl))(function(w) {
        return bind22(liftEffect1(map6(toDocument)(document2(w))))(function(d) {
          return bind22(liftEffect1(createTextNode(s)(d)))(function(n) {
            return liftEffect1(appendChild(toNode2(n))(toNode(parent2)));
          });
        });
      });
    };
  };
};
var appendText1 = /* @__PURE__ */ appendText(monadEffectEffect);
var appendElement = function(dictMonadEffect) {
  var liftEffect1 = liftEffect(dictMonadEffect);
  return function(parent2) {
    return function(child) {
      return liftEffect1(appendChild(toNode(child))(toNode(parent2)));
    };
  };
};
var appendElement1 = /* @__PURE__ */ appendElement(monadEffectEffect);
var create3 = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var Bind1 = Monad0.Bind1();
  var bind22 = bind(Bind1);
  var liftEffect1 = liftEffect(dictMonadEffect);
  var discard3 = discard2(Bind1);
  var Applicative0 = Monad0.Applicative0();
  var for_1 = for_(Applicative0)(foldableArray);
  var appendElement2 = appendElement(dictMonadEffect);
  var pure12 = pure(Applicative0);
  return function(tag) {
    return function(classes) {
      return function(children2) {
        return bind22(liftEffect1(windowImpl))(function(w) {
          return bind22(liftEffect1(map6(toDocument)(document2(w))))(function(d) {
            return bind22(liftEffect1(createElement(tag)(d)))(function(el) {
              return bind22(liftEffect1(classList(el)))(function(cl) {
                return discard3(for_1(classes)(function(c) {
                  return liftEffect1(add2(cl)(c));
                }))(function() {
                  return discard3(for_1(children2)(function(c) {
                    return appendElement2(el)(c);
                  }))(function() {
                    return pure12(el);
                  });
                });
              });
            });
          });
        });
      };
    };
  };
};
var create1 = /* @__PURE__ */ create3(monadEffectEffect);
var mainExtension = /* @__PURE__ */ launchAff_(/* @__PURE__ */ discard1(/* @__PURE__ */ liftEffect5(/* @__PURE__ */ log("hello from extension")))(function() {
  return setInterval3(1e3)(function __do() {
    var e = query1(".chat-scrollable-area__message-container")();
    var $$new5 = create1("div")([".chat-line__message"])([])();
    appendText1($$new5)("test")();
    return appendElement1(e)($$new5)();
  });
}));
var main = /* @__PURE__ */ function() {
  if (mode === 0) {
    return mainHomepage;
  }
  ;
  if (mode === 1) {
    return mainExtension;
  }
  ;
  if (mode === 2) {
    return mainObs;
  }
  ;
  return $$throw("unknown mode");
}();

// <stdin>
main();
