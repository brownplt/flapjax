"use strict";

/**
 * @namespace
 */
goog.provide('F');
goog.require('goog.structs.PriorityQueue');


/**
 * @namespace
 */
F.util = { };

F.util.identity = function(x) {
  return x;
};

F.util.iota = function(n) {
  var arr = [ ];
  for (var i = 0; i < n; i++) {
    arr.push(i);
  }
  return arr;
};

F.util.find = function(f, arr, notFound) {
  for (var i = 0; i < arr.length; i++) {
    if (f(arr[i]) === true) {
      return arr[i];
    }
  }
  return notFound;
};

F.util.now = function() {
  return (new Date()).valueOf();
};

/**
 * @returns {Array}
 */
F.util.mkArray = function(arrayLike) {
  return Array.prototype.slice.call(arrayLike);
};

/**
 * @param {goog.structs.PriorityQueue} q
 */
F.propagate_ = function(q) {
  var item;
  while (!q.isEmpty()) {
    item = q.dequeue();
    item.produce(q);
  }
};

/**
 * When an input does not carry a signal, its value is F.X.
 */
F.X = { 
  toString: function() { 
    return 'F.X'; 
  }
};

/**
 * The common prototype of all nodes.
 *
 * @param {*} valueNow The initial value. Use F.X if it is unintialized.
 * @param {number} rank 
 *
 * @constructor
 */
F.Node = function(valueNow, rank) {
  this.sendsTo_ = [];
  this.queuedNow_ = false;
  this.valueNow_ = valueNow;
  this.rank_ = rank;
};

/**
 * Attach a signal to receive values from this signal.
 *
 * @param {{key: *, signal: F.Node}} tuple
 */
F.Node.prototype.connect = function(tuple) {
  this.sendsTo_.push(tuple);
};

/**
 * Detach a signal from this signal.
 *
 * @param {F.Node} signal
 * @return {boolean}
 */
F.Node.prototype.disconnect = function(signal) {
  for (var i = 0; i < this.sendsTo_.length; i++) {
    if (this.sendsTo_[i].signal === signal) {
      return true;
    }
  }
  return false;
};

/**
 * Called by a dependent signal when its rank changes.
 *
 * Calls <code>childRankChanged</code> parents.
 *
 * @param {number} newChildRank
 */
F.Node.prototype.childRankChanged = function(newChildRank) {
  if (newChildRank < this.rank_) {
    return;
  }
  var newRank = newChildRank + 1;
  this.rank_ = newRank;
  this.sendsTo_.forEach(function(tup) {
                          tup.signal.childRankChanged(newRank);
                        });
};

/**
 * Send this signals value to <code>this.sendsTo_</code>.
 *
 * @param {goog.structs.PriorityQueue} q
 */
F.Node.prototype.produce = function(q) {
  var this_ = this;
  this.queuedNow_ = false;
  this.sendsTo_.forEach(function(tuple) {
                          tuple.signal.consume(q, tuple.key, this_);
                        });
};

/**
 * Receives values from dependents.
 *
 * @param {goog.structs.PriorityQueue} q
 * @param {string|number} k
 * @param {F.Node} child
 */
F.Node.prototype.consume = function(q, k, child) {
  throw 'F.Node does not consume values';
};

/**
 * @constructor
 * @extends F.Node
 * @param {F.Node} m
 * @param {function(*): F.Node} k
 * @private
 */
F.Bind = function(m, k) {
  this.m_ = m;
  this.k_ = k;
  if (m.valueNow_ === F.X) {
    this.r_ = new F.Node(F.X, 0);
  }
  else {
    this.r_ = this.k_(m.valueNow_);
  }
  F.Node.call(this, this.r_.valueNow_, 
              1 + Math.max(m.rank_, this.r_.rank_));
  m.connect({ key: 'm', signal: this });
  this.r_.connect({ key: 'r', signal: this });
};
goog.inherits(F.Bind, F.Node);

F.Bind.prototype.consume = function(q, k, child) {
  var v = child.valueNow_;
  if (k === 'm') {
    this.r_.disconnect(this);
    if (v === F.X) {
      this.r_ = new F.Node(F.X, 0);
    }
    else {
      this.r_ = this.k_(v);
    }
    this.childRankChanged(this.r_.rank_);
    this.valueNow_ = this.r_.valueNow_;
    q.enqueue(this.rank_, this);
    this.r_.connect({ key: 'r', signal: this });
    return;
  }
  if (k === 'r') {
    if (child !== this.r_) {
      return; // hack: stale signal
    }
    this.valueNow_ = v;
    q.enqueue(this.rank_, this);
    return;
  }
  throw 'unexpected key: ' + String(k);
};

/** 
 * @constructor
 * @extends F.Node
 * @private
 */
F.App = function(valueNow, f, args) {
  var this_ = this;
  var i = 0;
  args.forEach(function(arg) {
                 arg.connect({ key: i, signal: this_ });
                 i = i + 1;
               });
  this.f_ = f;
  this.args_ = args;
  var rank = 
    1 + Math.max.apply(null, args.map(function(a) { return a.rank_; }));
  F.Node.call(this, valueNow, rank);
};
goog.inherits(F.App, F.Node);

F.App.prototype.consume = function(q, k, child) {
  if (!this.queuedNow_) {
    this.queuedNow_ = true;
    q.enqueue(this.rank_, this);
  }
};

F.App.prototype.produce = function(q) {
  var this_ = this;
  this.queuedNow_ = false;
  var argVals = new Array(this.args_.length);
  var i = 0;
  this.args_.forEach(function(arg) {
                       argVals[i] = arg.valueNow_;
                       i = i + 1;
                     });
  var valueNow = this.f_.apply(null, argVals);
  this.valueNow_ = valueNow;
  this.sendsTo_.forEach(function(tuple) {
                          tuple.signal.consume(q, tuple.key, this_); 
                        });
};

/** 
 * @constructor
 * @extends F.Node
 * @private
 */
F.Filter = function(pred, src) {
  var this_ = this;
  src.connect({ key: 0, signal: this });
  this.pred_ = pred;
  var initValueNow = pred(src.valueNow_) ? src.valueNow_ : F.X;
  F.Node.call(this, initValueNow, 1 + src.rank_);
};
goog.inherits(F.Filter, F.Node);

F.Filter.prototype.consume = function(q, k, child) {
  if (!this.queuedNow_) {
    if (this.pred_(child.valueNow_)) {
      this.valueNow_ = child.valueNow_;
      this.queuedNow_ = true;
      q.enqueue(this.rank_, this);
    }
  }
};

/**
 * @constructor
 * @extends F.Node
 * @private
 */
F.Receiver = function(valueNow) {
  F.Node.call(this, valueNow, 0);
};
goog.inherits(F.Receiver, F.Node);

F.Receiver.prototype.send = function(v) {
  this.valueNow_ = v;
  var q = new goog.structs.PriorityQueue();
  q.enqueue(0, this);
  F.propagate_(q);
};

/**
 * @constructor
 * @extends F.Node
 * @private
 */
F.Untriggered = function(valueNow) {
  F.Node.call(this, valueNow, 0);
};
goog.inherits(F.Untriggered, F.Node);

F.Untriggered.prototype.consume = function(q, k, child) {
  // this is not pushed onto q
  this.valueNow_ = child.valueNow_;
};

F.Untriggered.prototype.produce = function(q) {
  throw 'Untriggered.prototype.produce must not be applied';
};

/**
 * @constructor
 * @extends F.Node
 * @private
 */
F.Merge = function(srcs) {
  var valueNow = F.util.find(function(n) { return n.valueNow_ !== F.X; }, 
                             srcs, { valueNow_ : F.X }).valueNow_;
  var rank = 1 + Math.max.apply(null, srcs);
  F.Node.call(this, valueNow, rank);

  var this_ = this;
  srcs.forEach(function(src) {
                 src.connect({ key: null, signal: this_ });
               });
};
goog.inherits(F.Merge, F.Node);

F.Merge.prototype.consume = function(q, k, child) {
  // Latest value from one of srcs
  if (this.queuedNow_) {
    return;
  }
  this.queuedNow_ = true;
  this.valueNow_ = child.valueNow_;
  q.enqueue(this.rank_, this);
};

/**
 * Creates constant signals from values.
 *
 * If v is a signal, returns v.
 * @returns {F.Node}
 */
F.sig = function(v) {
  if (v instanceof F.Node) {
    return v;
  }
  return F.constant(v);
};

/**
 * @param {number} n number of mutually-dependent signals to create
 * @param {function(Array.<F.Node>):Array.<F.Node>} f 
 *   consumes and produces n signals
 */
F.letrecN_ = function(n, f) {
  // TODO: Glitches? what if the outNodes have different ranks?
  var inNodes = F.util.iota(n).map(function(n) {
                                     return new F.Untriggered(F.X);
                                   });
  var outNodes = f.apply(null, inNodes);
  outNodes.forEach(function(outNode, i) {
                     outNode.connect({ key: 'src', signal: inNodes[i] });
                   });
  return outNodes;
};


// Construction helpers

F.letrec = function(f) {
  return F.letrecN_(f.length, f);
};

/**
 * Primitive signal transformer.
 *
 * This function is akin to monadic bind. It should not be necessary to use it
 * directly.
 *
 * @param {function(*):F.Node} k
 *   function that is applied to all signal values, which returns a new signal
 *   for each value
 * @returns {F.Node} signal carrying values from the last application of k
 */
F.Node.prototype.bind = function(k) {
  return new F.Bind(this, k);
};

F.Node.prototype.filter = function(pred) {
  return new F.Filter(pred, this);
};

/**
 * Given a signal carrying signals, produces the values of the current 
 * inner signal.
 *
 * @returns {F.Node}
 */
F.Node.prototype.flatten = function() {
  return new F.Bind(this, F.util.identity);
};

F.Node.prototype.get = function(propName) {
  return F.app(function(obj) { 
                 if (!obj.hasOwnProperty(propName)) {
                   return F.X;
                 }
                 return obj[propName]; 
               }, this);
};

F.Node.prototype.map = function(f) {
  return F.app(f, this);
};

/**
 * @returns {F.Node} carries the value of the current signal, but does not
 *                   trigger updates.
 */
F.Node.prototype.disableTrigger = function() {
  var node = new F.Untriggered(this.valueNow_);
  this.connect({ key: null, signal: node });
  return node;
};

F.receiver = function(v) {
  return new F.Receiver(v);
};

/**
 * A constant signal.
 *
 * @param {*} v any value
 * @returns {F.Node} a constant signal carrying v
 */
F.constant = function(v) {
  return new F.Node(v, 0);
};

F.merge = function(var_args) {
  return new F.Merge(Array.prototype.slice.call(arguments));
};

F.disjointMerge = function(var_args) {
  var args = Array.prototype.slice.call(arguments);
  function f(signal, i) {
    return F.app(function(v) { return { v: v, i: i }; }, signal);
  }
  return new F.Merge(args.map(f));
};

/**
 * @param {Function} f
 * @param {...F.Node} var_args
 * @returns {F.Node}
 */
F.app = function(f, var_args) {
  var args = Array.prototype.slice.call(arguments, 1);
  var valueNow = f.apply(null, args.map(function(v) { return v.valueNow_; }));
  return new F.App(valueNow, f, args);
};

F.appWithInit = function(valueNow, f, var_args) {
  var args = Array.prototype.slice.call(arguments, 2);
  return new F.App(valueNow, f, args);
};

/**
 * fold
 */
F.Node.prototype.fold = function(acc, f) {
  function g(v) {
    acc  = f(acc, v);
    return acc;
  }
  return F.appWithInit(acc, g, this);
};

F.fold2 = function(f, acc, x, y) {
  function g(x, y) {
    acc  = f(acc, x, y);
    return acc;
  }
  return F.appWithInit(acc, g, x, y);
};

/**
 * Send a signal every <code>interval</code> milliseconds.
 * 
 * If <code>interval</code> changes, the timer is reset.
 * 
 * @param {F.Node|number} interval
 * @returns {F.Node}
 */
F.interval = function(interval) {
  interval = F.sig(interval);

  var intervalID = null;
  var t = F.receiver(F.util.now());
  function callback() {
    t.send(F.util.now());
  }
  function f(interval) {
    if (intervalID !== null) {
      window.clearInterval(intervalID);
      intervalID = null;
    }
    if (typeof interval === 'number') {
      intervalID = window.setInterval(callback, interval);
    }
  }
  function g(t, _) {
    return t;
  }
  return F.app(g, t, F.app(f, interval).disableTrigger());
};

/**
 * Delay changes to this signal by <code>delay</code> milliseconds.
 *
 * If the delay changes while signals 
 * @param {F.Node|number} delay
 * @returns {F.Node}
 */
F.Node.prototype.delay = function(delay) {
  delay = F.sig(delay);

  /** @type Array<{val:*,time:number}> */
  var queue = [];
  
  var updDelta = F.receiver(F.X);

  function calcDelta(delay, updDelta) {
    if (updDelta === F.X) {
      return F.X;
    }
    else if (typeof updDelta === 'number') {
      if (updDelta < 0) {
        return delay;
      }
      else {
        return updDelta;
      }
    }
    else {
      debugger;
    }
  }
  var delta = F.app(calcDelta, delay.disableTrigger(), updDelta);
  
  function enqueue(v) {
    queue.push({ val: v, time: Date.now() });
    if (queue.length === 1) {
      updDelta.send(-1);
    }
  }
  this.map(enqueue);

  function dequeue() {
    if (queue.length === 0)
      debugger;
    var p = queue.shift();
    if (queue.length > 0) {
      updDelta.send(queue[0].time - p.time);
    }
    else {
      updDelta.send(F.X);
    }
    return p.val;
  }

  return F.interval(delta).map(dequeue);
};

F.Node.prototype.log = function(prefix) {
  return F.app(function(v) { 
                 console.log(prefix, v); 
                 return v; 
               }, this);
};

/*: ∀ α . α * Array<∃ β . {0: EventStream<β>, 1: α * β -> α}> -> Behavior<α> */
F.world = function(init, handlers) {
  return F.merge.apply(null,
                       handlers.map(function(handler)
                                    /*: ∃ β . {0: EventStream<β>, 1: α * β -> α} -> EventStream<α -> α> */
                                    {
                                      return handler[0].map(function(eventValue) /*: β -> (α -> α) */ {
                                                              return function(world) /*: α -> α */ {
                                                                return handler[1](world, eventValue);
                                                              };
                                                            });
                                    }))
    .fold(init, function(world, handler) /*: (α -> α) * α -> α */ {
            return handler(world);
          });
};




/**
 * @param {Object} obj
 * @param {string} prop
 * @returns {function(*)}
 * @private
 */
F.staticEnstyle_ = function(obj, prop) {
  return function(val) {
    if (val === F.X) {
      delete obj[prop];
    }
    else {
      obj[prop] = val;
    }
  };
};


/**
 * @private
 */
F.enstyle_ = function(target, obj) {
  Object.keys(obj).forEach(function(key) {
                             var val = obj[key];
                             if (val instanceof F.Node) {
                               F.app(F.staticEnstyle_(target, key), val);
                             }
                             else if (typeof val === 'object') {
                               F.enstyle_(target[key], val);
                             }
                             else {
                               target[key] = val;
                             }
                           });
};

/**
 * @param {Node} parent
 * @param {Node} newChild
 * @param {Node} refChild
 * @private
 */
F.insertAfter_ = function(parent, newChild, refChild) {
  if (typeof refChild != "undefined" && refChild.nextSibling) {
    parent.insertBefore(newChild, refChild.nextSibling);
  }
  else {
    // refChild == parent.lastChild
    parent.appendChild(newChild);
  }
};

/**
 * @param {Node} parent
 * @param {Array.<Node>} existingChildren
 * @param {Array.<Node>} newChildren
 * @private
 */
F.swapChildren_ = function(parent, existingChildren, newChildren) {
  var end = Math.min(existingChildren.length, newChildren.length);
  var i;

  for (i = 0; i < end; i++) {
    parent.replaceChild(newChildren[i], existingChildren[i]);
  }

  var lastInsertedChild = existingChildren[i - 1];

  if (end < existingChildren.length) {
    for (i = end; i < existingChildren.length; i++) {
      parent.removeChild(existingChildren[i]);
    }
  }
  else if (end < newChildren.length) {
    for (i = end; i < newChildren.length; i++) {
      F.insertAfter_(parent, newChildren[i], newChildren[i - 1]);
    }
  }
};

/**
 * @private
 */
F.appendDynChild_ = function(parent, child) {
  var lastVal = [];
  F.app(function(childV) {
          // TODO: flapjax.js msut have a bug when the time-varying array is empty
          if (childV === F.X || childV.length === 0) {
            childV = [document.createTextNode('')];
          }
          else if (!(childV instanceof Array)) {
            childV = [childV];
          }

          if (lastVal.length === 0) {
            childV.forEach(function(e) {
                             parent.appendChild(e);
                           });
          }
          else {
            F.swapChildren_(parent, lastVal, childV);
          }
          lastVal = childV;
        }, child);
};

/**
 * @private
 */
F.appendChild_ = function(parent, child) {
  if (child instanceof Array) {
    child.forEach(function(ch) { F.appendChild_(parent, ch); });
  }
  else if (child instanceof F.Node) {
    F.appendDynChild_(parent, child);
  }
  else {
    parent.appendChild(child);
  }
};

/**
 * An HTML element with attributes and children defined by signals.
 *
 * @param {string} tagName
 * @param {Object} attribs
 * @param {Array.<F.Node|Node>} children
 * @returns {HTMLElement}
 */
F.elt = function(tagName, attribs, var_args /* ... */) {
  var children = Array.prototype.slice.call(arguments, 2);
  var elt = document.createElement(tagName);
  F.enstyle_(elt, attribs);
  children.forEach(function(child) {
                     F.appendChild_(elt, child);
                   });
  return elt;
};

/**
 * @param {F.Node|string} text text to display
 * @returns {TextNode} a text node with time-varying content
 */
F.text = function(text) {
  if (!(text instanceof F.Node)) {
    return document.createTextNode(text);
  }

  var node = document.createTextNode('');
  F.app(function(t) { 
          if (t === F.X) {
            node.textContent = '';
          }
          else {
            node.textContent = t;
          }
        }, text);
  return node;
};

/**
 * A signal carrying DOM events, which triggers on each event.
 * 
 * The argument <code>elt</code> may be a behavior of DOM nodes or 
 * <code>false</code>.
 * 
 * @param {F.Node|Node|Window} elt the source of events or false
 * @param {string} eventName
 * @param {boolean=} useCapture
 * @returns {F.Node}
 */
F.evt = function(elt, eventName, useCapture) {
  elt = F.sig(elt);
  useCapture = typeof useCapture === 'undefined' ? false : useCapture;
  
  var sig = F.receiver(F.X);
  var callback = function(evt) {
    sig.send(evt);
  };
  var eltV = false;
  function f(newEltV) {
    if (eltV) {
      eltV.removeEventListener(eventName, callback, useCapture); 
    }
    eltV = newEltV;
    if (eltV !== false) {
      eltV.addEventListener(eventName, callback, useCapture);
    }
  }
  F.app(f, elt);
  return sig; // TODO: connection between sig and elt?
};

/**
 * a signal of DOM events, if this is a signal of DOM nodes
 *
 * @param {string} eventName
 * @param {boolean=} useCapture
 * @returns {F.Node}
 */
F.Node.prototype.evt = function(eventName, useCapture) {
  return F.evt(this, eventName, useCapture);
};
