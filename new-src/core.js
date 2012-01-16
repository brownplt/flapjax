"use strict";

goog.provide('F');
goog.require('goog.structs.PriorityQueue');

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
 * @param {goog.structs.PriorityQueue} q
 */
F.propagate_ = function(q) {
  var item;
  while (!q.isEmpty()) {
    item = q.dequeue();
    item.produce(q);
  }
};

F.X = { 
  toString: function() { 
    return 'F.X'; 
  }
};

F.nodes_ = { };

/**
 * @constructor
 */
F.Node = function(valueNow, rank) {
  this.sendsTo_ = [];
  this.queuedNow_ = false;
  this.valueNow_ = valueNow;
  this.rank_ = rank;
}

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
 * @extends {F.Node}
 * @param {F.Node} m
 * @param {function(*): F.Node} k
 */
F.nodes_.Bind = function(m, k) {
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
}
goog.inherits(F.nodes_.Bind, F.Node);

F.nodes_.Bind.prototype.consume = function(q, k, child) {
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
 * @extends {F.Node}
 */
F.nodes_.App = function(valueNow, f, args) {
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
goog.inherits(F.nodes_.App, F.Node);

F.nodes_.App.prototype.consume = function(q, k, child) {
  if (!this.queuedNow_) {
    this.queuedNow_ = true;
    q.enqueue(this.rank_, this);
  }
};

F.nodes_.App.prototype.produce = function(q) {
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
 * @extends {F.Node}
 */
F.nodes_.Receiver = function(valueNow) {
  F.Node.call(this, valueNow, 0);
}
goog.inherits(F.nodes_.Receiver, F.Node);

F.nodes_.Receiver.prototype.send = function(v) {
  this.valueNow_ = v;
  var q = new goog.structs.PriorityQueue();
  q.enqueue(0, this);
  F.propagate_(q);
}

/**
 * @constructor
 * @extends {F.Node}
 */
F.nodes_.Untriggered = function(valueNow) {
  F.Node.call(this, valueNow, 0);
}
goog.inherits(F.nodes_.Untriggered, F.Node);

F.nodes_.Untriggered.prototype.consume = function(q, k, child) {
  // this is not pushed onto q
  this.valueNow_ = child.valueNow_;
};

F.nodes_.Untriggered.prototype.produce = function(q) {
  throw 'Untriggered.prototype.produce must not be applied';
};

/**
 * @constructor
 * @extends {F.Node}
 */
F.nodes_.Merge = function(srcs) {
  var valueNow = F.util.find(function(n) { return n.valueNow_ !== F.X; }, 
    srcs, { valueNow_ : F.X }).valueNow_;
  var rank = 1 + Math.max.apply(null, srcs);
  F.Node.call(this, valueNow, rank);

  var this_ = this;
  srcs.forEach(function(src) {
    src.connect({ key: null, signal: this_ });
  });
};
goog.inherits(F.nodes_.Merge, F.Node);

F.nodes_.Merge.prototype.consume = function(q, k, child) {
  // Latest value from one of srcs
  if (this.queuedNow_) {
    return;
  }
  this.queuedNow_ = true;
  this.valueNow_ = child.valueNow_;
  q.enqueue(this.rank_, this);
};

/**
 * @param {number} n
 * @param {function(Array.<F.Node>):Array.<F.Node>} f
 */
F.letrecN_ = function(n, f) {
  // TODO: Glitches? what if the outNodes have different ranks?
  var inNodes = F.util.iota(n).map(function(n) {
    return new F.nodes_.Untriggered(F.X);
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

F.Node.prototype.bind = function(k) {
  return new F.nodes_.Bind(this, k);
};

F.Node.prototype.flatten = function() {
  return new F.nodes_.Bind(this, F.util.identity);
};

F.Node.prototype.disableTrigger = function() {
  var node = new F.nodes_.Untriggered(this.valueNow_);
  this.connect({ key: null, signal: node });
  return node;
};

F.receiver = function(v) {
  return new F.nodes_.Receiver(v);
};

F.constant = function(v) {
  return new F.Node(v, 0);
};

F.merge = function(var_args) {
  return new F.nodes_.Merge(Array.prototype.slice.call(arguments));
};

F.disjointMerge = function(var_args) {
  var args = Array.prototype.slice.call(arguments);
  function f(signal, i) {
    return F.app(function(v) { return { v: v, i: i }; }, signal);
  }
  return new F.nodes_.Merge(args.map(f));
};

/**
 * @param {Function} f
 * @param {...F.Node} var_args
 * @returns {F.Node}
 */
F.app = function(f, var_args) {
  var args = Array.prototype.slice.call(arguments, 1);
  var valueNow = f.apply(null, args.map(function(v) { return v.valueNow_; }));
  return new F.nodes_.App(valueNow, f, args);
};

F.appWithInit = function(valueNow, f, var_args) {
  var args = Array.prototype.slice.call(arguments, 2);
  return new F.nodes_.App(valueNow, f, args);
};

/**
 * @param {F.Node} interval
 * @returns {F.Node}
 */
F.interval = function(interval) {
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
 *
 */
F.Node.prototype.fold = function(acc, f) {
  function g(v) {
    acc  = f(v, acc);
    return acc;
  }
  return F.appWithInit(acc, g, this);
};

/**
 * @param {F.Node} delay
 * @returns {F.Node}
 */
/*
F.Node.prototype.delay = function(delay) {
  // Pairs each triggered value the delay it incurs.
  function f(val, delay) {
    return { val: val, delay: delay };
  }
  var vals = F.app(f, this, delay.disableTrigger());

  var es = F.letrec(function(queue, ticks) {
    // Ticks won't trigger until queue fires
    // A ticks trigger means a delayed event should now fire, queue can discard
    // the value
    var ticks_ =  F.interval(queue.map(function(v) { return v[0].delay; }));
    // valsTicks fires whenver either vals or ticks fires. 
    var valsTicks = F.disjointMerge(vals, ticks);
    // Queue will trigger whenever valsTicks fires. So, first trigger will
    // be by vals, since ticks is waiting for queue to trigger.
    var queue_ = valsTicks.fold([], function(vt, q) {
      if (vt.i === 0) {
        return q.enqueue(vt.v);
      }
      // assume v.t === 1
      else {
        // TODO: timer should only update when something is dequeued. It is easy
        // to also return a didDequeue flag in the accumulator and filter by
        // that value
        return q.dequeue();
      }
    });
  
    return [queue_, ticks_];
  });
  var queue = es[0];
  var ticks = es[1];
  
  // TODO: Relies on reading the last value of q, before q is dequeued on this
  // turn. Simplest solution is to add a last value field to the accumulation
  // variable q.
  return F.app(function(q, t) {
    return q[0].value;
  }, queue.disableTrigger(), ticks);

};
*/
