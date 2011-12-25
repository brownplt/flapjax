/*
 * Copyright (c) 2006-2009, The Flapjax Team.  All Rights Reserved.
 *  
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the name of the Brown University, the Flapjax Team, nor the names
 *   of its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
  
///////////////////////////////////////////////////////////////////////////////
// Miscellaneous functions

var F = F || { };

F.internal_ = { };
F.dom_ = { };
F.xhr_ = { };

// Sentinel value returned by updaters to stop propagation.
F.doNotPropagate = { };

/**
 * @return {Array}
 */
F.mkArray = function(arrayLike) {
  return Array.prototype.slice.call(arrayLike);
};

//////////////////////////////////////////////////////////////////////////////
// Flapjax core

/**
 * Stamp * Path * Obj
 * @constructor Pulse
 */
F.internal_.Pulse = function (stamp, value) {
  // Timestamps are used by liftB (and ifE).  Since liftB may receive multiple
  // update signals in the same run of the evaluator, it only propagates the 
  // signal if it has a new stamp.
  this.stamp = stamp;
  this.value = value;
};

/**
 * @constructor PQ
 */
F.internal_.PQ = function () {
  var ctx = this;
  ctx.val = [];
  this.insert = function (kv) {
    ctx.val.push(kv);
    var kvpos = ctx.val.length-1;
    while(kvpos > 0 && kv.k < ctx.val[Math.floor((kvpos-1)/2)].k) {
      var oldpos = kvpos;
      kvpos = Math.floor((kvpos-1)/2);
      ctx.val[oldpos] = ctx.val[kvpos];
      ctx.val[kvpos] = kv;
    }
  };
  this.isEmpty = function () { 
    return ctx.val.length === 0; 
  };
  this.pop = function () {
    if(ctx.val.length === 1) {
      return ctx.val.pop();
    }
    var ret = ctx.val.shift();
    ctx.val.unshift(ctx.val.pop());
    var kvpos = 0;
    var kv = ctx.val[0];
    while(1) { 
      var leftChild = (kvpos*2+1 < ctx.val.length ? ctx.val[kvpos*2+1].k : kv.k+1);
      var rightChild = (kvpos*2+2 < ctx.val.length ? ctx.val[kvpos*2+2].k : kv.k+1);
      if(leftChild > kv.k && rightChild > kv.k)
          break;

      if(leftChild < rightChild) {
        ctx.val[kvpos] = ctx.val[kvpos*2+1];
        ctx.val[kvpos*2+1] = kv;
        kvpos = kvpos*2+1;
      }
      else {
        ctx.val[kvpos] = ctx.val[kvpos*2+2];
        ctx.val[kvpos*2+2] = kv;
        kvpos = kvpos*2+2;
      }
    }
    return ret;
  };
};

F.internal_.lastRank = 0;
F.internal_.stamp = 1;
F.internal_.nextStamp = function () { return ++F.internal_.stamp; };

//propagatePulse: Pulse * Array Node -> 
//Send the pulse to each node 
F.internal_.propagatePulse = function (pulse, node) {
  var queue = new F.internal_.PQ(); //topological queue for current timestep

  queue.insert({k:node.rank,n:node,v:pulse});
  var len = 1;

  while (len) {
    var qv = queue.pop();
    len--;
    var nextPulse = qv.n.updater(new F.internal_.Pulse(qv.v.stamp, qv.v.value));

    if (nextPulse != F.doNotPropagate) {
      for (var i = 0; i < qv.n.sendsTo.length; i++) {
        len++;
	queue.insert({k:qv.n.sendsTo[i].rank,n:qv.n.sendsTo[i],v:nextPulse});
      }
    }
  }
};

/**
 * Event: Array Node b * ( (Pulse a -> Void) * Pulse b -> Void)
 * @constructor
 * @param {Array.<F.EventStream>} nodes
 */
F.EventStream = function (nodes,updater) {
  this.updater = updater;
  
  this.sendsTo = []; //forward link
  
  for (var i = 0; i < nodes.length; i++) {
    nodes[i].attachListener(this);
  }
  
  this.rank = ++F.internal_.lastRank;
};

/**
 * note: does not add flow as counting for rank nor updates parent ranks
 * @param {F.EventStream} dependent
 */
F.EventStream.prototype.attachListener = function(dependent) {
  if (!(dependent instanceof F.EventStream)) {
    throw 'attachListener: expected an F.EventStream';
  }
  this.sendsTo.push(dependent);
  
  if(this.rank > dependent.rank) {
    var lowest = F.internal_.lastRank+1;
    var q = [dependent];
    while(q.length) {
      var cur = q.splice(0,1)[0];
      cur.rank = ++F.internal_.lastRank;
      q = q.concat(cur.sendsTo);
    }
  }
};

//note: does not remove flow as counting for rank nor updates parent ranks
F.EventStream.prototype.removeListener = function (dependent) {
  if (!(dependent instanceof F.EventStream)) {
    throw 'removeListener: expected an F.EventStream';
  }

  var foundSending = false;
  for (var i = 0; i < this.sendsTo.length && !foundSending; i++) {
    if (this.sendsTo[i] === dependent) {
      this.sendsTo.splice(i, 1);
      foundSending = true;
    }
  }
  
  return foundSending;
};

/**
 *An internalE is a node that propagates all pulses it receives.  It's used
 * internally by various combinators.
 *
 * @param {Array.<F.EventStream>=} dependsOn
 */
F.internal_.internalE = function(dependsOn) {
  return new F.EventStream(dependsOn || [ ],function(pulse) { return pulse; });
};

/**
 * @return {F.EventStream}
 */
F.zeroE = function() {
  return new F.EventStream([],function(pulse) {
      throw ('zeroE : received a value; zeroE should not receive a value; the value was ' + pulse.value);
  });
};


/**
 * @param {*} val
 * @return {F.EventStream}
 */
F.oneE = function(val) {
  var sent = false;
  var evt = new F.EventStream([],function(pulse) {
    if (sent) {
      throw ('oneE : received an extra value');
    }
    sent = true;
    return pulse;
  });
  window.setTimeout(function() { F.sendEvent(evt,val); },0);
  return evt;
};


/**
 * @param {F.EventStream=} var_args
 * @return {F.EventStream}
 */
F.mergeE = function(var_args) {
  if (arguments.length === 0) {
    return F.zeroE();
  }
  else {
    var deps = F.mkArray(arguments);
    return F.internal_.internalE(deps);
  }
};

F.EventStream.prototype.mergeE = function() {
  var deps = F.mkArray(arguments);
  deps.push(this);
  return F.internal_.internalE(deps);
};

/**
 * @return {F.EventStream}
 */
F.EventStream.prototype.constantE = function(constantValue) {
  return new F.EventStream([this],function(pulse) {
    pulse.value = constantValue;
    return pulse;
  });
};

/**
 * @constructor
 * @param {F.EventStream} event
 * @param {*} init
 * @param {Function=} updater
 */
F.Behavior = function (event, init, updater) {
  if (!(event instanceof F.EventStream)) { 
    throw 'F.Behavior: expected event as second arg'; 
  }
  
  var behave = this;
  this.last = init;
  
  //sendEvent to this might impact other nodes that depend on this event
  //sendF.Behavior defaults to this one
  this.underlyingRaw = event;
  
  //unexposed, sendEvent to this will only impact dependents of this behaviour
  this.underlying = new F.EventStream([event], updater 
    ? function (p) {
        behave.last = updater(p.value); 
        p.value = behave.last; return p;
      } 
    : function (p) {
        behave.last = p.value;
        return p;
      });
};

F.receiverE = function() {
  var evt = F.internal_.internalE();
  evt.sendEvent = function(value) {
    F.internal_.propagatePulse(new F.internal_.Pulse(F.internal_.nextStamp(), value),evt);
  };
  return evt;
};

//note that this creates a new timestamp and new event queue
F.sendEvent = function (node, value) {
  if (!(node instanceof F.EventStream)) { throw 'sendEvent: expected Event as first arg'; } //SAFETY
  
  F.internal_.propagatePulse(new F.internal_.Pulse(F.internal_.nextStamp(), value),node);
};

// bindE :: F.EventStream a * (a -> F.EventStream b) -> F.EventStream b
F.EventStream.prototype.bindE = function(k) {
  /* m.sendsTo resultE
   * resultE.sendsTo prevE
   * prevE.sendsTo returnE
   */
  var m = this;
  var prevE = false;
  
  var outE = new F.EventStream([],function(pulse) { return pulse; });
  outE.name = "bind outE";
  
  var inE = new F.EventStream([m], function (pulse) {
    if (prevE) {
      prevE.removeListener(outE, true);
      
    }
    prevE = k(pulse.value);
    if (prevE instanceof F.EventStream) {
      prevE.attachListener(outE);
    }
    else {
      throw "bindE : expected F.EventStream";
    }

    return F.doNotPropagate;
  });
  inE.name = "bind inE";
  
  return outE;
};

/**
 * @param {function(*):*} f
 * @return {F.EventStream}
 */
F.EventStream.prototype.mapE = function(f) {
  if (!(f instanceof Function)) {
    throw ('mapE : expected a function as the first argument; received ' + f);
  };
  
  return new F.EventStream([this],function(pulse) {
    pulse.value = f(pulse.value);
    return pulse;
  });
};

/**
 * @return {F.EventStream}
 */
F.EventStream.prototype.notE = function() { 
	return this.mapE(function(v) { 
		return !v; 
	}); 
};

/**
 * @param {function(*):boolean} pred
 * @return {F.EventStream}
 */
F.EventStream.prototype.filterE = function(pred) {
  if (!(pred instanceof Function)) {
    throw ('filterE : expected predicate; received ' + pred);
  };
  
  // Can be a bindE
  return new F.EventStream([this], function(pulse) {
    return pred(pulse.value) ? pulse : F.doNotPropagate;
  });
};

/**
 * @return {F.EventStream}
 */
F.EventStream.prototype.onceE = function() {
  var done = false;
  return new F.EventStream([this],function(pulse) {
    if (!done) { done = true; return pulse; }
    else { return F.doNotPropagate; }
  });
};

/**
 * @return {F.EventStream}
 */
F.EventStream.prototype.skipFirstE = function() {
  var skipped = false;
  return new F.EventStream([this],function(pulse) {
    if (skipped)
      { return pulse; }
    else
      { return F.doNotPropagate; }
  });
};

/**
 * @param {*} init
 * @param {Function} fold
 * @return {F.EventStream}
 */
F.EventStream.prototype.collectE = function(init,fold) {
  var acc = init;
  return this.mapE(
    function (n) {
      var next = fold(n, acc);
      acc = next;
      return next;
    });
};

/**
 * @return {F.EventStream}
 */
F.EventStream.prototype.switchE = function() {
  return this.bindE(function(v) { return v; });
};

F.recE = function(fn) {
  var inE = F.receiverE(); 
  var outE = fn(inE); 
  outE.mapE(function(x) { 
    inE.sendEvent(x); }); 
  return outE; 
};

F.internal_.delayStaticE = function (event, time) {
  
  var resE = F.internal_.internalE();
  
  new F.EventStream([event], function (p) { 
    setTimeout(function () { F.sendEvent(resE, p.value);},  time ); 
    return F.doNotPropagate;
  });
  
  return resE;
};

/**
 * @param {F.Behavior|number} time
 * @return {F.EventStream}
 */
F.EventStream.prototype.delayE = function (time) {
  var event = this;
  
  if (time instanceof F.Behavior) {
    
    var receiverEE = F.internal_.internalE();
    var link = 
    {
      from: event, 
      towards: F.internal_.delayStaticE(event, time.valueNow())
    };
    
    //TODO: Change semantics such that we are always guaranteed to get an event going out?
    var switcherE = 
    new F.EventStream(
      [time.changes()],
      function (p) {
        link.from.removeListener(link.towards); 
        link =
        {
          from: event, 
          towards: F.internal_.delayStaticE(event, p.value)
        };
        F.sendEvent(receiverEE, link.towards);
        return F.doNotPropagate;
      });
    
    var resE = receiverEE.switchE();
    
    F.sendEvent(switcherE, time.valueNow());
    return resE;
    
      } else { return F.internal_.delayStaticE(event, time); }
};

//mapE: ([Event] (. Array a -> b)) . Array [Event] a -> [Event] b
F.mapE = function (fn /*, [node0 | val0], ...*/) {
  //      if (!(fn instanceof Function)) { throw 'mapE: expected fn as second arg'; } //SAFETY
  
  var valsOrNodes = F.mkArray(arguments);
  //selectors[i]() returns either the node or real val, optimize real vals
  var selectors = [];
  var selectI = 0;
  var nodes = [];
  for (var i = 0; i < valsOrNodes.length; i++) {
    if (valsOrNodes[i] instanceof F.EventStream) {
      nodes.push(valsOrNodes[i]);
      selectors.push( 
        (function(ii) {
            return function(realArgs) { 
              return realArgs[ii];
            };
        })(selectI));
      selectI++;
    } else {
      selectors.push( 
        (function(aa) { 
            return function () {
              return aa;
            }; 
        })(valsOrNodes[i]));
    } 
  }
  
  var context = this;
  var nofnodes = selectors.slice(1);
  
  if (nodes.length === 0) {
    return F.oneE(fn.apply(context, valsOrNodes));
  } else if ((nodes.length === 1) && (fn instanceof Function)) {
    return nodes[0].mapE(
      function () {
        var args = arguments;
        return fn.apply(
          context, 
          nofnodes.map(function (s) {return s(args);}));
      });
  } else if (nodes.length === 1) {
    return fn.mapE(
      function (v) {
        var args = arguments;
        return v.apply(
          context, 
          nofnodes.map(function (s) {return s(args);}));
      });                
  }
  else {
	throw 'unknown mapE case';
	}
};

/** 
 * @param {F.Behavior} valueB
 * @return {F.EventStream}
 */
F.EventStream.prototype.snapshotE = function (valueB) {
  return new F.EventStream([this], function (pulse) {
    pulse.value = valueB.valueNow();
    return pulse;
  });
};

/**
 * @param {*=} optStart initial value (optional)
 * @return {F.EventStream}
 */
F.EventStream.prototype.filterRepeatsE = function(optStart) {
  var hadFirst = optStart === undefined ? false : true;
  var prev = optStart;

  return this.filterE(function (v) {
    if (!hadFirst || prev !== v) {
      hadFirst = true;
      prev = v;
      return true;
    }
    else {
      return false;
    }
  });
};

/**
 * @param {(number|F.Behavior)} time
 * @return {F.EventStream}
 */
F.EventStream.prototype.calmE = function(time) {
  if (!(time instanceof F.Behavior)) {
		time = F.constantB(time);
	}

  var out = F.internal_.internalE();
  new F.EventStream(
    [this],
    function() {
      var towards = null;
      return function (p) {
        if (towards !== null) { clearTimeout(towards); }
        towards = setTimeout( function () { 
            towards = null;
            F.sendEvent(out,p.value); }, time.valueNow());
        return F.doNotPropagate;
      };
    }());
  return out;
};

F.EventStream.prototype.blindE = function (time) {
  return new F.EventStream(
    [this],
    function () {
      var intervalFn = 
      time instanceof F.Behavior?
      function () { return time.valueNow(); }
      : function () { return time; };
      var lastSent = (new Date()).getTime() - intervalFn() - 1;
      return function (p) {
        var curTime = (new Date()).getTime();
        if (curTime - lastSent > intervalFn()) {
          lastSent = curTime;
          return p;
        }
        else { return F.doNotPropagate; }
      };
    }());
};

/**
 * @param {*} init
 * @return {F.Behavior}
 */
F.EventStream.prototype.startsWith = function(init) {
  return new F.Behavior(this,init);
};

/**
 * Returns the presently stored value.
 */
F.Behavior.prototype.valueNow = function() {
  return this.last;
};

/**
 * @return {F.EventStream}
 */
F.Behavior.prototype.changes = function() {
  return this.underlying;
};

/**
 * @return {F.Behavior}
 */
F.Behavior.prototype.switchB = function() {
  var behaviourCreatorsB = this;
  var init = behaviourCreatorsB.valueNow();
  
  var prevSourceE = null;
  
  var receiverE = F.internal_.internalE();
  
  //XXX could result in out-of-order propagation! Fix!
  var makerE = 
  new F.EventStream(
    [behaviourCreatorsB.changes()],
    function (p) {
      if (!(p.value instanceof F.Behavior)) { throw 'switchB: expected F.Behavior as value of F.Behavior of first argument'; } //SAFETY
      if (prevSourceE != null) {
        prevSourceE.removeListener(receiverE);
      }
      
      prevSourceE = p.value.changes();
      prevSourceE.attachListener(receiverE);
      
      F.sendEvent(receiverE, p.value.valueNow());
      return F.doNotPropagate;
    });
  
  if (init instanceof F.Behavior) {
    F.sendEvent(makerE, init);
  }
  
  return receiverE.startsWith(init instanceof F.Behavior? init.valueNow() : init);
};

/**
 * @param {F.Behavior|number} interval
 * @return {F.Behavior}
 */
F.timerB = function(interval) {
  return F.timerE(interval).startsWith((new Date()).getTime());
};

//TODO test, signature
F.Behavior.prototype.delayB = function (time, init) {
  var triggerB = this;
  if (!(time instanceof F.Behavior)) {
		time = F.constantB(time);
	}
  return triggerB.changes()
		       .delayE(time)
		       .startsWith(arguments.length > 3 ? init : triggerB.valueNow());
};

//artificially send a pulse to underlying event node of a behaviour
//note: in use, might want to use a receiver node as a proxy or an identity map
F.Behavior.prototype.sendBehavior = function(val) {
  F.sendEvent(this.underlyingRaw,val);
};

F.Behavior.prototype.ifB = function(trueB,falseB) {
  var testB = this;
  //TODO auto conversion for behaviour funcs
  if (!(trueB instanceof F.Behavior)) { trueB = F.constantB(trueB); }
  if (!(falseB instanceof F.Behavior)) { falseB = F.constantB(falseB); }
  return F.liftB(function(te,t,f) { return te ? t : f; },testB,trueB,falseB);
};


/** 
 * condB: . [F.Behavior boolean, F.Behavior a] -> F.Behavior a
 *
 * @param {Array.<Array.<F.Behavior>>} var_args
 * @return {F.Behavior}
 */
F.condB = function (var_args ) {
  var pairs = F.mkArray(arguments);
return F.liftB.apply({},[function() {
    for(var i=0;i<pairs.length;i++) {
      if(arguments[i]) return arguments[pairs.length+i];
    }
    return undefined;
  }].concat(pairs.map(function(pair) {return pair[0];})
            .concat(pairs.map(function(pair) {return pair[1];}))));
};

/**
 * @param {*} val
 * @return {F.Behavior}
 */
F.constantB = function (val) {
  return new F.Behavior(F.internal_.internalE(), val);
};

/**
 * @param {Function|F.Behavior} fn
 * @param {...F.Behavior} var_args
 * @return F.Behavior
 */
F.liftB = function (fn, var_args) {

  var args = Array.prototype.slice.call(arguments, 1);
  
  //dependencies
  var constituentsE =
    F.mkArray(arguments)
    .filter(function (v) { return v instanceof F.Behavior; })
    .map(function (b) { return b.changes(); });
  
  //calculate new vals
  var getCur = function (v) {
    return v instanceof F.Behavior ? v.last : v;
  };
  
  var ctx = this;
  var getRes = function () {
    return getCur(fn).apply(ctx, args.map(getCur));
  };

  if(constituentsE.length === 1) {
    return new F.Behavior(constituentsE[0],getRes(),getRes);
  }
    
  //gen/send vals @ appropriate time
  var prevStamp = -1;
  var mid = new F.EventStream(constituentsE, function (p) {
    if (p.stamp != prevStamp) {
      prevStamp = p.stamp;
      return p; 
    }
    else {
      return F.doNotPropagate;
    }
  });
  
  return new F.Behavior(mid,getRes(),getRes);
};

/**
 * @param {...F.Behavior} var_args
 */
F.Behavior.prototype.ap = function(var_args) {
	var args = [this].concat(var_args);
	return F.liftB.apply(null, var_args);
}

/**
 * @param {F.Behavior|Function} fn
 * @return F.Behavior
 */
F.Behavior.prototype.liftB = function(fn) {
	return F.liftB(fn, this);
};

/**
 * @param {...F.Behavior} var_args
 */
F.Behavior.andB = function (var_args) {
return F.liftB.apply({},[function() {
    for(var i=0; i<arguments.length; i++) {if(!arguments[i]) return false;}
    return true;
}].concat(F.mkArray(arguments)));
};

/**
 * @param {...F.Behavior} var_args
 */
F.Behavior.orB = function (var_args) {
  return F.liftB.apply({},[function() {
      for(var i=0; i<arguments.length; i++) {if(arguments[i]) return true;}
      return false;
  }].concat(F.mkArray(arguments)));
};

/**
 * @return {F.Behavior}
 */
F.Behavior.prototype.notB = function() {
  return this.liftB(function(v) { return !v; });
};

F.Behavior.prototype.blindB = function (intervalB) {
  return this.changes().blindE(intervalB).startsWith(this.valueNow());
};

F.Behavior.prototype.calmB = function (intervalB) {
  return this.changes().calmE(intervalB).startsWith(this.valueNow());
};

//////////////////////////////////////////////////////////////////////////////////////
// DOM Utilities

/**
 * assumes IDs already preserved
 *
 * @param {Node|string} replaceMe
 * @param {Node|string} withMe
 * @return {Node}
 */
F.dom_.swapDom = function(replaceMe, withMe) {
  if ((replaceMe === null) || (replaceMe === undefined)) { throw ('swapDom: expected dom node or id, received: ' + replaceMe); } //SAFETY
  
  var replaceMeD = F.dom_.getObj(replaceMe);
  if (!(replaceMeD.nodeType > 0)) { throw ('swapDom expected a Dom node as first arg, received ' + replaceMeD); } //SAFETY
  
  if (withMe) {
    var withMeD = F.dom_.getObj(withMe);
    if (!(withMeD.nodeType > 0)) { throw 'swapDom: can only swap with a DOM object'; } //SAFETY
    try {
      if (replaceMeD.parentNode === null) { return withMeD; }
      if(withMeD != replaceMeD) replaceMeD.parentNode.replaceChild(withMeD, replaceMeD);
    } catch (e) {
      throw('swapDom error in replace call: withMeD: ' + withMeD + ', replaceMe Parent: ' + replaceMeD + ', ' + e + ', parent: ' + replaceMeD.parentNode);                    
    }
  } else {
    replaceMeD.parentNode.removeChild(replaceMeD); //TODO isolate child and set innerHTML to "" to avoid psuedo-leaks?
  }
  return replaceMeD;
};

//getObj: String U Dom -> Dom
//throws 
//  'getObj: expects a Dom obj or Dom id as first arg'
//  'getObj: flapjax: cannot access object'
//  'getObj: no obj to get
//also known as '$'
F.dom_.getObj = function (name) {
  if (typeof(name) === 'object') { return name; }
  else if ((typeof(name) === 'null') || (typeof(name) === 'undefined')) {
    throw 'getObj: expects a Dom obj or Dom id as first arg';
  } else {
    
    var res = 
    document.getElementById ? document.getElementById(name) :
    document.all ? document.all[name] :
    document.layers ? document.layers[name] :
    (function(){ throw 'getObj: flapjax: cannot access object';})();
    if ((res === null) || (res === undefined)) { 
      throw ('getObj: no obj to get: ' + name); 
    }
    return res;
  }
};

// TODO: should be richer
F.$ = F.dom_.getObj;

/**
 * helper to reduce obj look ups
 * getDynObj: domNode . Array (id) -> domObj
 * obj * [] ->  obj
 * obj * ['position'] ->  obj
 * obj * ['style', 'color'] ->  obj.style
 *
 * @param {Node|string} domObj
 * @param {Array.<string>} indices
 * @return {Object}
 */
F.dom_.getMostDom = function (domObj, indices) {
  var acc = F.dom_.getObj(domObj);
  if ( (indices === null) || (indices === undefined) || (indices.length < 1)) {
    return acc;
  } else {
    for (var i = 0; i < indices.length - 1; i++) {
      acc = acc[indices[i]];
    }
    return acc;
  }       
};

F.dom_.getDomVal = function (domObj, indices) {
  var val = F.dom_.getMostDom(domObj, indices);
  if (indices && indices.length > 0) {
    val = val[indices[indices.length - 1]];
  }
  return val;
};

/**
 * @param {F.Behavior|number} intervalB
 * @return {F.EventStream}
 */
F.timerE = function(intervalB) {
  if (!(intervalB instanceof F.Behavior)) {
		intervalB = F.constantB(intervalB);
  }
  var eventStream = F.receiverE();
  var callback = function() {
    eventStream.sendEvent((new Date()).getTime());
  };
  var timerID = null;
  intervalB.liftB(function(interval) {
    if (timerID) {
      clearInterval(timerID);
      timerID = null;
    }
    if (typeof interval === 'number' && interval > 0) {
      timerID =  setInterval(callback, interval);
    }
  });
  return eventStream;
};


// Applies f to each element of a nested array.
F.dom_.deepEach = function(arr, f) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] instanceof Array) {
      F.dom_.deepEach(arr[i], f);
    }
    else {
      f(arr[i]);
    }
  }
};


F.dom_.mapWithKeys = function(obj, f) {
  for (var ix in obj) {
    if (!(Object.prototype && Object.prototype[ix] === obj[ix])) {
      f(ix, obj[ix]);
    }
  }
};


/**
 * @param {Node} parent
 * @param {Node} newChild
 * @param {Node} refChild
 */
F.dom_.insertAfter = function(parent, newChild, refChild) {
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
 */
F.dom_.swapChildren = function(parent, existingChildren, newChildren) {
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
      F.dom_.insertAfter(parent, newChildren[i], newChildren[i - 1]);
    }
  }
};

/**
 * not a word
 *
 * @param {*} maybeElement
 * @return {Node}
 *
 * @suppress {checkTypes} the nodeType check does not get by the typechecker
 */
F.dom_.elementize = function(maybeElement) {
  return (maybeElement.nodeType > 0) 
           ? maybeElement
           : document.createTextNode(maybeElement.toString()); // TODO: toString!!
};


/**
 * @param {Object} obj
 * @param {string} prop
 * @param {*} val
 */
F.dom_.staticEnstyle = function(obj, prop, val) {
  if (val instanceof Object) {
    // TODO: enstyle is missing? I think this should be staticEnstyle.
    // mapWithKeys(val, function(k, v) { enstyle(obj[prop], k, v); });
  }
  else {
    obj[prop] = val;
  }
};


/**
 * @param {Object} obj
 * @param {string} prop
 * @param {F.Behavior|*} val
 */
F.dom_.dynamicEnstyle = function(obj, prop, val) {
  if (val instanceof F.Behavior) {
    // TODO: redundant? liftB will call anyway ...
    F.dom_.staticEnstyle(obj, prop, val.valueNow()); 
    val.liftB(function(v) {
      F.dom_.staticEnstyle(obj, prop, v);
    });
  }
  else if (val instanceof Object) {
    F.dom_.mapWithKeys(val, function(k, v) {
      F.dom_.dynamicEnstyle(obj[prop], k, v);
    });
  }
  else {
    obj[prop] = val;
  }
};
  

/**
 * @param {string} tagName
 * @return {function((string|Object|Node)=, ...[(string|Node|Array.<Node>)]):Node}
 */
F.dom_.makeTagB = function(tagName) { return function() {
  var attribs, children;

  if (typeof(arguments[0]) === "object" && 
      !(arguments[0].nodeType > 0 || arguments[0] instanceof F.Behavior || 
        arguments[0] instanceof Array)) {
    attribs = arguments[0];
    children = Array.prototype.slice.call(arguments, 1);
  }
  else {
    attribs = { };
    children = F.mkArray(arguments);
  }
 
  var elt = document.createElement(tagName);

  F.dom_.mapWithKeys(attribs, function(name, val) {
    if (val instanceof F.Behavior) {
      elt[name] = val.valueNow();
      val.liftB(function(v) { 
        F.dom_.staticEnstyle(elt, name, v); });
    }
    else {
      F.dom_.dynamicEnstyle(elt, name, val);
    }
  });

  F.dom_.deepEach(children, function(child) {
    if (child instanceof F.Behavior) {
      var lastVal = child.valueNow();
      if (lastVal instanceof Array) {
        lastVal = lastVal.map(F.dom_.elementize);
        lastVal.forEach(function(dynChild) { elt.appendChild(dynChild); });
        child.liftB(function(currentVal) {
          currentVal = currentVal.map(F.dom_.elementize);
          F.dom_.swapChildren(elt, lastVal, currentVal);
          lastVal = currentVal;
        });
      }
      else {
        lastVal = F.dom_.elementize(lastVal);
        elt.appendChild(lastVal);
        var lastValIx = elt.childNodes.length - 1; 
        child.liftB(function(currentVal) {
          currentVal = F.dom_.elementize(currentVal);
          if (lastVal.parentNode != elt) {
            elt.appendChild(currentVal); }
          else {
            elt.replaceChild(currentVal, lastVal); }
          lastVal = currentVal;
        });
      }
    }
    else {
      elt.appendChild(F.dom_.elementize(child));
    }
  });

  return elt;
}; };


[ "a", "b", "blockquote", "br", "button", "canvas", "div", "fieldset", 
"form", "font", "h1", "h2", "h3", "h4", "hr", "img", "iframe", "input", 
"label", "legend", "li", "ol", "optgroup", "option", 
"p", "pre", "select", "span", "strong", "table", "tbody", 
"td", "textarea", "tfoot", "th", "thead", "tr", "tt", "ul" ].forEach(function (name) {
  window[name.toUpperCase()] = F.dom_.makeTagB(name);
});


var DIV = F.dom_.makeTagB('div');
var SPAN = F.dom_.makeTagB('span');
var A = F.dom_.makeTagB('a');
var TEXTAREA = F.dom_.makeTagB('TEXTAREA');
var OPTION = F.dom_.makeTagB('OPTION');
var INPUT = F.dom_.makeTagB('INPUT');
var SELECT = F.dom_.makeTagB('SELECT');

//TEXTB: F.Behavior a -> F.Behavior Dom TextNode    
var TEXTB = function (strB) {
  //      if (!(strB instanceof F.Behavior || typeof(strB) == 'string')) { throw 'TEXTB: expected F.Behavior as second arg'; } //SAFETY
  if (!(strB instanceof F.Behavior)) { strB = F.constantB(strB); }
  
  return strB.changes().mapE(
      function (txt) { return document.createTextNode(txt); })
	  .startsWith(document.createTextNode(strB.valueNow()));
};

var TEXT = function (str) {
  return document.createTextNode(str);
};

///////////////////////////////////////////////////////////////////////////////
// Reactive DOM

//tagRec: Array (EventName a) * 
//      ( .Array (Event a) * Array (Event a) -> F.Behavior Dom) -> F.Behavior Dom

// tagRec :: [EventName] * (F.EventStream DOMEvent, ... -> Element) -> Element
F.tagRec = function (eventNames, maker) {
  if (!(eventNames instanceof Array)) { throw 'tagRec: expected array of event names as first arg'; } //SAFETY
  if (!(maker instanceof Function)) { throw 'tagRec: expected function as second arg'; } //SAFETY
  
  var numEvents = eventNames.length;

  var receivers = [ ];
  var i;
  for (i = 0; i < numEvents; i++) {
    receivers.push(F.internal_.internalE());
  }

  var elt = maker.apply(this, receivers);

  for (i = 0; i < numEvents; i++) {
    F.extractEventE(elt, eventNames[i]).attachListener(receivers[i]);
  }

  return elt;
};

F.dom_.extractEventDynamicE = function(eltB, eventName) {
  var eventStream = F.receiverE();
  var callback = function(evt) {
    eventStream.sendEvent(evt); 
  };
  var currentElt = false;
  eltB.liftB(function(elt) {
    if (currentElt) {
      currentElt.removeEventListener(eventName, callback); 
    }
    currentElt = elt;
    if (elt && elt.addEventListener && elt.removeEventListener) {
      elt.addEventListener(eventName, callback);
    }
  });
  return eventStream;
};

F.dom_.extractEventStaticE = function(elt, eventName) {
  var eventStream = F.receiverE();
  var callback = function(evt) {
    eventStream.sendEvent(evt); 
  };
  elt.addEventListener(eventName, callback);
  return eventStream;
};

/**
 * @param {F.Behavior|Node} elt
 * @param {string} eventName
 * @return {F.EventStream}
 */
F.extractEventE = function(elt, eventName) {
  if (elt instanceof F.Behavior) {
    return F.dom_.extractEventDynamicE(elt, eventName);
  }
  else {
    return F.dom_.extractEventStaticE(elt, eventName);
  }
};

F.$E = F.extractEventE;

/**
 * @param {F.Behavior} domObj
 * @param {...string} var_args
 * @return {F.EventStream}
 */
F.extractEventsE = function (domObj, var_args) {
  var eventNames = Array.prototype.slice.call(arguments, 1);
  
  var events = (eventNames.length === 0 ? [] : eventNames)
    .map(function (eventName) {
           return F.extractEventE(domObj, eventName); 
         });
  
  return F.mergeE.apply(null, events);
};

//value of dom form object during trigger
F.extractValueOnEventE = function (triggerE, domObj) {
  if (!(triggerE instanceof F.EventStream)) { throw 'extractValueOnEventE: expected Event as first arg'; } //SAFETY
  
  return F.extractValueOnEventB.apply(this, arguments).changes();
  
};

/**extractDomFieldOnEventE: Event * Dom U String . Array String -> Event a
 *
 * @param {F.EventStream} triggerE
 * @param {Node} domObj
 * @param {...*} var_args
 */
F.extractDomFieldOnEventE = function (triggerE, domObj, var_args) {
  if (!(triggerE instanceof F.EventStream)) { throw 'extractDomFieldOnEventE: expected Event as first arg'; } //SAFETY
  var indices = Array.prototype.slice.call(arguments, 2);
  var res =
  triggerE.mapE(
    function () { return F.dom_.getDomVal(domObj, indices); });
  return res;
};

F.extractValueE = function (domObj) {
  return F.extractValueB.apply(this, arguments).changes();
};

//extractValueOnEventB: Event * DOM -> F.Behavior
// value of a dom form object, polled during trigger
F.extractValueOnEventB = function (triggerE, domObj) {
  return F.dom_.extractValueStaticB(domObj, triggerE);
};

/**
 * If no trigger for extraction is specified, guess one
 *
 * @param {Node} domObj
 * @param {F.EventStream=} triggerE
 * @return {F.Behavior}
 */
F.dom_.extractValueStaticB = function (domObj, triggerE) {
  
  var objD;
  try {
    objD = F.dom_.getObj(domObj);
    //This is for IE
    if(typeof(domObj) === 'string' && objD.id != domObj) {
      throw 'Make a radio group';
    }
  } catch (e) {
    objD = {type: 'radio-group', name: domObj};
  }
  
  var getter; // get value at any current point in time
  
  var result;

  switch (objD.type)  {
    //TODO: checkbox.value instead of status?
  case 'checkbox': 
    result = F.extractDomFieldOnEventE(
          triggerE ? triggerE : 
          F.extractEventsE(
            objD, 
            'click', 'keyup', 'change'),
          objD,
          'checked').filterRepeatsE(objD.checked).startsWith(objD.checked);
    break; 
  case 'select-one':
      getter = function () {                         
        return objD.selectedIndex > -1 ? 
        (objD.options[objD.selectedIndex].value ?
          objD.options[objD.selectedIndex].value :
          objD.options[objD.selectedIndex].innerText)
        : undefined;
      };
      result = (triggerE ? triggerE :
            F.extractEventsE(
              objD,
              'click', 'keyup', 'change')).mapE(getter).filterRepeatsE().startsWith(getter());
      break;
  case 'select-multiple':
    //TODO ryan's cfilter adapted for equality check
    getter = function () {
      var res = [];
      for (var i = 0; i < objD.options.length; i++) {
        if (objD.options[i].selected) {
          res.push(objD.options[i].value ? objD.options[i].value : objD.options[i].innerText);
        }
      }
      return res;
    };
    result = 
        (triggerE ? triggerE : 
        F.extractEventsE(
          objD,
          'click', 'keyup', 'change')).mapE(getter).startsWith(getter());
    break;
    
  case 'text':
  case 'textarea':
  case 'hidden':
  case 'password':
    result = F.extractDomFieldOnEventE(
          triggerE ? triggerE :
          F.extractEventsE(
            objD, 
            'click', 'keyup', 'change'),
          objD,
          'value').filterRepeatsE(objD.value).startsWith(objD.value);
    break;
    
  case 'button': //same as above, but don't filter repeats
    result = F.extractDomFieldOnEventE(
        triggerE ? triggerE :
        F.extractEventsE(
          objD, 
          'click', 'keyup', 'change'),
        objD,
        'value').startsWith(objD.value);
    break;
    
  case 'radio': 
  case 'radio-group':
    
    //TODO returns value of selected button, but if none specified,
    //      returns 'on', which is ambiguous. could return index,
    //      but that is probably more annoying
    
    var radiosAD = 
      F.mkArray(document.getElementsByTagName('input'))
      .filter(
      function (elt) { 
        return (elt.type === 'radio') &&
        (elt.getAttribute('name') === objD.name); 
      });
    
    getter = 
    objD.type === 'radio' ?
    
    function () {
      return objD.checked;
    } :
    
    function () {
      for (var i = 0; i < radiosAD.length; i++) {
        if (radiosAD[i].checked) {
          return radiosAD[i].value; 
        }
      }
      return undefined; //TODO throw exn? 
    };
    
    var actualTriggerE = triggerE ? triggerE :
    F.mergeE.apply(
      this,
      radiosAD.map(
        function (radio) { 
          return F.extractEventsE(
            radio, 
        'click', 'keyup', 'change'); }));
    
    result =
			actualTriggerE.mapE(getter).filterRepeatsE(getter()).startsWith(getter());
    break;
  default:
    throw ('extractValueStaticB: unknown value type "' + objD.type + '"');
  }

  return result;
};

F.extractValueB = function (domObj) {
  if (domObj instanceof F.Behavior) {
    return domObj.liftB(function (dom) { return F.dom_.extractValueStaticB(dom); })
                 .switchB();
  } else {
    return F.dom_.extractValueStaticB(domObj);
  }
};

F.$B = F.extractValueB;


//into[index] = deepValueNow(from) via descending from object and mutating each field
F.dom_.deepStaticUpdate = function (into, from, index) {
  var fV = (from instanceof F.Behavior)? from.valueNow() : from;
  if (typeof(fV) === 'object') {
    for (var i in fV) {
      if (!(Object.prototype) || !(Object.prototype[i])) {
        F.dom_.deepStaticUpdate(index? into[index] : into, fV[i], i);
      }
    }
  } else {
    var old = into[index];
    into[index] = fV;
  }
};

//note: no object may be time varying, just the fields
//into[index] = from
//only updates on changes
F.dom_.deepDynamicUpdate = function (into, from, index) {
  var fV = (from instanceof F.Behavior)? from.valueNow() : from;
  if (typeof(fV) === 'object') {
    if (from instanceof F.Behavior) {
      throw 'deepDynamicUpdate: dynamic collections not supported';
    }
    for (var i in fV) {
      if (!(Object.prototype) || !(Object.prototype[i])) {
        F.dom_.deepDynamicUpdate(index? into[index] : into, fV[i], i);
      }
    }
  } else {
    if (from instanceof F.Behavior) {
      new F.EventStream(
        [from.changes()],
        function (p) {
          if (index) { 
            var old = into[index];
            into[index] = p.value;
          }
          else { into = p.value; } //TODO notify topE?
          return F.doNotPropagate;
        });
    }
  }
};


F.insertValue = function (val, domObj /* . indices */) {
  var indices = Array.prototype.slice.call(arguments, 2);
  var parent = F.dom_.getMostDom(domObj, indices);
  F.dom_.deepStaticUpdate(parent, val, 
			indices ? indices[indices.length - 1] : undefined);      
};

//TODO convenience method (default to firstChild nodeValue) 
F.insertValueE = function (triggerE, domObj /* . indices */) {
  if (!(triggerE instanceof F.EventStream)) { throw 'insertValueE: expected Event as first arg'; } //SAFETY
  
  var indices = Array.prototype.slice.call(arguments, 2);
  var parent = F.dom_.getMostDom(domObj, indices);
  
    triggerE.mapE(function (v) {
      F.dom_.deepStaticUpdate(parent, v, indices? indices[indices.length - 1] : undefined);
    });
};

//insertValueB: F.Behavior * domeNode . Array (id) -> void
//TODO notify adapter of initial state change?
F.insertValueB = function (triggerB, domObj /* . indices */) { 
  
  var indices = Array.prototype.slice.call(arguments, 2);
  var parent = F.dom_.getMostDom(domObj, indices);
  
  
  //NOW
  F.dom_.deepStaticUpdate(parent, triggerB, indices ? indices[indices.length - 1] : undefined);
  
  //LATER
  F.dom_.deepDynamicUpdate(parent, triggerB, indices? indices[indices.length -1] : undefined);
  
};

//TODO copy dom event call backs of original to new? i don't thinks so
//  complication though: registration of call backs should be scoped
F.insertDomE = function (triggerE, domObj) {
  
  if (!(triggerE instanceof F.EventStream)) { throw 'insertDomE: expected Event as first arg'; } //SAFETY
  
  var objD = F.dom_.getObj(domObj);
  
  var res = triggerE.mapE(
    function (newObj) {
      //TODO safer check
      if (!((typeof(newObj) === 'object') && (newObj.nodeType === 1))) { 
        newObj = SPAN({}, newObj);
      }
      F.dom_.swapDom(objD, newObj);
      objD = newObj;
      return newObj; // newObj;
    });
  
  return res;
};

//insertDom: dom 
//          * dom 
//          [* (null | undefined | 'over' | 'before' | 'after' | 'leftMost' | 'rightMost' | 'beginning' | 'end']
//          -> void
// TODO: for consistency, switch replaceWithD, hookD argument order
F.dom_.insertDomInternal = function (hookD, replaceWithD, optPosition) {
  switch (optPosition)
  {
  case undefined:
  case null:
  case 'over':
    F.dom_.swapDom(hookD,replaceWithD);
    break;
  case 'before':  
    hookD.parentNode.insertBefore(replaceWithD, hookD);
    break;
  case 'after':
    if (hookD.nextSibling) {
      hookD.parentNode.insertBefore(replaceWithD, hookD.nextSibling);
    } else {
      hookD.parentNode.appendChild(replaceWithD);
    }
    break;
  case 'leftMost':
    if (hookD.parentNode.firstChild) { 
      hookD.parentNode.insertBefore(
        replaceWithD, 
        hookD.parentNode.firstChild);
              } else { hookD.parentNode.appendChild(replaceWithD); }
              break;
            case 'rightMost':
              hookD.parentNode.appendChild(replaceWithD);
              break;
            case 'beginning':
              if (hookD.firstChild) { 
                hookD.insertBefore(
                  replaceWithD, 
                  hookD.firstChild);
              } else { hookD.appendChild(replaceWithD); }
              break;
            case 'end':
              hookD.appendChild(replaceWithD);
              break;
            default:
              throw ('domInsert: unknown position: ' + optPosition);
  }
};

//insertDom: dom 
//          * dom U String domID 
//          [* (null | undefined | 'over' | 'before' | 'after' | 'leftMost' | 'rightMost' | 'beginning' | 'end']
//          -> void
F.insertDom = function (replaceWithD, hook, optPosition) {
  //TODO span of textnode instead of textnode?
  F.dom_.insertDomInternal(
    F.dom_.getObj(hook), 
    ((typeof(replaceWithD) === 'object') && (replaceWithD.nodeType > 0)) ? replaceWithD :
    document.createTextNode(replaceWithD),      
    optPosition);           
};

/**
 * if optID not specified, id must be set in init val of trigger
 * if position is not specified, default to 'over'
 *
 * @param {F.Behavior|Node} initTriggerB
 * @param {string=} optID
 * @param {string=} optPosition
 */
F.insertDomB = function (initTriggerB, optID, optPosition) {
  
  if (!(initTriggerB instanceof F.Behavior)) { 
    initTriggerB = F.constantB(initTriggerB);
  }
  
  var triggerB = initTriggerB.liftB(function (d) { 
      if ((typeof(d) === 'object') && (d.nodeType >  0)) {
        return d;
      } else {
        var res = document.createElement('span'); //TODO createText instead
        res.appendChild(document.createTextNode(d));
        return res;
      }
    });
  
  var initD = triggerB.valueNow();
  if (!((typeof(initD) === 'object') && (initD.nodeType === 1))) { throw ('insertDomB: initial value conversion failed: ' + initD); } //SAFETY  
  
  F.dom_.insertDomInternal(
    optID === null || optID === undefined ? F.dom_.getObj(initD.getAttribute('id')) : F.dom_.getObj(optID), 
    initD, 
    optPosition);
  
  var resB = F.insertDomE(triggerB.changes(), initD).startsWith(initD);
  
  return resB;
};

/**
 * @param {Node} elem
 * @return {F.EventStream}
 */
F.mouseE = function(elem) {
  return F.extractEventE(elem,'mousemove')
  .mapE(function(evt) {
      if (evt.pageX | evt.pageY) {
        return { left: evt.pageX, top: evt.pageY };
      }
      else if (evt.clientX || evt.clientY) {
        return { left : evt.clientX + document.body.scrollLeft,
                 top: evt.clientY + document.body.scrollTop };
      }
      else {
        return { left: 0, top: 0 };
      }
  });
};

/**
 * @param {Node} elem
 * @return {F.Behavior}
 */
F.mouseB = function(elem) {
  return F.mouseE(elem).startsWith({ left: 0, top: 0 });
};

/**
 * @param {Node} elem
 * @return {F.Behavior}
 */
F.mouseLeftB = function(elem) {
  return F.mouseB(elem).liftB(function(v) { return v.left; });
};


/** 
 * @param {Node} elem
 * @return {F.Behavior}
 */
F.mouseTopB = function(elem) {
  return F.mouseB(elem).liftB(function(v) { return v.top; });
};

/**
 * @param {Node} elem
 * @return {F.EventStream}
 */
F.clicksE = function(elem) {
  return F.extractEventE(elem,'click');
};


//////////////////////////////////////////////////////////////////////////////
// Combinators for web services

F.xhr_.ajaxRequest = function(method,url,body,async,callback) {
  var xhr = new window.XMLHttpRequest();
  xhr.onload = function() { callback(xhr); };
  xhr.open(method,url,async);
  if (method === 'POST') {
    xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
  }
  xhr.send(body);
  return xhr;
};

F.xhr_.encodeREST = function(obj) {
  var str = "";
  for (var field in obj) {
    if (typeof(obj[field]) !== 'function') { // skips functions in the object
      if (str != '') { str += '&'; }
      str += field + '=' + encodeURIComponent(obj[field]);
    }
  }
  return str;
};

/**
 * @param {F.EventStream} requestE
 * @return F.EventStream
 */
F.getWebServiceObjectE = function(requestE) {
  var responseE = F.receiverE();

  requestE.mapE(function (obj) {
      var body = '';
      var method = 'GET';
      var url = obj.url;
      
      var reqType = obj.request ? obj.request : (obj.fields ? 'post' : 'get');
      if (obj.request === 'get') {
        if (obj.fields) { url += "?" + F.xhr_.encodeREST(obj.fields); }
        body = '';
        method = 'GET';
      } else if (obj.request === 'post') {
        body = JSON.stringify(obj.fields); 
        method = 'POST';
      } else if (obj.request === 'rawPost') {
        body = obj.body;
        method = 'POST';
      }
      else if (obj.request === 'rest') {
        body = F.xhr_.encodeREST(obj.fields);
        method = 'POST';
      }
      else {
        throw("Invalid request type: " + obj.request);
      }
      
      var async = obj.async;
      
      var xhr;
      
      // Branch on the response type to determine how to parse it
      if (obj.response === 'json') {
        xhr = F.xhr_.ajaxRequest(method,url,body,async,
          function(xhr) {
            responseE.sendEvent(JSON.parse(xhr.responseText)); 
          });
      }
      else if (obj.response === 'xml') {
        F.xhr_.ajaxRequest(method,url,body,async,
          function(xhr) {
            responseE.sendEvent(xhr.responseXML);
          });
      }
      else if (obj.response === 'plain' || !obj.response) {
        F.xhr_.ajaxRequest(method,url,body,async,
          function(xhr) {
            responseE.sendEvent(xhr.responseText);
        });
      }
      else {
        throw('Unknown response format: ' + obj.response);
      }
    return F.doNotPropagate;
  });
  
  return responseE;
};
