// :maxLineLen=80:
/*
Copyright (c) 2006-2008, the Flapjax Team All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the Brown University, the Flapjax Team, nor the names
  of its contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

//flapjaxInit: [Boolean] -> {...}
function flapjaxInit(options) {
  // compress via http://alex.dojotoolkit.org/shrinksafe/
  // make sure to change final eval call to use exported library renamed 
  // name
  
  var defaultOptions = {
    includeSynonyms: true,
    exportMisc: true,
    exportCore: true,
    exportDOM: true,
    exportDOMMisc: true,
    exportWS: true,
    hide: [ ],
    show: [ ],
    redefine: false,
  };
  
  if (options === false) {
    options = { hide: [ ], show: [ ] };
  }
  else if (options === true || !options) {
    options = { };
  }

  // Fill in any missing options using the defaults.
  for (var option in defaultOptions) {
    if (options[option] === undefined) {
      options[option] = defaultOptions[option];
    }
  };
  
  // Transform hide and show lists into tables
  var hideArray = options.hide;
  options.hide = { };
  for (var i = 0; i < hideArray.length; i++) {
    options.hide[hideArray[i]] = true;
  }
  var showArray = options.show;
  options.show = { };
  for (var i = 0; i < showArray.length; i++) {
    options.show[showArray[i]] = true;
  }
  
  var warn = function(s) {
    /*if (console && console.warn) {
      console.warn(s);
    }*/
  };
   
  
  var flapjax = {     
    version: 4, 
    base: {}, //pulses
    combinators: {}, // combinators yielding nodes
	  behaviours: {},
    dom: {} // dom convenience methods and combinators
  };
  
  var d = flapjax.dom;
  
	flapjax.pub = {util:flapjax};
  
	var annotate = function(fn,names,protoArg,protoObjs,protoNames) {
		for(var i=0; i<names.length;i++) {
			flapjax.pub[names[i]] = fn;
		}
		if(protoArg != undefined) {
			var pf = function() {
				var args = slice(arguments,0);
				args.splice(protoArg,0,this);
				return fn.apply(this,args);
			}
			for(var i=0; i<protoObjs.length; i++) {
				for(var j=0; j<protoNames.length;j++) {
					protoObjs[i][protoNames[j]] = pf;
				}
			}
		}
	}
  
  var exports = { };
  var synonyms = { };

  var misc = { }; /* Used as a symbol */
  var core = { }; /* Used as a symbol */
  var dom = { }; /* Used as a symbol */
  var domMisc = { }; /* Used as a symbol */
  var ws = { }; /* Used as a symbol */
  
  var fxExport = function(exportCategory,exportVal,
                          exportName /* , synonyms */) {
    if (!(exportCategory instanceof Object)) {
      throw 'fxExport: category is ' + exportCategory;
    };

    if (typeof(exportName) != 'string') {
      console.log(exportVal);
      throw 'fxExport: name is ' + exportName;   
    };

    exportVal.__flapjaxCategory = exportCategory;
    exports[exportName] = exportVal;
    for (var i = 3; i < arguments.length; i++) {
      exports[arguments[i]] = exportVal;
      synonyms[arguments[i]] = true;
    }
  }
  
  var fxMethodSynonyms = function(obj,method) {
    if (options.includeSynonyms === true) {
      for (var i = 2; i < arguments.length; i++) {
        obj.prototype[arguments[i]] = obj.prototype[method];
      };
    };
  };
  
  //////////////////////////////////////////////////////////////////////////////
  // Miscellaneous functions
  
	//credit 4umi
	//slice: Array a * Integer * Integer -> Array a
	var slice = function (arr, start, stop) {
		var i, len = arr.length, r = [];
		if( !stop ) { stop = len; }
		if( stop < 0 ) { stop = len + stop; }
		if( start < 0 ) { start = len - start; }
		if( stop < start ) { i = start; start = stop; stop = i; }
		for( i = 0; i < stop - start; i++ ) { r[i] = arr[start+i]; }
		return r;
	}
  
	var isEqual = function (a,b) {
		return (a == b) ||
			( (((typeof(a) == 'number') && isNaN(a)) || a == 'NaN') &&
			  (((typeof(b) == 'number') && isNaN(b)) || b == 'NaN') );
	};

	//member: a * Array b -> Boolean
	var member = function(elt, lst) {
		for (var i = 0; i < lst.length; i++) { 
			if (isEqual(lst[i], elt)) {return true;} 
		}
		return false;
	};
  
	var zip = function(arrays) {
		if (arrays.length == 0) return [];
		var ret = [];
		for(var i=0; i<arrays[0].length;i++) {
			ret.push([]);
			for(var j=0; j<arrays.length;j++) 
				ret[i].push(arrays[j][i]);
		}
		return ret;
	}

	//map: (a * ... -> z) * [a] * ... -> [z]
	var map = function (fn) {
		var arrays = slice(arguments, 1);
		if (arrays.length === 0) { return []; }
		else if (arrays.length === 1) {
			var ret = [];
			for(var i=0; i<arrays[0].length; i++) {ret.push(fn(arrays[0][i]));}
			return ret;
		}
		else {
			var ret = zip(arrays);
			var o = new Object();
			for(var i=0; i<ret.length; i++) {ret[i] = fn.apply(o,ret[i]);}
			return ret;
		}
	};
	  
	//filter: (a -> Boolean) * Array a -> Array a
	var filter = function (predFn, arr) {
		var res = [];
		for (var i = 0; i < arr.length; i++) { 
			if (predFn(arr[i])) { res.push(arr[i]); }
		}
		return res;
	};
	  
  //fold: (a * .... * accum -> accum) * accum * [a] * ... -> accum
  //fold over list(s), left to right
  var fold = function(fn, init /* arrays */) {
		var lists = slice(arguments, 2);
		if (lists.length === 0) { return init; }
		else if(lists.length === 1) {
			var acc = init;
			for(var i = 0; i < lists[0].length; i++) {
				acc = fn(lists[0][i],acc);
			}
			return acc;
		}
		else {
			var acc = init;
			for (var i = 0; i < lists[0].length; i++) {
				var args = map( function (lst) { return lst[i];}, 
		          lists);
				args.push(acc);
				acc = fn.apply({}, args);
			}
			return acc;
		}
  };
	  
  //foldR: (a * .... * accum -> accum) * accum * [a] * ... -> accum
	//fold over list(s), right to left, fold more memory efficient (left to right)
	var foldR = function (fn, init /* arrays */) {
    var lists = slice(arguments, 2);
		if (lists.length === 0) { return init; }
		else if(lists.length === 1) {
			var acc = init;
			for(var i=lists[0].length - 1; i > -1; i--)
				acc = fn(lists[0][i],acc);
			return acc;
		}
		else {
			var acc = init;
			for (var i = lists[0].length - 1; i > -1; i--) {
				var args = map( function (lst) { return lst[i];}, 
		          lists);
				args.push(acc);
				acc = fn.apply({}, args);
			}
			return acc;     
		}
	};
  
  var pushAll = function(destArray,srcArray) {
    for (var i = 0; i < srcArray.length; i++) {
      destArray.push(srcArray[i]);
    }
    return destArray;
  };
	  
  fxExport(misc,slice,'slice');
  fxExport(misc,member,'member');
  fxExport(misc,map,'map');
  fxExport(misc,map,'forEach');
  fxExport(misc,filter,'filter');
  fxExport(misc,fold,'fold');
  fxExport(misc,foldR,'foldR');
  
  //////////////////////////////////////////////////////////////////////////////
  // Flapjax core
  
  //Pulse: Stamp * Path * Obj
  var Pulse = function (stamp, value) {
    this.stamp = stamp;
    this.value = value;
  };


  //Probably can optimize as we expect increasing insert runs etc
  var PQ = function () {
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
			if(ctx.val.length == 1) {
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
				else if(leftChild < rightChild) {
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
  
  var lastRank = 0;
  var stamp = 1;
  var nextStamp = function () { return ++stamp; };
  
  //propagatePulse: Pulse * Array Node -> 
  //Send the pulse to each node 
  var propagatePulse = function (pulse, node) {
    var queue = new PQ(); //topological queue for current timestep
	queue.insert({k:node.rank,n:node,v:pulse});
	while(!(queue.isEmpty())) {
		var qv = queue.pop();
		qv.n.updater(function(nextPulse) {
			for(var i=0; i<qv.n.sendsTo.length;i++)
				queue.insert({k:qv.n.sendsTo[i].rank,n:qv.n.sendsTo[i],v:nextPulse});
		},new Pulse(qv.v.stamp,qv.v.value));
	}
  };
  
  //Event: Array Node b * ( (Pulse a -> Void) * Pulse b -> Void)
  var EventStream = function (nodes,updater) {
    this.updater = updater;
    
    this.sendsTo = []; //forward link
    
    for (var i = 0; i < nodes.length; i++) {
      nodes[i].sendsTo.push(this);
    }
    
    this.rank = ++lastRank;
  };
	EventStream.prototype = new Object();
  
  fxExport(core,EventStream,'EventStream');
  
  
  //createNode: Array Node a * ( (Pulse b ->) * (Pulse a) -> Void) -> Node b
  var createNode = function (nodes, updater) {
		return new EventStream(nodes,updater);
	};
  
  //attachListenerNode: Node * Node -> Voids
  //flow from node to dependent
  //note: does not add flow as counting for rank nor updates parent ranks
  var attachListener = function (node, dependent) {
    if (!(node instanceof EventStream)) { throw 'attachListenenerNode: expects event as first arg';} //SAFETY
    if (!(dependent instanceof EventStream)) { throw 'attachListenenerNode: expects event as second arg';} //SAFETY
    
    node.sendsTo.push(dependent);
		if(node.rank > dependent.rank) {
			var lowest = lastRank+1;
			var q = [dependent];
			while(q.length) {
				var cur = q.splice(0,1)[0];
				cur.rank = ++lastRank;
				q = q.concat(cur.sendsTo);
			}
		}
  };
  
  //removeListenerNode: Node * Node -> Boolean
  //remove flow from node to dependent
  //note: does not remove flow as counting for rank nor updates parent ranks
  var removeListener = function (node, dependent)
  {
    if (!(node instanceof EventStream)) { throw 'removeListenerNode: expects event as first arg';} //SAFETY
    if (!(dependent instanceof EventStream)) { throw 'removeListenenerNode: expects event as second arg';} //SAFETY
    
    var foundSending = false;
    for (var i = 0; i < node.sendsTo.length && !foundSending; i++) {
      if (node.sendsTo[i] == dependent) {
        node.sendsTo.splice(i, 1);
        foundSending = true;
      }
    }
    
    return foundSending;
  };
  
  // An internal_e is a node that simply propagates all pulses it receives.  It's used internally by various 
  // combinators.
  var internal_e = function(dependsOn) {
    return createNode(dependsOn || [ ],function(send,pulse) { send(pulse); });
  }
  
  var zero_e = function() {
    return createNode([],function(send,pulse) {
        throw ('zero_e : received a value; zero_e should not receive a value; the value was ' + pulse.value);
    });
  };
  fxExport(core,zero_e,'zero_e');
  
  var one_e = function(val) {
    var sent = false;
    var evt = createNode([],function(send,pulse) {
      if (sent) {
        throw ('one_e : received an extra value');
      }
      sent = true;
      send(pulse);
    });
    window.setTimeout(function() { sendEvent(evt,val); },0);
    return evt;
  };
  fxExport(core,one_e,'one_e');
  
  // a.k.a. mplus; merge_e(e1,e2) == merge_e(e2,e1)
  var merge_e = function() {
    if (arguments.length == 0) {
      return zero_e();
    }
    else {
      var deps = slice(arguments,0);
      return internal_e(deps);
    }
  };
  fxExport(core,merge_e,'merge_e');

  EventStream.prototype.merge_e = function() {
    var deps = slice(arguments,0);
    deps.push(this);
    return internal_e(deps);
  };
  fxMethodSynonyms(EventStream,'merge_e','merge');

  EventStream.prototype.constant_e = function(constantValue) {
    return createNode([this],function(send,pulse) {
      pulse.value = constantValue;
      send(pulse);
    });
  };
  fxMethodSynonyms(EventStream,'constant_e','constant','replaceValue_e','replaceValue');
  
  var constant_e = function(e) { return e.constant_e(); };
  fxExport(core,constant_e,'constant_e','replaceValue_e');
  
	var createTimeSyncNode = function(nodes) {
		var nqs = map(function(n) {
				var qpulse = [];
				return {q:qpulse,v:createNode([n],function(s,p) {qpulse.push(p.value); s(p);},nodes)};
    },nodes);
		return createNode(
      map(function(n) {return n.v;},nqs), function(s,p) {
        var allfull = fold(function(n,acc) {return n.q.length && acc;},true,nqs);
        if(allfull) {
          p.value = map(function(n) {return n.q.shift();},nqs);
          s(p);
      }});
	};
  
	//This is up here so we can add things to its prototype that are in flapjax.combinators
  var Behaviour = function (event, init, updater) {
    if (!(event instanceof EventStream)) { 
      throw 'Behaviour: expected event as second arg'; 
    }
    
    var behave = this;
    this.last = init;
    
    //sendEvent to this might impact other nodes that depend on this event
    //sendBehaviour defaults to this one
    this.underlyingRaw = event;
    
    //unexposed, sendEvent to this will only impact dependents of this behaviour
    this.underlying =
    createNode(
      [event], 
	  (updater ? 
	   function (s, p) {behave.last = updater(p.value); p.value = behave.last; s(p);} : 
	   function (s, p) {behave.last = p.value;  s(p);}));
  };
	Behaviour.prototype = new Object();
  fxExport(core,Behaviour,'Behaviour');
  fxExport(core,Behaviour,'Behavior');
  
  var receiver_e = function() {
    var evt = internal_e();
    evt.sendEvent = function(value) {
      propagatePulse(new Pulse(nextStamp(), value),evt);
    };
    return evt;
  };
  fxExport(core,receiver_e,'receiver_e');
  
  //note that this creates a new timestamp and new event queue
  var sendEvent = function (node, value) {
    if (!(node instanceof EventStream)) { throw 'sendEvent: expected Event as first arg'; } //SAFETY
    
    propagatePulse(new Pulse(nextStamp(), value),node);
  };
  fxExport(core,sendEvent,'sendEvent');
  synonyms['sendEvent'] = true; // Hack: sendEvent simply should not be exported; calling it a synonym deprecates it.
  
  // bind_e :: EventStream a * (a -> EventStream b) -> EventStream b
  EventStream.prototype.bind_e = function(k) {
    /* m.sendsTo result_e
     * result_e.sendsTo prev_e
     * prev_e.sendsTo return_e
     */
    var m = this;
    var prev_e = false;
    
    var out_e = createNode([],function(send,pulse) { send(pulse); });
    out_e.name = "bind out_e";
    
    var in_e = createNode([m], function (send,pulse) {
      if (prev_e) { 
        removeListener(prev_e,out_e);
      }
      prev_e = k(pulse.value);
      if (prev_e instanceof EventStream) {
        attachListener(prev_e,out_e);
      }
      else {
        throw "bind_e : expected EventStream";
      }
    });
    in_e.name = "bind in_e";
    
    return out_e;
  };

  /* Could be written as:
   *
   * e.bind_e(function(v) { return one_e(f(v)); })
   */
  EventStream.prototype.lift_e = function(f) {
    if (!(f instanceof Function)) {
      throw ('lift_e : expected a function as the first argument; received ' + f);
    };
    
    return createNode([this],function(send,pulse) {
      pulse.value = f(pulse.value);
      send(pulse);
    });
  };
  fxMethodSynonyms(EventStream,'lift_e',
    'transform_e','map_e','apply_e','lift','transform','map','apply');
  
  EventStream.prototype.not_e = function() { return this.lift_e(function(v) { return !v; }); };
  fxMethodSynonyms(EventStream,'not_e','not');
  
  var not_e = function(e) { return e.not_e(); };
  fxExport(core,not_e,'not_e','not');
  
  EventStream.prototype.filter_e = function(pred) {
    if (!(pred instanceof Function)) {
      throw ('filter_e : expected predicate; received ' + pred);
    };
    
    // Can be a bind_e
    return createNode([this],
      function(send,pulse) {
        if (pred(pulse.value)) { send(pulse); }
    });
  };
  fxMethodSynonyms(EventStream,'filter_e','filter');
  
  var filter_e = function(e,p) { return e.filter_e(p); };
  fxExport(core,filter_e,'filter_e');
  
  // Fires just once.
  EventStream.prototype.once_e = function() {
    var done = false;
    // Alternately: this.collect_e(0,\n v -> (n+1,v)).filter_e(\(n,v) -> n == 1).lift_e(fst)
    return createNode([this],function(send,pulse) {
      if (!done) { done = true; send(pulse); }
    });
  };
  fxMethodSynonyms(EventStream,'once_e','once')
  
  var once_e = function(e) { return e.once_e(); };
  fxExport(core,once_e,'once_e');
  
  EventStream.prototype.skipFirst_e = function() {
    var skipped = false;
    return createNode([this],function(send,pulse) {
      if (skipped)
        { send(pulse); }
      else
        { skipped = true; }
    });
  };
  fxMethodSynonyms(EventStream,'skipFirst_e','skipFirst');
  
  var skipFirst_e = function(e) { return e.skipFirst_e(); };
  fxExport(core,skipFirst_e,'skipFirst_e');
  
  EventStream.prototype.collect_e = function(init,fold) {
    var acc = init;
    return this.lift_e(
      function (n) {
        var next = fold(n, acc);
        acc = next;
        return next;
      });
  };
  fxMethodSynonyms(EventStream,'collect_e','collect','transformWithMemory');
  
  var collect_e = function(e,i,f) { return e.collect_e(i,f); };
  fxExport(core,collect_e,'collect_e');
  
  // a.k.a. join
  EventStream.prototype.switch_e = function() {
    return this.bind_e(function(v) { return v; });
  };
  fxMethodSynonyms(EventStream,'switch_e','forwardLatest');
  
  var switch_e = function(e) { return e.switch_e(); };
  fxExport(core,switch_e,'switch_e');
  
  EventStream.prototype.if_e = function(thenE,elseE) {
    var testStamp = -1;
    var testValue = false;
    
    createNode([this],function(_,pulse) { testStamp = pulse.stamp; testValue = pulse.value; });
    
    return merge_e(
      createNode([thenE],function(send,pulse) { if (testValue && (testStamp == pulse.stamp)) { send(pulse); } }),
      createNode([elseE],function(send,pulse) { if (!testValue && (testStamp == pulse.stamp)) { send(pulse); } }));
  };
  fxMethodSynonyms(EventStream,'if_e','choose_e','choose');
  
  var if_e = function(test,thenE,elseE) {
    if (test instanceof EventStream)
      { return test.if_e(thenE,elseE); }
    else
      { return test ? thenE : elseE; }
  };
  fxExport(core,if_e,'if_e','choose_e');
      
  var cond_e = function (/*. predValArrays */) {
    var predValArrays = slice(arguments, 0);
    var acc = zero_e();
    for (var i = predValArrays.length - 1; i > -1; i--) {
      acc = if_e(predValArrays[i][0],predValArrays[i][1], acc);
    }
    return acc; 
  };
  fxExport(core,cond_e,'cond_e');
  
  var and_e = function (/* . nodes */) {
    var nodes = slice(arguments, 0);
    
    var acc = (nodes.length > 0)? 
    nodes[nodes.length - 1] : one_e(true);
    
    for (var i = nodes.length - 2; i > -1; i--) {
      acc = if_e(
        nodes[i], 
        acc, 
        nodes[i].constant_e(false));
    }
    return acc;
  };
  fxExport(core,and_e,'and_e');
  
  EventStream.prototype.and_e = function( /* others */ ) {
    var deps = [this].concat(slice(arguments,0));
    return and_e.apply(this,deps);
  };
  fxMethodSynonyms(EventStream,'and_e','and');
  
  var or_e = function () {
    var nodes = slice(arguments, 0);
    var acc = (nodes.length > 2)? 
    nodes[nodes.length - 1] : one_e(false); 
    for (var i = nodes.length - 2; i > -1; i--) {
      acc = if_e(
        nodes[i],
        nodes[i],
        acc);
    }
    return acc;
  };
  fxExport(core,or_e,'or_e');
  
  EventStream.prototype.or_e = function(/*others*/) {
    var deps = [this].concat(slice(arguments,0));
    return or_e.apply(this,deps);
  };
  fxMethodSynonyms(EventStream,'or_e','or');
  
  var delayStatic_e = function (event, time) {
    
    var resE = internal_e();
    
    createNode(
      [event],
      function (s, p) { 
        setTimeout( 
          function () { sendEvent(resE, p.value);}, 
      time ); });
    
    return resE;
  };
  
  //delay_e: Event a * [Behaviour] Number ->  Event a
  EventStream.prototype.delay_e = function (time) {
    var event = this;
    
    if (time instanceof Behaviour) {
      
      var receiverEE = internal_e();
      var link = 
      {
        from: event, 
        towards: delayStatic_e(event, valueNow(time))
      };
      
			//TODO: Change semantics such that we are always guaranteed to get an event going out?
      var switcherE = 
      createNode(
        [changes(time)],
        function (s, p) {
          removeListener(link.from, link.towards); 
          link =
          {
            from: event, 
            towards: delayStatic_e(event, p.value)
          };
          sendEvent(receiverEE, link.towards);
        });
      
      var resE = receiverEE.switch_e();
      
      sendEvent(switcherE, valueNow(time));
      return resE;
      
        } else { return delayStatic_e(event, time); }
  };
  fxMethodSynonyms(EventStream,'delay_e','delay');
  
  var delay_e = function(sourceE,interval) {
    return sourceE.delay_e(interval);
  };
  fxExport(core,delay_e,'delay_e');
  
  //lift_e: ([Event] (. Array a -> b)) . Array [Event] a -> [Event] b
  var lift_e = function (fn /*, [node0 | val0], ...*/) {
    //      if (!(fn instanceof Function)) { throw 'lift_e: expected fn as second arg'; } //SAFETY
    
    var valsOrNodes = slice(arguments, 0);
    //selectors[i]() returns either the node or real val, optimize real vals
    var selectors = [];
    var selectI = 0;
    var nodes = [];
    for (var i = 0; i < valsOrNodes.length; i++) {
      if (valsOrNodes[i] instanceof EventStream) {
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
		var nofnodes = slice(selectors,1);
    
    if (nodes.length === 0) {
      return one_e(fn.apply(context, valsOrNodes));
    } else if ((nodes.length === 1) && (fn instanceof Function)) {
      return nodes[0].lift_e(
        function () {
          var args = arguments;
          return fn.apply(
            context, 
            map(function (s) {return s(args);}, nofnodes));
        });
    } else if (nodes.length === 1) {
      return fn.lift_e(
        function (v) {
          var args = arguments;
          return v.apply(
            context, 
            map(function (s) {return s(args);}, nofnodes));
        });                
    } else if (fn instanceof Function) {
      return createTimeSyncNode(nodes).lift_e(
        function (arr) {
          return fn.apply(
            this,
            map(function (s) { return s(arr); }, nofnodes));
        });
    } else if (fn instanceof EventStream) {
      return createTimeSyncNode(nodes).lift_e(
        function (arr) {
          return arr[0].apply(
            this, 
            map(function (s) {return s(arr); }, nofnodes));
        });
        } else {throw 'unknown lift_e case';}
  };
  fxExport(core,lift_e,'lift_e','transform_e','map_e','apply_e');
  
  EventStream.prototype.snapshot_e = function (valueB) {
    return createNode(
      [this],
      function (s, p) {
        p.value = valueNow(valueB);
        s(p);
      }
      ); 
  };
  fxMethodSynonyms(EventStream,'snapshot_e','snapshot','takeSnapshot');

  var snapshot_e = function(triggerE,valueB) {
    return triggerE.snapshot_e(valueB);
  };
  fxExport(core,snapshot_e,'snapshot_e');

  EventStream.prototype.filterRepeats_e = function(optStart) {
    var hadFirst = optStart === undefined ? false : true;
    var prev = optStart;
  
    return this.filter_e(function (v) {
      if (!hadFirst || !(isEqual(prev,v))) {
        hadFirst = true;
        prev = v;
        return true;
      }
      else {
        return false;
      }
    });
  };
  fxMethodSynonyms(EventStream,'filterRepeats_e','filterRepeats');

  var filterRepeats_e = function(sourceE,optStart) {
    return sourceE.filterRepeats_e(optStart);
  };
  fxExport(core,filterRepeats_e,'filterRepeats_e','filterRepeats');

  //credit Pete Hopkins
  var calmStatic_e = function (triggerE, time) {
		var out = internal_e();
    createNode(
      [triggerE],
      function() {
        var towards = null;
        return function (s, p) {
          if (towards !== null) { clearTimeout(towards); }
          towards = setTimeout( function () { towards = null; sendEvent(out,p.value) }, time );
        };
      }());
		return out;
  };
  
  //calm_e: Event a * [Behaviour] Number -> Event a
  EventStream.prototype.calm_e = function(time) {
    if (time instanceof Behaviour) {
			var out = internal_e();
      createNode(
        [this],
        function() {
          var towards = null;
          return function (s, p) {
            if (towards !== null) { clearTimeout(towards); }
            towards = setTimeout( function () { towards = null; sendEvent(out,p.value) }, valueNow(time));
          };
        }());
			return out;
    } else {
      return calmStatic_e(this,time);       
    }
  };
  fxMethodSynonyms(EventStream,'calm_e','calm');
  
  var calm_e = function(sourceE,interval) {
    return sourceE.calm_e(interval);
  };
  fxExport(core,calm_e,'calm_e');
  
  EventStream.prototype.blind_e = function (time) {
    return createNode(
      [this],
      function () {
        var intervalFn = 
        time instanceof Behaviour?
        function () { return valueNow(time); }
        : function () { return time; };
        var lastSent = (new Date()).getTime() - intervalFn() - 1;
        return function (s, p) {
          var curTime = (new Date()).getTime();
          if (curTime - lastSent > intervalFn()) {
            lastSent = curTime;
            s(p);
          }
        };
      }());
  };
  fxMethodSynonyms(EventStream,'blind_e','blind');
  
  var blind_e = function(sourceE,interval) {
    return sourceE.blind_e(interval);
  };
  fxExport(core,blind_e,'blind_e');
 
  EventStream.prototype.hold = function(init) {
    return new Behaviour(this,init);
  };
  fxMethodSynonyms(EventStream,'hold','startsWith','toBehavior');

  var hold = function(e,init) {
    if (!(e instanceof EventStream)) {
      throw 'hold: expected EventStream; received ' + e;
    }
    return e.hold(init); 
  };
  fxExport(core,hold,'hold');
  
  Behaviour.prototype.valueNow = function() {
    return this.last;
  };
  var valueNow = function(behavior) { return behavior.valueNow(); };
  fxExport(core,valueNow,'valueNow');

  Behaviour.prototype.changes = function() {
    return this.underlying;
  };
  fxMethodSynonyms(Behaviour,'changes','toEvent');
  
  var changes = function (behave) { return behave.changes(); }
  fxExport(core,changes,'changes');
  
  Behaviour.prototype.switch_b = function() {
    var behaviourCreatorsB = this;
    var init = valueNow(behaviourCreatorsB);
    
    var prevSourceE = null;
    
    var receiverE = new internal_e();
    
		//XXX could result in out-of-order propagation! Fix!
    var makerE = 
    createNode(
      [changes(behaviourCreatorsB)],
      function (_, p) {
        if (!(p.value instanceof Behaviour)) { throw 'switch_b: expected Behaviour as value of Behaviour of first argument'; } //SAFETY
        if (prevSourceE != null) {
          removeListener(prevSourceE, receiverE);
        }
        
        prevSourceE = changes(p.value);
        attachListener(prevSourceE, receiverE);
        
        sendEvent(receiverE, valueNow(p.value));
      });
    
    if (init instanceof Behaviour) {
      sendEvent(makerE, init);
    }
    
    return hold(
      receiverE,
      init instanceof Behaviour? valueNow(init) : init);
  };
  fxMethodSynonyms(Behaviour,'switch_b','forwardLatest');
  
  var switch_b = function (b) { return b.switch_b(); };
  fxExport(core,switch_b,'switch_b');
  
  //TODO test, signature
  var timer_b = function(interval) {
    return hold(timer_e(interval), (new Date()).getTime());
  };
  fxExport(dom,timer_b,'timer_b','asTimer_b');
  
  //TODO test, signature
  var delayStatic_b = function (triggerB, time, init) {
    return hold(delayStatic_e(changes(triggerB), time), init);
  };
  
  //TODO test, signature
  Behaviour.prototype.delay_b = function (time, init) {
    var triggerB = this;
    if (time instanceof Behaviour) {
      return hold(
        delay_e(
          changes(triggerB), 
          time),
        arguments.length > 3 ? init : valueNow(triggerB));
    } else {
      return delayStatic_b(
        triggerB, 
        time,
        arguments.length > 3 ? init : valueNow(triggerB));
    }
  };
  fxMethodSynonyms(Behaviour,'delay_b','delay');
  
  var delay_b = function(srcB, timeB, init) { 
    return srcB.delay_b(timeB,init); 
  };
  fxExport(core,delay_b,'delay_b');
  
  //artificially send a pulse to underlying event node of a behaviour
  //note: in use, might want to use a receiver node as a proxy or an identity map
  Behaviour.prototype.sendBehaviour = function(val) {
    sendEvent(this.underlyingRaw,val);
  };
  Behaviour.prototype.sendBehavior = Behaviour.prototype.sendBehaviour;
  
  var sendBehaviour = function (b,v) { b.sendBehaviour(v); };
  fxExport(core,sendBehaviour,'sendBehaviour');
  fxExport(core,sendBehaviour,'sendBehavior');

  Behaviour.prototype.if_b = function(trueB,falseB) {
    var testB = this;
    //TODO auto conversion for behaviour funcs
    if (!(trueB instanceof Behaviour)) { trueB = constant_b(trueB); }
    if (!(falseB instanceof Behaviour)) { falseB = constant_b(falseB); }
    return lift_b(function(te,t,f) { return te ? t : f; },testB,trueB,falseB);
  };
  fxMethodSynonyms(Behaviour,'if_b','choose_b','choose');
  
  var if_b = function(test,cons,altr) {
    if (!(test instanceof Behaviour)) { test = constant_b(test); };
    
    return test.if_b(cons,altr);
  };
  fxExport(core,if_b,'if_b','choose_b');
  

  //cond_b: . [Behaviour boolean, Behaviour a] -> Behaviour a
  var cond_b = function (/* . pairs */ ) {
    var pairs = slice(arguments, 0);
	return lift_b.apply({},[function() {
			for(var i=0;i<pairs.length;i++) {
				if(arguments[i]) return arguments[pairs.length+i];
			}
			return undefined;
		}].concat(map(function(pair) {return pair[0];},pairs).concat(map(function(pair) {return pair[1];},pairs))));
  };
  fxExport(core,cond_b,'cond_b');
  
  //TODO optionally append to objects
  //createConstantB: a -> Behaviour a
  var constant_b = function (val) {
    return new Behaviour(internal_e(), val);
  };
  fxExport(core,constant_b,'constant_b','receiver_b');
  
	var lift_b = function (fn /* . behaves */) {
    var args = slice(arguments, 1);

    //dependencies
    var constituentsE =
      map(changes,
      filter(function (v) { return v instanceof Behaviour; },
        arguments));
    
    //calculate new vals
    var getCur = function (v) {
      return v instanceof Behaviour ? v.last : v;
    };
    
    var ctx = this;
    var getRes = function () {
      return getCur(fn).apply(ctx, map(getCur, args));
    };
	
    if(constituentsE.length == 1) {
      return new Behaviour(constituentsE[0],getRes(),getRes);
    }
      
    //gen/send vals @ appropriate time
    var prevStamp = -1;
    var mid = createNode(constituentsE, function (s, p) {
        if (p.stamp != prevStamp) {
          prevStamp = p.stamp;
          s(p);
    }});
    return new Behaviour(mid,getRes(),getRes);
  };
  fxExport(core,lift_b,'lift_b','transform_b','apply_b');
  
  Behaviour.prototype.lift_b = function(/* args */) {
    var args= slice(arguments,0).concat([this]);
    return lift_b.apply(this,args);
  };
  fxMethodSynonyms(Behaviour,'lift_b','transform_b','apply_b','transform',
    'lift');
  
  var and_b = function (/* . behaves */) {
	return lift_b.apply({},[function() {
			for(var i=0; i<arguments.length; i++) {if(!arguments[i]) return false;}
			return true;
	}].concat(slice(arguments,0)));
  };
  fxExport(core, and_b, 'and_b');
  
  Behaviour.prototype.and_b = function() {
    return and_b([this].concat(arguments));
  };
  fxMethodSynonyms(Behaviour,'and_b','and');
  
  var or_b = function (/* . behaves */ ) {
	return lift_b.apply({},[function() {
			for(var i=0; i<arguments.length; i++) {if(arguments[i]) return true;}
			return false;
	}].concat(slice(arguments,0)));
  };
  fxExport(core,or_b,'or_b');
  
  Behaviour.prototype.or_b = function () {
    return or_b([this].concat(arguments));
  };
  fxMethodSynonyms(Behaviour, 'or_b', 'or');
  
  Behaviour.prototype.not_b = function() {
    return this.lift_b(function(v) { return !v; });
  };
  fxMethodSynonyms(Behaviour,'not_b','not');
  
  var not_b = function(b) { return b.not_b(); };
  fxExport(core,not_b,'not_b');
  
  Behaviour.prototype.blind_b = function (intervalB) {
    return changes(this).blind_e(intervalB).hold(this.valueNow());
  };
  fxMethodSynonyms(Behaviour,'blind_b','blind');
  
  var blind_b = function(srcB,intervalB) {
    return srcB.blind_b(intervalB);
  };
  fxExport(core,blind_b,'blind_b');
  
  Behaviour.prototype.calm_b = function (intervalB) {
    return this.changes().calm_e(intervalB).hold(this.valueNow());
  };
  fxMethodSynonyms(Behaviour,'calm_b','calm');
  
  var calm_b = function (srcB,intervalB) { 
    return srcB.calm_b(intervalB);
  };
  fxExport(core,calm_b,'calm_b');
    
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // DOM Utilities
  
	//credit Scott Andrew
	//usage: addEvent(myDomObj, "mouseover", event->void )
	//warning: do not use 'this' as meaning depends on browser (myDomObj vs window)
	//addEvent: Dom * String DomEventEnum * (DomEvent -> a) -> Void
	var addEvent = function (obj, evType, fn) {
		//TODO encode mouseleave evt, formalize new evt interface
    
		if (obj.addEventListener) {
      obj.addEventListener(evType, fn, false); //TODO true fails on Opera
      return true;
		} else if (obj.attachEvent) {
      //some reason original had code bloat here
      return obj.attachEvent("on"+evType, fn); 
		} else {
      return false; 
		}
	};
  fxExport(domMisc,addEvent,'addEvent');

  // The Flapjax library does not use this function  
	//credit Dustin Diaz 
	//note: node/tag optional
	//getElementsByClass: Regexp CSSSelector * Dom * String DomNodeEnum -> Array Dom
	var getElementsByClass = function (searchClass, node, tag) {
		var classElements = [];
		if ( (node === null) || (node === undefined) ) { node = document; }
		if ( (tag === null) || (tag === undefined) ) { tag = '*'; }
		var els = node.getElementsByTagName(tag);
		var elsLen = els.length;
		var pattern = new RegExp("(^|\\s)"+searchClass+"(\\s|$)");
		for (var i = 0, j = 0; i < elsLen; i++) {
      if ( pattern.test(els[i].className) ) {
        classElements.push(els[i]);
      }
		}
		return classElements;
	};
  fxExport(domMisc,getElementsByClass,'getElementsByClass','$$');
  
	//assumes IDs already preserved
	//swapDom: (Dom a U String) [* (Dom b U String)] -> Dom a
	var swapDom = function(replaceMe, withMe) {
		if ((replaceMe === null) || (replaceMe === undefined)) { throw ('swapDom: expected dom node or id, received: ' + replaceMe); } //SAFETY
    
		var replaceMeD = getObj(replaceMe);
		if (!(replaceMeD.nodeType > 0)) { throw ('swapDom expected a Dom node as first arg, received ' + replaceMeD); } //SAFETY
    
		if (withMe) {
      var withMeD = getObj(withMe);
      if (!(withMeD.nodeType > 0)) { throw 'swapDom: can only swap with a DOM object'; } //SAFETY
      try {
        if(withMeD != replaceMeD) replaceMeD.parentNode.replaceChild(withMeD, replaceMeD);
      } catch (e) {
        throw('swapDom error in replace call: withMeD: ' + withMeD + ', replaceMe Parent: ' + replaceMeD + ', ' + e + ', parent: ' + replaceMeD.parentNode);                    
      }
		} else {
      replaceMeD.parentNode.removeChild(replaceMeD); //TODO isolate child and set innerHTML to "" to avoid psuedo-leaks?
		}
		return replaceMeD;
	};
  fxExport(domMisc,swapDom,'swapDom');
  
	//getObj: String U Dom -> Dom
	//throws 
	//  'getObj: expects a Dom obj or Dom id as first arg'
	//  'getObj: flapjax: cannot access object'
	//  'getObj: no obj to get
	//also known as '$'
	//TODO Maybe alternative?
	var getObj = function (name) {
		if (typeof(name) == 'object') { return name; }
		else if ((typeof(name) == 'null') || (typeof(name) == 'undefined')) {
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
  fxExport(domMisc,getObj,'getObj');
  fxExport(domMisc,getObj,'$');
  
  //helper to reduce obj look ups
  //getDynObj: domNode . Array (id) -> domObj
  //obj * [] ->  obj
  //obj * ['position'] ->  obj
  //obj * ['style', 'color'] ->  obj.style
  var getMostDom = function (domObj, indices) {
    var acc = getObj(domObj);
    if ( (indices === null) || (indices === undefined) || (indices.length < 1)) {
      return acc;
    } else {
      for (var i = 0; i < indices.length - 1; i++) {
        acc = acc[indices[i]];
      }
      return acc;
    }       
  };
  
  var getDomVal = function (domObj, indices) {
    var val = getMostDom(domObj, indices);
    if (indices && indices.length > 0) {
      val = val[indices[indices.length - 1]];
    }
    return val;
  };
  
  //TODO: manual timer management stinks.
  // TODO: Name turn off or somethin
  var ___timerID = 0;
  var __getTimerId = function () { return ++___timerID; };    
  var timerDisablers = [];
  
  var disableTimerNode = function (node) { timerDisablers[node.__timerId](); };
  
  var disableTimer = function (v) {
    if (v instanceof Behaviour) { 
      disableTimerNode(v.underlyingRaw); 
    } else if (v instanceof EventStream) {
      disableTimerNode(v);
    }
  };
  fxExport(dom,disableTimer,'disableTimer');
  
  var createTimerNodeStatic = function (interval) {
    var node = internal_e();
    node.__timerId = __getTimerId();
    var fn = function () { sendEvent(node, (new Date()).getTime());};
    var timer = setInterval(fn, interval);
    timerDisablers[node.__timerId] = function () {clearInterval(timer); };
    return node;
  };
  
  var timer_e = function (interval) {
    if (interval instanceof Behaviour) {
      var receiverE = internal_e();
      
      //the return
      var res = receiverE.switch_e();
      
      //keep track of previous timer to disable it
      var prevE = createTimerNodeStatic(valueNow(interval));
      
      //init
      sendEvent(receiverE, prevE);
      
      //interval changes: collect old timer
      createNode(
        [changes(interval)],
        function (_, p) {
          disableTimerNode(prevE); 
          prevE = createTimerNodeStatic(p.value);
          sendEvent(receiverE, prevE);
        });
      
      res.__timerId = __getTimerId();
      timerDisablers[res.__timerId] = function () {
        disableTimerNode[prevE]();
        return;
      };
      
      return res;
    } else {
      return createTimerNodeStatic(interval);
    }
  };
  fxExport(dom,timer_e,'timer_e','asTimer_e');

	d.TagB = function(tagName,args) {
		this.resE = internal_e();
		this.currentTag = document.createElement(tagName);
		this.extractParameters(args);
		this.insertChildrenNodes();
		this.styleHooks = [];
		this.styleChangedE = internal_e();
		var ctx = this;
    this.styleChangedE.lift_e(function(_) {
				var oldTag = ctx.currentTag;
				ctx.currentTag = document.createElement(tagName);
        while (oldTag.firstChild) {
          ctx.currentTag.appendChild(oldTag.removeChild(oldTag.firstChild));
				}
				while(ctx.styleHooks.length) removeListener(ctx.styleHooks.pop(),ctx.styleChangedE);
        ctx.enstyle(ctx.currentTag,ctx.attribs);
        sendEvent(ctx.resE, ctx.currentTag);
		});
		this.enstyle(this.currentTag,this.attribs);
    this.resB = hold(this.resE, this.currentTag);
	};
	d.TagB.prototype = {
    // Array [[Behaviour] Object *] [[Behaviour] Array] [Behaviour] Dom U String U undefined
    //   --> {attribs: Array [Behaviour] Object, arrs: Array [Behaviour] Array [Behaviour] Dom }
    // split an arguments array into:
    //   1. arrs: (coalesced, and possibly time varying) arrays of dom objects 
    //   2. attribs: attribute objects
		extractParameters: function(args) {
			this.arrs = [];
			var attribs = [];
      
			var curarr = [];
			this.arrs.push(curarr);
			for (var i = 0; i < args.length; i++) {
				if (args[i] instanceof Behaviour) {
					var vn = valueNow(args[i]);
					if (vn instanceof Array) {	        
						this.arrs.push(args[i]);
						curarr = [];
						this.arrs.push(curarr);
					} else {
						if ( ((typeof(vn) == 'object') && (vn.nodeType == 1)) ||
              (typeof(vn) == 'string') || (vn == undefined)) {
            curarr.push(args[i]);
              } else if (typeof(vn) == 'object') {
                attribs.push(args[i]);
              }
              else { throw ('createParameterizedTagB: unknown behaviour argument argument ' + i); } //SAFETY
					}
				} else {
					if (args[i] instanceof Array) {
						var arr = args[i];
						for (var j = 0; j < arr.length; j++) { curarr.push(arr[j]); }
					} else {
						var vn = args[i];
						if ( ((typeof(vn) == 'object') && (vn.nodeType == 1)) ||
              (typeof(vn) == 'string') || (vn == undefined)) {
						curarr.push(args[i]);
              } else if (typeof(vn) == 'object') {
                attribs.push(args[i]);
              }
					}
				}
			};
			if(attribs.length > 1) throw ('createParameterizedTagB ' + tagName + ': more than one attribute (' + attribs.length + ')');
			this.attribs = attribs.length > 0 ? attribs[0] : {};
		},
		insertChildrenNodes: function() {
			var ctx = this;
      
			function quickNode(e) { 
        if ((typeof(e) == 'object') && (e.nodeType))
					return e; 
        else if ( e == undefined )
					return document.createTextNode(''); 
        else 
          return document.createTextNode(e); 
      }
      
			function unBehaviourize(arr) {
				return map(function(n) {return (n instanceof Behaviour) ? valueNow(n) : n;},arr)
			}
      
			var lnodes = map(function() {return [];},this.arrs);
			var arrLastVals = map(unBehaviourize,unBehaviourize(this.arrs));
      
			var arrChangesE = internal_e();
			var nodeChangesE = internal_e();
			
			function attachNodes(i,arr) {
				for(var j=0;j<arr.length;j++) {
					var cnode = arr[j];
					if(cnode instanceof Behaviour) {
						var newnode = (function(jj) {
                return changes(cnode).lift_e(function(n) {return {index:i,jdex:jj,val:n};});
						})(j);
						lnodes[i].push(newnode);
						attachListener(newnode,nodeChangesE);
						cnode = valueNow(cnode);
					}
				}
			}
      
			var childChangesE = merge_e(
				// Behaviour arrays change
				arrChangesE.lift_e(function(ai) {
            var i = ai.index;
            var newarr = ai.val;
            while(lnodes[i].length) {
              var ln = lnodes[i].pop();
              removeListener(ln,nodeChangesE);
            }
            var newvals = map(function(n) {return quickNode(n);},unBehaviourize(newarr));
            for(var j=0;j<arrLastVals[i].length;j++)
				try {
					ctx.currentTag.removeChild(arrLastVals[i][j]);
				}
				catch(e) {}
            if(newvals.length) {
              var nextNode = null;
              for(ii = i+1; ii < arrLastVals.length && !(arrLastVals[ii].length) ; ii++);
              if(ii < arrLastVals.length) nextNode = arrLastVals[ii][0];
              for(var j=0; j<newvals.length; j++) ctx.currentTag.insertBefore(newvals[j],nextNode);
            }
            arrLastVals[i] = newvals;
            attachNodes(i,newarr);
            return ctx.currentTag;
				}),
				// Behaviour nodes change
				nodeChangesE.lift_e(function(ni) {
            var i = ni.index;
            var j = ni.jdex;
            var newnode = quickNode(ni.val);
            swapDom(arrLastVals[i][j],newnode);
            arrLastVals[i][j] = newnode;
            return ctx.currentTag;
				}));
			childChangesE.lift_e(function(cc) {sendEvent(ctx.resE,cc);});
      
			for(var i=0; i<this.arrs.length;i++) {
				for(var j=0; j<arrLastVals[i].length; j++) {
					arrLastVals[i][j] = quickNode(arrLastVals[i][j]);
					this.currentTag.appendChild(arrLastVals[i][j]);
				}
				if(this.arrs[i] instanceof Behaviour) {
					attachNodes(i,valueNow(this.arrs[i]));
					var newnode = (function(ii) {return changes(ctx.arrs[ii]).lift_e(function(na) {return {index:ii,val:na};});})(i);
					attachListener(newnode,arrChangesE);
				}
				else {
					attachNodes(i,this.arrs[i]);
				}
			}
		},
		enstyle: function(obj,vals) {
			//build & record hook if dynamic collection
			if (vals instanceof Behaviour) {
				if (!(typeof(valueNow(vals)) == 'object')) { throw 'enstyle: expected object literal as behaviour value'; } //SAFETY
				this.styleHooks.push(changes(vals));
				attachListener(changes(vals),this.styleChangedE);
			}
			var valsV = vals instanceof Behaviour ? valueNow(vals) : vals;
			if (typeof(valsV) == 'object') {
				for (var i in valsV) {
					if (!(Object.prototype) || !(Object.prototype[i])) {
						this.enstyleProperty(obj,valsV, i);
					}
				}
			} 
			else { throw 'enstyle: expected object literals'; } //SAFETY
		},
    enstyleProperty: function (obj, vals, i) {
      if (vals[i] instanceof Behaviour) {
        if (typeof(valueNow(vals[i])) == 'object') {
          this.enstyle(obj[i], vals[i]);
        }
				else {
          obj[i] = valueNow(vals[i]);
					changes(vals[i]).lift_e(function(v) {obj[i] = v;});
				}
			}
			else {
        if (typeof(vals[i]) == 'object') {
          this.enstyle(obj[i], vals[i]);
        } else {
          obj[i] = vals[i];
        }
      }
    }
	};
  
	d.createParameterizedTagB = function(tagName) {
		return new d.TagB(tagName,slice(arguments,1)).resB;
	}
  
  d.enstyleStaticProperty = function (obj, props, index) {
    if (typeof(props[index]) == 'object') {
      for (var i in props[index]) {
        if (!(Object.prototype) || !(Object.prototype[i])) {
          d.enstyleStaticProperty(obj[index], props[index], i);
        }
      }
    } else {
      obj[index] = props[index];
			if (index == 'checked')	obj['defaultChecked'] = props[index]; /* TODO: this should maybe be elsewhere? */
			if (index == 'selected') obj['defaultSelected'] = props[index]; /* TODO: this should maybe be elsewhere? */
    }
  };
  
  d.staticTagMaker = function (tagName) {
    
    return function () {
      
      var tagD = document.createElement(tagName);
      if (!(tagD.nodeType > 0)) { throw (tagName + ': invalid tag name'); } //SAFETY
      
      //partition input
      
      //          if (arguments[1] === null || arguments[1] === undefined) { arguments[1] = {}; }
      var attribs = [];
      for (var i = 0; i < arguments.length; i++) {
        if (arguments[i] instanceof Array) {
          for (var j = 0; j < arguments[i].length; j++) {
            if (arguments[i][j]) {
              tagD.appendChild(
                (typeof(arguments[i][j]) == 'object' &&
                  arguments[i][j].nodeType > 0)?
                arguments[i][j] :
                document.createTextNode(arguments[i][j]));
            }
          }
        } else if (!arguments[i]) {
          //ignore
        } else if ((typeof(arguments[i]) == 'object') && 
          (arguments[i].nodeType > 0)) {
        tagD.appendChild(arguments[i]);
          } else if (typeof(arguments[i]) == 'object') {
            attribs.push(arguments[i]);
          } else {
            tagD.appendChild(document.createTextNode(arguments[i]));                    
          }
      }
      
      if (attribs.length == 1) { 
        for (var k in attribs[0]) {
          if (!(Object.prototype) || !(Object.prototype[k])) {
            d.enstyleStaticProperty(tagD, attribs[0], k);   
          }
        }
      } else if (attribs.length >  0) { 
        throw 'static enstyle: expected object literals'; //SAFETY
      } /* else {
      alert('no attribs on: ' + tagName);
      } */
      
      
      return tagD;
    };
  };
  
  
  
  var generatedTags = 
  [ "a", "b", "blockquote", "br", "button", "canvas", "div", "fieldset", 
  "form", "font", "h1", "h2", "h3", "h4", "hr", "img", "iframe", "input", 
  "label", "legend", "li", "ol", "optgroup", "option", 
  "p", "pre", "select", "span", "strong", "table", "tbody", 
  "td", "textarea", "tfoot", "th", "thead", "tr", "tt", "ul" ];
  
  map(
    function (tagName) { 
      
      var upper = tagName.toUpperCase();
      
      //d.<TAG>B
      d[upper + 'B'] = function () { 
        return d.createParameterizedTagB.apply(this, [tagName].concat(slice(arguments,0)));
      };          
			annotate(d[upper+'B'],[upper+'B']);
      
      //d.<TAG>
      d[upper] = d.staticTagMaker(tagName);  //faster, simple
      /*              function () {
      // 8/16/06 - leo - arguments bug
      // more forgiving: allows reactive children (just doesn't propagate them)
      var args = [b.maybeEmpty].concat(slice(arguments, 0)); 
      return valueNow(d[upper + 'B'].apply(this, args));
      }; */
			annotate(d[upper],[upper]);
    },
    generatedTags);
  
  //TEXTB: Behaviour a -> Behaviour Dom TextNode    
  d.TEXTB = function (strB) {
    //      if (!(strB instanceof Behaviour || typeof(strB) == 'string')) { throw 'TEXTB: expected Behaviour as second arg'; } //SAFETY
    if (!(strB instanceof Behaviour)) { strB = constant_b(strB); }
    
    return hold(
      changes(strB).lift_e(
        function (txt) { return document.createTextNode(txt); }),
      document.createTextNode(valueNow(strB)));
  };
	annotate(d.TEXTB,['TEXTB']);
  
  var TEXT = function (str) {
    return document.createTextNode(str);
  };
  fxExport(dom,TEXT,'TEXT');
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Reactive DOM
  
  //tagRec: Array (EventName a) * 
  //      ( .Array (Event a) * Array (Event a) -> Behaviour Dom) -> Behaviour Dom
  var tagRec = function (eventNames, maker) {
    if (!(eventNames instanceof Array)) { throw 'tagRec: expected array of event names as first arg'; } //SAFETY
    if (!(maker instanceof Function)) { throw 'tagRec: expected function as second arg'; } //SAFETY
    
    var numEvents = eventNames.length;
    var internals = [ ];
    var switches = [ ];
    for (var i = 0; i < numEvents; i++) {
      internals[i] = internal_e();
      switches[i] = internals[i].switch_e();
    };
    
    var domB = maker.apply(this,switches);
    
    var prevValue;
    
    var interceptE = createNode([domB.changes()],function (_,p) {
      if (isEqual(p.value,prevValue)) { return; }
      
      prevValue = p.value;
      for (var i = 0; i < numEvents; i++) {
        sendEvent(internals[i],extractEvent_e(prevValue,eventNames[i]));
      };
    });
    
    sendEvent(interceptE,domB.valueNow());
    
    return domB;
  };
  fxExport(dom,tagRec,'tagRec','nodeWithOwnEvents');
  
  //extractEventStatic_e: Dom * String -> Event
  var extractEventStatic_e = function (domObj, eventName) {
    if (!eventName) { throw 'extractEvent_e : no event name specified'; }
    if (!domObj) { throw 'extractEvent_e : no DOM element specified'; }
    
    domObj = getObj(domObj);
    
    var primEventE = internal_e();
    addEvent(domObj,eventName,function(evt) {
       sendEvent(primEventE, evt || window.event);
       // Important for IE; false would prevent things like a checkbox actually
       // checking.
       return true;
    });
    
    return primEventE;
  };

  //extractEvent_e: [Behaviour] Dom * String -> Event
  var extractEvent_e = function (domB, eventName) {
    if (!(domB instanceof Behaviour)) {
      return extractEventStatic_e(domB,eventName);
    }
    else {
      var domE = domB.changes();
      
      var eventEE = domE.lift_e(function(dom) {
        return extractEventStatic_e(dom,eventName);
      });
      
      var resultE = eventEE.switch_e();
      
      sendEvent(domE,domB.valueNow());
      
      return resultE;
    };
  };
  fxExport(dom,extractEvent_e,'extractEvent_e','$EVT');
  
  //extractEvents_e: 
  //      [Behaviour] Dom  
  //      . Array String
  //      -> Event
  // ex: extractEvents_e(m, 'body', 'mouseover', 'mouseout')
  d.extractEvents_e = function (domObj /* . eventNames */) {
    var eventNames = slice(arguments, 1);
    
    var events = map(
      function (eventName) {
        return extractEvent_e(domObj, eventName); 
      },
      eventNames.length === 0 ? [] : eventNames);
    
    return merge_e.apply(this, events);
  };
  annotate(d.extractEvents_e,['extractEvents_e'],0,[Behaviour.prototype],['extractEvents_e']);
  
  //value of dom form object during trigger
  d.extractValueOnEvent_e = function (triggerE, domObj) {
    if (!(triggerE instanceof EventStream)) { throw 'extractValueOnEvent_e: expected Event as first arg'; } //SAFETY
    
    return changes(d.extractValueOnEvent_b.apply(this, arguments));
    
  };
  annotate(d.extractValueOnEvent_e,['extractValueOnEvent_e']);
  
  //extractDomFieldOnEvent_e: Event * Dom U String . Array String -> Event a
  d.extractDomFieldOnEvent_e = function (triggerE, domObj /* . indices */) {
    if (!(triggerE instanceof EventStream)) { throw 'extractDomFieldOnEvent_e: expected Event as first arg'; } //SAFETY
    var indices = slice(arguments, 2);
    var res =
    triggerE.lift_e(
      function () { return getDomVal(domObj, indices); });
    return res;
  };
  
  d.extractValue_e = function (domObj) {
    return changes(d.extractValue_b.apply(this, arguments));
  };
  annotate(d.extractValue_e,['extractValue_e','$E']);
  
  //extractValueOnEvent_b: Event * DOM -> Behaviour
  // value of a dom form object, polled during trigger
  d.extractValueOnEvent_b = function (triggerE, domObj) {
    return d.extractValueStatic_b(domObj, triggerE);
  };
  annotate(d.extractValueOnEvent_b,['extractValueOnEvent_b']);
  
  //extractValueStatic_b: DOM [ * Event ] -> Behaviour a
  //If no trigger for extraction is specified, guess one
  d.extractValueStatic_b = function (domObj, triggerE) {
    
    var objD;
    try {
      objD = getObj(domObj);
	    //This is for IE
	    if(typeof(domObj) == 'string' && objD.id != domObj) {
	    	throw 'Make a radio group';
	    }
    } catch (e) {
      objD = {type: 'radio-group', name: domObj};
    }
    
    var getter; // get value at any current point in time
    
    
    switch (objD.type)  {
      
      //TODO: checkbox.value instead of status?
    case 'checkbox': 
      
      return hold(
        filterRepeats_e(
          d.extractDomFieldOnEvent_e(
            triggerE ? triggerE : 
            d.extractEvents_e(
              objD, 
              'click', 'keyup', 'change'),
            objD,
            'checked'),objD.checked),
        objD.checked);
      
      case 'select-one':
        
        getter = function (_) {                         
          return objD.selectedIndex > -1 ? 
          (objD.options[objD.selectedIndex].value ?
            objD.options[objD.selectedIndex].value :
            objD.options[objD.selectedIndex].innerText)
			    : undefined;
        };
        
        return hold(
          filterRepeats_e(
              (triggerE ? triggerE :
              d.extractEvents_e(
                objD,
                'click', 'keyup', 'change')).lift_e(getter)),getter(),
          getter());
        
        case 'select-multiple':
          //TODO ryan's cfilter adapted for equality check
          
          getter = function (_) {
            var res = [];
            for (var i = 0; i < objD.options.length; i++) {
              if (objD.options[i].selected) {
                res.push(objD.options[i].value ? objD.options[i].value : objD.options[i].innerText);
              }
            }
            return res;
          };
          
          
          return hold(
              (triggerE ? triggerE : 
              d.extractEvents_e(
                objD,
                'click', 'keyup', 'change')).lift_e(getter),
            getter());
          
          case 'text':
          case 'textarea':
          case 'hidden':
          case 'password':
            
            return hold(
              filterRepeats_e(
                d.extractDomFieldOnEvent_e(
                  triggerE ? triggerE :
                  d.extractEvents_e(
                    objD, 
                    'click', 'keyup', 'change'),
                  objD,
                  'value'),objD.value),
              objD.value);
            
            case 'button': //same as above, but don't filter repeats
              
              return hold(
                d.extractDomFieldOnEvent_e(
                  triggerE ? triggerE :
                  d.extractEvents_e(
                    objD, 
                    'click', 'keyup', 'change'),
                  objD,
                  'value'),
                objD.value);
              
              
              case 'radio': 
              case 'radio-group':
                
                //TODO returns value of selected button, but if none specified,
                //      returns 'on', which is ambiguous. could return index,
                //      but that is probably more annoying
                
                var radiosAD = filter(
                  function (elt) { 
                    return (elt.type == 'radio') &&
                    (elt.getAttribute('name') == objD.name); 
                  },
                  document.getElementsByTagName('input'));
                
                getter = 
                objD.type == 'radio' ?
                
                function (_) {
                  return objD.checked;
                } :
                
                function (_) {
                  for (var i = 0; i < radiosAD.length; i++) {
                    if (radiosAD[i].checked) {
                      return radiosAD[i].value; 
                    }
                  }
                  return undefined; //TODO throw exn? 
                };
                
                var actualTriggerE = triggerE ? triggerE :
                merge_e.apply(
                  this,
                  map(
                    function (radio) { 
                      return d.extractEvents_e(
                        radio, 
                    'click', 'keyup', 'change'); },
                      radiosAD));
                
                return hold(
                  filterRepeats_e(
                      actualTriggerE.lift_e(getter),getter()),
                  getter());
                
                default:
                  
                  throw ('extractValueStatic_b: unknown value type "' + objD.type + '"');
    }
  };
  
  d.extractValue_b = function (domObj) {
    if (domObj instanceof Behaviour) {
      return lift_b(function (dom) { return d.extractValueStatic_b(dom); },
                    domObj)
             .switch_b();
    } else {
      return d.extractValueStatic_b(domObj);
    }
  };
	annotate(d.extractValue_b,['extractValue_b','$B'],0,[Behaviour.prototype],['extractValue_b','$B']);
  
  //into[index] = deepValueNow(from) via descending from object and mutating each field
  d.deepStaticUpdate = function (into, from, index) {
    var fV = (from instanceof Behaviour)? valueNow(from) : from;
    if (typeof(fV) == 'object') {
      for (var i in fV) {
        if (!(Object.prototype) || !(Object.prototype[i])) {
          d.deepStaticUpdate(index? into[index] : into, fV[i], i);
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
  d.deepDynamicUpdate = function (into, from, index) {
    var fV = (from instanceof Behaviour)? valueNow(from) : from;
    if (typeof(fV) == 'object') {
      if (from instanceof Behaviour) {
        throw 'deepDynamicUpdate: dynamic collections not supported';
      }
      for (var i in fV) {
        if (!(Object.prototype) || !(Object.prototype[i])) {
          d.deepDynamicUpdate(index? into[index] : into, fV[i], i);
        }
      }
    } else {
      if (from instanceof Behaviour) {
        createNode(
          [changes(from)],
          function (s, p) {
            if (index) { 
              var old = into[index];
              into[index] = p.value;
            }
            else { into = p.value; } //TODO notify topE?
          });
      }
    }
  };
  
	
  d.insertValue = function (val, domObj /* . indices */) {
    var indices = slice(arguments, 2);
    var parent = getMostDom(domObj, indices);
    d.deepStaticUpdate(parent, val, indices ? indices[indices.length - 1] : undefined);    	
  };
  annotate(d.insertValue,['insertValue']);
  
  //TODO convenience method (default to firstChild nodeValue) 
  d.insertValueE = function (triggerE, domObj /* . indices */) {
    if (!(triggerE instanceof EventStream)) { throw 'insertValueE: expected Event as first arg'; } //SAFETY
    
    var indices = slice(arguments, 2);
    var parent = getMostDom(domObj, indices);
    
      triggerE.lift_e(function (v) {
        d.deepStaticUpdate(parent, v, indices? indices[indices.length - 1] : undefined);
      });
  };
  annotate(d.insertValueE,['insertValueE'],0,[EventStream.prototype],['insertValueE']);
  
  //insertValueB: Behaviour * domeNode . Array (id) -> void
  //TODO notify adapter of initial state change?
  d.insertValueB = function (triggerB, domObj /* . indices */) { 
    
    var indices = slice(arguments, 2);
    var parent = getMostDom(domObj, indices);
    
    
    //NOW
    d.deepStaticUpdate(parent, triggerB, indices ? indices[indices.length - 1] : undefined);
    
    //LATER
    d.deepDynamicUpdate(parent, triggerB, indices? indices[indices.length -1] : undefined);
    
  };
  annotate(d.insertValueB,['insertValueB'],0,[Behaviour.prototype],['insertValueB']);
  
  //TODO copy dom event call backs of original to new? i don't thinks so
  //  complication though: registration of call backs should be scoped
  d.insertDomE = function (triggerE, domObj) {
    
    if (!(triggerE instanceof EventStream)) { throw 'insertDomE: expected Event as first arg'; } //SAFETY
    
    var objD = getObj(domObj);
    
    var res = triggerE.lift_e(
      function (newObj) {
        //TODO safer check
        if (!((typeof(newObj) == 'object') && (newObj.nodeType == 1))) { 
          newObj = d.SPAN({}, newObj);
        }
        swapDom(objD, newObj);
        objD = newObj;
        return newObj; // newObj;
      });
    
    return res;
  };
  annotate(d.insertDomE,['insertDomE'],0,[EventStream.prototype],['insertDomE']);
  
  //insertDom: dom 
  //          * dom 
  //          [* (null | undefined | 'over' | 'before' | 'after' | 'leftMost' | 'rightMost' | 'beginning' | 'end']
  //          -> void
  // TODO: for consistency, switch replaceWithD, hookD argument order
  d.insertDom = function (hookD, replaceWithD, optPosition) {
    switch (optPosition)
    {
    case undefined:
    case null:
    case 'over':
      swapDom(hookD,replaceWithD);
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
  d.insertDomClean = function (replaceWithD, hook, optPosition) {
    //TODO span of textnode instead of textnode?
    d.insertDom(
	    getObj(hook), 
	    ((typeof(replaceWithD) == 'object') && (replaceWithD.nodeType > 0)) ? replaceWithD :
      document.createTextNode(replaceWithD),	    
	    optPosition);     	    
  };
  annotate(d.insertDomClean,['insertDom']);
  
  //TODO test
  //insertDomB: 
  //      [Behaviour] String U Dom 
  //      [* ( id U null U undefined ) 
  //          [* ('before' U 'after' U 'leftMost' U 'rightMost' U 'over' U 'beginning' U 'end')]]
  //      -> Behaviour a
  //if optID not specified, id must be set in init val of trigger
  //if position is not specified, default to 'over'
  //performs initial swap onload    
  d.insertDomB = function (initTriggerB, optID, optPosition) {
    
    if (!(initTriggerB instanceof Behaviour)) { 
      initTriggerB = constant_b(initTriggerB);
    }
    
    var triggerB = 
    lift_b(
      function (d) { 
        if ((typeof(d) == 'object') && (d.nodeType >  0)) {
          return d;
        } else {
          var res = document.createElement('span'); //TODO createText instead
          res.appendChild(document.createTextNode(d));
          return res;
        }
      },
      initTriggerB);
    
    var initD = valueNow(triggerB);
    if (!((typeof(initD) == 'object') && (initD.nodeType == 1))) { throw ('insertDomB: initial value conversion failed: ' + initD); } //SAFETY	
    
    d.insertDom(
      optID === null || optID === undefined ? getObj(initD.getAttribute('id')) : getObj(optID), 
      initD, 
      optPosition);
    
    var resB = hold(
      d.insertDomE(
        changes(triggerB),
        initD), 
      initD);
    
    return resB;
  };
  annotate(d.insertDomB,['insertDomB'],0,[Behaviour.prototype],['insertDomB']);
  
  
  d.extractId_b = function (id, start)
  {
    return hold(
      createNode( start instanceof Behaviour? [changes(start)] :
        [],
        function (s, p) {
          p.value = getObj(id);
          s(p);
        }),
      getObj(id));
  };
  annotate(d.insertDomB,['extractId_b'],1,[Behaviour.prototype],['extractId_b']);
  
  var mouse_e = function(elem) {
    return extractEvent_e(elem,'mousemove')
    .lift_e(function(evt) {
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
  
  var mouse_b = function(elem) {
    return mouse_e(elem).hold({ left: 0, top: 0 });
  }
  fxExport(dom,mouse_b,'mouse_b');
  
  var mouseLeft_b = function(elem) {
    return lift_b(function(v) { return v.left; },mouse_b(elem));
  };
  fxExport(dom,mouseLeft_b,'mouseLeft_b');
  
  var mouseTop_b = function(elem) {
    return mouse_b(elem).lift_b(function(v) { return v.top; });
  };
  
  fxExport(dom,mouseTop_b,'mouseTop_b');
  
  var clicks_e = function(elem) {
    return extractEvent_e(elem,'click');
  };
  fxExport(dom,clicks_e,'clicks_e');
  
  //////////////////////////////////////////////////////////////////////////////
  // Combinators for web services
  
  
	//credit Matt White
	var getURLParam = function (param) {
    var lparam = param.toLowerCase();
    var aReturn = [];
    var strHref = window.location.href;
    var endstr = (strHref.indexOf('#') > -1) ? strHref.indexOf('#') : strHref.length;
    if ( strHref.indexOf("?") > -1 ) {
      var strQueryString = strHref.substring(strHref.indexOf("?")+1,endstr);
      map(function(qp) {
          var eq = qp.indexOf('=');
          var qname = qp.substr(0,eq+1).toLowerCase();
          if(qname == lparam+"=") aReturn.push(decodeURIComponent(qp.substr(eq+1)));
      },strQueryString.split("&"));
    }
    if (aReturn.length == 0) return undefined;
    else if(aReturn.length == 1) return aReturn[0];
    else return aReturn;
	};
  fxExport(domMisc,getURLParam,'getURLParam','$URL');
  
	//credit Quirksmode
	//readCookie: String -> String U Undefined
	var readCookie = function (name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for (var i=0; i < ca.length; i++) {
      var co = ca[i];
      while (co.charAt(0) == ' ') { co = co.substring(1, co.length); }
      if (co.indexOf(nameEQ) === 0) { 
        return co.substring(nameEQ.length, co.length);
      }
    }
    return undefined;       
	};
  fxExport(domMisc,readCookie,'readCookie');
  
  //========== dynamic scripts ==========
  var scriptCounter = 0;
  var deleteScript = function (scriptID) {
    var scriptD = getObj(scriptID);
    scriptD.parentNode.removeChild(scriptD); //TODO isolate child and set innerHTML to "" to avoid psuedo-leaks?
  };
  
  // optional fn/param that gets polled until parm is defined
  var runScript = function (url, fn, param) {
    var script = document.createElement("script");
    script.src = url;
    var scriptID = 'scriptFnRPC' + scriptCounter++;
    script.setAttribute('id', scriptID);
    document.getElementsByTagName("head").item(0).appendChild(script);
    var timer = {};
    var check = 
    function () {
      eval("try { if (" + param + "!== undefined) {var stat = " + param + ";}} catch (e) {}");
      if (stat !== undefined) {
        eval(param + " = undefined;");
        clearInterval(timer.timer);
        clearInterval(timer.timeout);
        if (fn instanceof Function) {
          fn(stat); 
        }
        deleteScript(scriptID);
      }
    };
    timer.timer = setInterval(check, 3500);
    timer.timeout = 
    setTimeout( 
      function () { 
        try { clearInterval(timer.timer); }
        catch (e) {}
      },
      5000); //TODO make parameter?
  };
  
  // Node {url, globalArg} -> Node a
  //load script @ url and poll until param is set, then pass it along
  var evalForeignScriptVal_e = function(urlArg_e) {
    var result = receiver_e();
    urlArg_e.lift_e(function(urlArg) {
        runScript(urlArg.url,
          function(val) { result.sendEvent(val); }, 
          urlArg.globalArg);
    });
    
    return result;
  };
  fxExport(ws,evalForeignScriptVal_e,'evalForeignScriptVal_e');
  
  var ajaxRequest = function(method,url,body,async,useFlash,callback) {
    var xhr;
    if (useFlash) {
      xhr = new FlashXMLHttpRequest();
      xhr.onload = function() { callback(xhr); };
    }
    else if (window.XMLHttpRequest && !(window.ActiveXObject)) {
      xhr = new window.XMLHttpRequest();
      xhr.onload = function() { callback(xhr); };
    }
    else if (window.ActiveXObject) {
      try { xhr = new ActiveXObject("Msxml2.XMLHTTP"); }
      catch(e) { xhr = new ActiveXObject("Microsoft.XMLHTTP"); }
      
      xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) { callback(xhr); }
      };
    };
    
    xhr.open(method,url,async);
    
    if (method == 'POST') {
      xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
    }
    
    xhr.send(body);
    return xhr;
  };

  var encodeREST = function(obj) {
    var str = "";
    for (var field in obj) {
      if (typeof(obj[field]) !== 'function') { // skips functions in the object
        if (str != '') { str += '&'; }
        str += field + '=' + encodeURIComponent(obj[field]);
      }
    }
    return str;
  };

  // From json.org
  var parseJSON = function(str) {
    try {
        return !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
                str.replace(/"(\\.|[^"\\])*"/g, ''))) &&
            eval('(' + str + ')');
    } catch (e) {
        throw 'cannot parse JSON string: ' + e;
    }
  };

  var toJSONString = (function() {
    var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };
    var s = {
      array: function (x) {
        var a = ['['], b, f, i, l = x.length, v;
        for (i = 0; i < l; i += 1) {
          v = x[i];
          f = s[typeof v];
          if (f) {
            v = f(v);
            if (typeof v == 'string') {
              if (b) {
                a[a.length] = ',';
              }
              a[a.length] = v;
              b = true;
            }
          }
        }
        a[a.length] = ']';
        return a.join('');
      },
      'boolean': function (x) {
        return String(x);
      },
      'null': function (x) {
        return "null";
      },
      number: function (x) {
        return isFinite(x) ? String(x) : 'null';
      },
      object: function (x) {
        if (x) {
          if (x instanceof Array) {
            return s.array(x);
          }
          var a = ['{'], b, f, i, v;
          for (i in x) {
            v = x[i];
            f = s[typeof v];
            if (f) {
              v = f(v);
              if (typeof v == 'string') {
                if (b) {
                  a[a.length] = ',';
                }
                a.push(s.string(i), ':', v);
                b = true;
              }
            }
          }
          a[a.length] = '}';
          return a.join('');
        }
        return 'null';
      },
      string: function (x) {
        if (/["\\\x00-\x1f]/.test(x)) {
          x = x.replace(/([\x00-\x1f\\"])/g, function(a, b) {
              var c = m[b];
              if (c) {
                return c;
              }
              c = b.charCodeAt();
              return '\\u00' +
              Math.floor(c / 16).toString(16) +
              (c % 16).toString(16);
          });
        }
        return '"' + x + '"';
      }
    };
    return function(val) {
      var f = s[typeof val];
      if (f) { return f(val); }
      else { throw 'parseJSON: unknown object type: ' + (typeof val); }
    };
  })();
   
  var serverRequest_e = function(useFlash,requestE) {
    var response_e = receiver_e();
    
    requestE.lift_e(function (obj) {
        var body = '';
        var method = 'GET';
        var url = obj.url;
        
        var reqType = obj.request ? obj.request : (obj.fields ? 'post' : 'get');
        if (obj.request == 'get') {
          url += "?" + encodeRest(obj.fields);
          body = '';
          method = 'GET';
        } else if (obj.request == 'post') {
          body = toJSONString(obj.fields); 
          method = 'POST';
        } else if (obj.request == 'rawPost') {
          body = obj.body;
          method = 'POST';
        }
        else if (obj.request == 'rest') {
          body = encodeREST(obj.fields);
          method = 'POST';
        }
        else {
          raise ("Invalid request type: " + obj.request);
        }
        
        var async = obj.async;
        
        var xhr;
        
        // Branch on the response type to determine how to parse it
        if (obj.response == 'json') {
          xhr = ajaxRequest(method,url,body,async,useFlash,
            function(xhr) {
              response_e.sendEvent(parseJSON(xhr.responseText)); 
            });
        }
        else if (obj.response == 'xml') {
          ajaxRequest(method,url,body,async,useFlash,
            function(xhr) {
              response_e.sendEvent(xhr.responseXML);
            });
        }
        else if (obj.response == 'plain' || !obj.response) {
          ajaxRequest(method,url,body,async,useFlash,
            function(xhr) {
              response_e.sendEvent(xhr.responseText);
          });
        }
        else {
          raise('Unknown response format: ' + obj.response);
        }
    });
    
    return response_e;
  };
  
  var getWebServiceObject_e = function(requestE) {
    return serverRequest_e(false,requestE);
  };
  fxExport(ws,getWebServiceObject_e,'getWebServiceObject_e');
  
  var getForeignWebServiceObject_e = function(requestE) {
    return serverRequest_e(true,requestE);
  };
  fxExport(ws,getForeignWebServiceObject_e,'getForeignWebServiceObject_e');

  var cumulativeOffset = function(element) {
    var valueT = 0, valueL = 0;
    do {
      valueT += element.offsetTop  || 0;
      valueL += element.offsetLeft || 0;
      element = element.offsetParent;
    } while (element);
    return { left: valueL, top: valueT };
 };
fxExport(domMisc,cumulativeOffset,'cumulativeOffset');

  ///////// OPTIONALLY EXPORT PUBLIC FUNCTIONS TO GLOBAL NAMESPACE
  if (!options.hideAll) {
    for (var zz in flapjax.pub) {
      if (!options[zz]) {
        window[zz] = flapjax.pub[zz]; // use eval instead so as to achieve Flash compatibility
      }
    }
  }
  
  for (var id in exports) {
    if (window[id]) {
      if (options.redefine === false) {
        warn('window.' + id + ' is already defined; Flapjax will not ' +
             'redefine it.');
      }
      else {
        warn('Flapjax is redefining window.' + id + '.');
      }
    }

    if (window[id] && options.redefine === false) {
      // Don't redefine id
    }
    else if (options.show && options.hide && options.show[id] === true && 
        options.hide[id] === true) {
      throw('flapjaxInit: invalid options: ' + id + " specified as 'show' and 'hide'"); 
    }
    else if (options.show && options.show[id]) {
      window[id] = exports[id];
    }
    else if (options.hide && options.hide[id]) {
      // do nothing -- don't export
    }
    // Default behavior--check if it is a synonym first ...
    else if (!synonyms[id] || options.includeSynonyms) {
      // ... then export if its of the right category
      if (options.exportMisc && exports[id].__flapjaxCategory === misc) {
        window[id] = exports[id];
      }
      else if (options.exportDOMMisc && exports[id].__flapjaxCategory === domMisc) {
        window[id] = exports[id];
      }
      else if (options.exportDOM && exports[id].__flapjaxCategory === dom) {
        window[id] = exports[id];
      }
      else if (options.exportCore && exports[id].__flapjaxCategory === core) {
        window[id] = exports[id];
      }
      else if (options.exportWS && exports[id].__flapjaxCategory === ws) {
        window[id] = exports[id];
      }
    }
  }
  
  return exports;
}
