function testSuite() {

	var flapjax = flapjaxInit().util;  
  
	var a = function (b, m) { (b? info : error)('tests: ' + m); };

	//test strong single value 
	var sTest = function (inpt, out, msg, k)
	{
		k = true;
		if (inpt !== out)
		{
			error(msg + ' input: ' + inpt + ' out: ' + out);
			return false;
		}
		else { if (k) {info(' . . . try: ' + msg);} }
		return true;
	};

	//test weak single value 
	var swTest = function (inpt, out, msg, k)
	{
		if (inpt != out)
		{
			error(msg + ' input: ' + inpt + ' out: ' + out);
			return false;
		}
		else { if (k) {info(' . . . try: ' + msg);} }
		return true;
	};


	//test array
	var aTest = function (arr1, arr2, msg)
	{
		var legit = true;
		
		if (arr1.length != arr2.length)
		{
			legit = false;
			error(msg + 'array lengths wrong	arr1: [' + arr1.join(':::') 
				+ ']	arr2: [' + arr2.join(':::') +']');
		}
				
		for (var i = 0; i < Math.min(arr1.length, arr2.length); i++)
		{
			if (arr1[i] !== arr2[i])
			{
				legit = false;
				error('	 bad arr entries [' + i + ']: ' 
					+ '	   arr1: ' + arr1[i] + '	 arr2:' + arr2[i]); 
			}
		}
		if (!legit) 
		{
			error('^^^^^^: ' + msg + '	  arr1: [' + arr1.join(':::') 
				+ ']	  arr2: [' + arr2.join(':::') + ']');
		}

		return legit;
	};

	//test array of arrays
	var aaTest = function (arr1, arr2, msg)
	{
		var legit = true;
		
		if (arr1.length != arr2.length)
		{
			legit = false;
			error(msg + 'array lengths wrong	arr1: [' + arr1.join(':::') 
				+ ']	  arr2: [' + arr2.join(':::') +']');
		}

		for (var i = 0; i < Math.min(arr1.length, arr2.length); i++) {
			legit = legit && aTest(arr1[i], arr2[i], msg); 
		}

		return legit;
	};

	/* 
	//test testers
	sTest(1, 1, 'same1');
	sTest(1, 2, 'not same2');
	aTest([], [], 'same3'); 
	aTest([1], [1], 'same4'); 
	aTest([1, 2], [1, 2], 'same5');
	aTest([], [1], 'not same6'); 
	aTest([1, 2], [1, 2, 3], 'not same7'); 
	aTest([1,2], [1,3], 'not same8'); */
   
	/* proto =============================================*/
	/* lib _______________________________________________*/
	info('====flapjax.lib====');
	
	var l = flapjax.lib;
	
	var bLib = true;
	var bLSum = true;
	var bLMax = true;
	var bLMin = true;
	var bLWMap = true;
	var bLWMapM = true;
	var bLWFoldL = true;
	var bLWFoldLM = true;
	
	
	var add1 = function (x) {return 1 + x;};
	var diff = function (n1, n2) {return n1 - n2;};
	bLWMapM &= aTest(l.map(diff, []), [], 'wmapm1');
	bLWMapM &= aTest(l.map(diff, [1,4],[3,5]), [-2,-1], 'wmapm2');
	bLWMapM &= aTest(l.map(add1, [1, 1, 2]), [2, 2, 3], 'wmapm3');
	a(bLWMapM, 'map multiple');
	
	var summer = function (x, acc) {return x + acc;};
	bLWFoldLM &= sTest(l.fold(summer, 1, []), 1, 'wfold1');
	bLWFoldLM &= sTest(l.fold(summer, 1, [1,2,3]), 7, 'wfold2');
	
	var summerm = function (n1, n2, acc) {return n1 + n2 + acc;};
	bLWFoldLM &= sTest(l.fold(summerm, 1, [1,2,3],[1,2,1]), 11, 'wfoldlm1');
	bLWFoldLM &= sTest(l.fold(summerm, 1, [], []), 1, 'wfoldlm2');
	
	var multsummerm = function (n1, n2, acc) {return n1 * n2 + acc;};
	bLWFoldLM &= sTest(l.fold(multsummerm, 1, [2,5,7], [2, 11, 3]),
	81, 'wfoldlm3');

	a(bLWFoldLM, 'foldlm');

	bLib &= bLSum && bLMax && bLMin && bLWMap && bLWMapM && bLWFoldL && bLWFoldLM;
	(bLib? info : error)('====flapjax.lib====');
   
	var b = flapjax.base;

	/* engine ____________________________________________*/
	info('====flapjax.engine====');
	
	var bEng = true;
	var bStamp = true;

	var e = flapjax.engine;
	
	var bCreateNode = true;

	var pulse2 = new b.Pulse(70, 2);
		var recConst = 0;
	
	var secondsNode = 
		e.createNode(
			[], 
			function (send, pulse) {
				send(pulse);
			});

	var secondsAdd1Node = 
		e.createNode(
			[secondsNode], 
			function (send, pulse) {
				recConst = pulse.value + 1;
				pulse.value  = recConst;
				send(pulse);
			});

	bCreateNode &= sTest( 
		(function () {
			b.propagatePulse(pulse2, secondsNode); return recConst;})(),
		3, 
		'createNode1');
	a(bCreateNode, 'createNode');
	
	var bConstantNode = true;

	recConst = -2;	
	var const1	= e.createConstantNode([], 1);
	var receiveN = 
		e.createNode(
			[const1], 
			function (send, pulse) { 
				recConst = pulse.value; send(pulse);});
	bConstantNode &= 
		sTest( 
			(function(){b.propagatePulse(pulse2, const1); return recConst;})(), 
			1, 
			'constantNode1', 
			false);
	a(bConstantNode, 'createConstantNode');

	bEng &= bStamp && bCreateNode && bConstantNode;
		(bEng? info : error)('====flapjax.engine====');


		/* combinators ____________________________________________*/
		info('====flapjax.combinators====');
	var bComb = true;
	var c = flapjax.combinators;

	var bEventReceiver = true;
	var received = 0;
	var receiver = c.createEventReceiver();
	var passer = e.createNode([receiver], function (s, p) {received = p.value;});
	
	bEventReceiver &= sTest( (function(){b.propagatePulse(pulse2, receiver); return received;})(),
		2, 'createEventReceiver1');
	a(bEventReceiver, 'createEventReceiver');
	
	var bSendEvent = true;
	received = 0;
	
	bSendEvent &= sTest( (function(){c.sendEvent(passer, 3, 3); return received;})(),
		3, 'sendEvent1');
	a(bSendEvent, 'sendEvent');

	var bMapEV = true;
	received = 0;
	var receiver2 = c.createEventReceiver();
	var doubler = c.map_ev(function (v) {received = 2 * v; return 2*v;}, receiver2);
	
	bMapEV &= sTest( (function(){c.sendEvent(receiver2, 3, 3); return received;})(),
		6, 'bMapEV1');
	a(bMapEV, 'map_ev');

	var bFilterEV = true;
	received = 0;
	var receiver3 = c.createEventReceiver();
	var gt5 = c.filter_ev(function(v){return (v > 5);}, receiver3);
	var recorder = e.createNode([gt5], function (s, p) {received = p.value; s(p);});

	bFilterEV &= sTest((function(){c.sendEvent(receiver3, 1); return received;})(),
		0, 'filter_ev1');
	received = 0;
	bFilterEV &= sTest((function(){c.sendEvent(receiver3, 6); return received;})(),
		6, 'filter_ev2');
	
	a(bFilterEV, 'filter_ev');
	
    var bFilterRepeatsE = true;
    bFilterRepeatsE &= sTest((function (){
            var received = 0;
            var receiver = c.createEventReceiver();
            var f = c.filterRepeats_e(receiver);
            e.createNode([f], function (s, p) { received += p.value; s(p)});
            c.sendEvent(receiver, 1);
            c.sendEvent(receiver, 1);
            return received;})(), 1, 'filterRepeats_e1');
     bFilterRepeatsE &= sTest((function (){
            var received = 0;
            var receiver = c.createEventReceiver();
            var f = c.filterRepeats_e(receiver);
            e.createNode([f], function (s, p) { received += p.value; s(p)});
            c.sendEvent(receiver, 1);
            c.sendEvent(receiver, 2);
            c.sendEvent(receiver, 1);
            return received;})(), 4, 'filterRepeats_e2');
     bFilterRepeatsE &= sTest((function (){
            var received = 'asdf';
            var receiver = c.createEventReceiver();
            var f = c.filterRepeats_e(receiver);
            e.createNode([f], function (s, p) { received += p.value; s(p)});
            c.sendEvent(receiver, 'a');
            c.sendEvent(receiver, 'a');
            return received;})(), 'asdfa', 'filterRepeats_e3');
     a(bFilterRepeatsE, 'filterRepeats_e3');


	var bMergeE = true;
	var mergeERcva = c.createEventReceiver();
	var mergeERcvb = c.createEventReceiver();
	bMergeE &= sTest( (function () {
		var mergeERcvd0 = 0;
		var mergeERcvr0 = c.merge_e(mergeERcva);
		c.map_ev(function (v) {return (mergeERcvd0 = v);},
		mergeERcvr0);
		c.sendEvent(mergeERcva, 1);
		return mergeERcvd0; })(), 1, 'merge_e1');
	bMergeE &= sTest( (function () {
		var mergeERcvd1 = 1;
		var mergeERcvr1 = c.merge_e(mergeERcva, mergeERcvb);
		c.map_ev(function (v) { return (mergeERcvd1 *= v); },
		mergeERcvr1);
		c.sendEvent(mergeERcva, 3);
		c.sendEvent(mergeERcvb, 5);
		return mergeERcvd1; })(), 15, 'merge_e2');
	a(bMergeE, 'merge_e');

	var bOnceE = true;
	var onceERcvE = c.createEventReceiver();
	bOnceE &= sTest( 
		(function () {
			var result = 0;
			c.map_ev(
				function (v) { result = v; return v; },
				c.once_e(onceERcvE));
			c.sendEvent(onceERcvE, 1);
			c.sendEvent(onceERcvE, 2);
			return result; })(),
		1,
		'once_e1');
	
	a(bOnceE, 'once_e');

	//TODO test collect_e

	var bSwitchE = true;
	
	bSwitchE &=
		sTest(
			(function () {
				var stream1 = c.createEventReceiver();
				var stream2 = c.createEventReceiver();
				var streamReceiverE = c.createEventReceiver();
				var switchedE = c.switch_e(streamReceiverE);				
				var lastReceived = undefined;
				c.map_ev(
					function (v) { lastReceived = v; },
					switchedE);					
				c.sendEvent(streamReceiverE, stream1);
				c.sendEvent(stream1, 1);
				return lastReceived;
			})(),
			1,
			'switch_e1');

	bSwitchE &=
		sTest(
			(function () {
				var stream1 = c.createEventReceiver();
				var stream2 = c.createEventReceiver();
				var streamReceiverE = c.createEventReceiver();
				var switchedE = c.switch_e(streamReceiverE);				
				var lastReceived = undefined;
				c.map_ev(
					function (v) { lastReceived = v; },
					switchedE);					
				c.sendEvent(streamReceiverE, stream1);
				c.sendEvent(streamReceiverE, stream2);
				c.sendEvent(stream2, 2);
				return lastReceived;
			})(),
			2,
			'switch_e2');

	bSwitchE &=
		sTest(
			(function () {
				var stream1 = c.createEventReceiver();
				var stream2 = c.createEventReceiver();
				var streamReceiverE = c.createEventReceiver();
				var switchedE = c.switch_e(streamReceiverE);				
				var lastReceived = undefined;
				c.map_ev(
					function (v) { lastReceived = v; },
					switchedE);					
				c.sendEvent(streamReceiverE, stream1);
				c.sendEvent(streamReceiverE, stream2);
				c.sendEvent(stream2, 2);
				c.sendEvent(stream1, 1);
				return lastReceived;
			})(),
			2,
			'switch_e3');

	a(bSwitchE, 'switch_e');


	var bIfE = true;
	received = 0;
	receiver3 = c.createEventReceiver();
	var iffer = c.if_e(
		receiver3, 
		e.createConstantNode([receiver3], 2),
			e.createConstantNode([receiver3], 3));
	var receiver4 = e.createNode([iffer], function (s, p) {
		 received = p.value; s(p);});	
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, true); return received;})(),
		2, 'if_e1');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, true); return received;})(),
		2, 'if_e2');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, false); return received;})(),
		3, 'if_e3');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, false); return received;})(),
		3, 'if_e4');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, true); return received;})(),
		2, 'if_e5');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(receiver3, false); return received;})(),
		3, 'if_e6');
	var ifReceiver5 = c.createEventReceiver();
	var ifReceiver6b = c.createEventReceiver();
	var iffer2 = c.if_e(ifReceiver5, ifReceiver5, ifReceiver6b);
	var ifReceiver6 = e.createNode([iffer2], function (s, p) {
			received = p.value; s(p); });
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(ifReceiver5, true); return received;})(),
			true, 'if_e7');
	received = 0;
	bIfE &= sTest((function(){
			c.sendEvent(ifReceiver5, false); return received;})(),
			0, 'if_e8');
	a(bIfE, 'if_e');

	var bCondE = true;
	var rcvCond1 = c.createEventReceiver();
	var conder1 = c.cond_e();
	bCondE &= sTest((function(){
			received = 0;
		c.sendEvent(rcvCond1, 1);
		return received;})(),
		0, 'cond_e1');
	
	var rcvCond2 = c.createEventReceiver();
	var conder2 = c.cond_e(
		[e.createConstantNode([rcvCond2], true), 
			e.createConstantNode([rcvCond2], 2)]);
	var receiverCond2 = e.createNode([conder2], function (s, p) {
		received = p.value; s(p);});
	bCondE &= sTest((function(){
			received = 0;
		c.sendEvent(rcvCond2, 1);
		return received;})(),
		2, 'cond_e2');

	var rcvCond = c.createEventReceiver();
	var m = function (f) { return c.map_ev(f, rcvCond);};
	var conder = c.cond_e(
		[m(function(x){ return (x < 0);}), 
		 e.createConstantNode([rcvCond], -1)],
		[m(function(x){ return (x < 1);}), 
		 e.createConstantNode([rcvCond], 1)],
		[e.createConstantNode([rcvCond], true), 
		 e.createConstantNode([rcvCond], 2)]);
	var receiverCond = e.createNode([conder], function(s,p) {
		received = p.value;	 s(p);});
	bCondE &= sTest((function(){
			received = 0; c.sendEvent(rcvCond, -1); return received;})(),
		-1, 'cond_e3');
	bCondE &= sTest((function(){
		received = 0; c.sendEvent(rcvCond, 0); return received;})(),
		1, 'cond_e4');
	bCondE &= sTest((function(){
		received = 0; c.sendEvent(rcvCond, 2); return received;})(),
		2, 'cond_e5'); 

	a(bCondE, 'cond_e');

	var bAndE = true;
	var andReceiver1 = c.createEventReceiver();
	var and1N = c.and_e(andReceiver1);
	var andResult = 0;
	var andRecord1 = c.map_ev( 
		function (v) {andResult = v; return v;},
		and1N);
	bAndE &= sTest( (function(){
			andResult = 0; 
			c.sendEvent(andReceiver1, true); 
			return andResult;})(),
		true, 'and_e1');
	bAndE &= sTest( (function (){
			andResult = 0;	
			c.sendEvent(andReceiver1, false);
			return andResult;})(),
		false, 'and_e2');

	var andResult2 = 0;
	var and2N = c.and_e(and1N, and1N);
	var andRecord2 = c.map_ev(
		function (v) {andResult2 = v; return v;},
		and2N);
	bAndE &= sTest( (function(){
			andResult2 = -2; 
			c.sendEvent(andReceiver1, true); 
			return andResult2;})(),
		true, 'and_e3');
		
		
		
	bAndE &= sTest( (function (){
			andResult2 = -2;	
			c.sendEvent(andReceiver1, false);
			return andResult2;})(),
		false, 'and_e4');
	
	var bOrE = true;

	var bTimer = true;

	/* should print a couple times */
	var oftenTimer = c.createTimerNode(100);
	var timeReceiver = e.createNode([oftenTimer], function(s, p) {
		info('timer test (2x): ' + p.value); s(p); });
	setTimeout(function () {c.disableTimerNode(oftenTimer)}, 201);

	var bDelay = true;
	
	
	/* should print twice */
	var delayTimerE = c.createTimerNode(100);
	c.map_ev(
		function (v) {info('delay test received: ' + (new Date()).getTime()); return v;},
		c.delay_e(
			c.map_ev(
				function (v) { info('delay sent: ' + v); return v; },	
				delayTimerE),
			30));
	setTimeout(function () {c.disableTimerNode(delayTimerE)}, 201);

	var bLift = true;

	var bLiftE = true;		
	var toLiftAddXY = function (x, y) { return x + y; };
	var liftX = c.createEventReceiver();
	var liftY = e.createConstantNode([liftX], 3);
	var lifted = c.lift_e(toLiftAddXY, liftX, liftY);
	var recordLift = e.createNode([lifted], function (s, p) {
		received = p.value; s(p); });
	bLiftE &= sTest((function(){
		received = 0; c.sendEvent(liftX, 4); return received;})(),
		7, 'bLiftE1');

	var liftZ = c.createEventReceiver();
	var lifted2 = c.lift_e(toLiftAddXY, liftZ, 20);
	var recordLift2 = e.createNode([lifted2], function (s, p) {
		received = p.value; s(p); });
	bLiftE &= sTest((function(){
		received = 0; c.sendEvent(liftZ, 5); return received;})(),
		25, 'bLiftE2');

	bLiftE &= sTest((function(){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
			var resE =
				c.lift_e(
					function (v) { return v * 2; },
					c.collect_ev(
						senderE,
						0,
						function (_, acc) { return acc + 1; }));
			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 2);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 4);
			return success;
		})(), true, 'bLiftE3');


	bLiftE &= sTest((function(){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
			var count4e = c.collect_ev(
				senderE,
				0,
				function (_, acc) { return acc + 1; });
			var count4e2 = c.map_ev(
				function (v) { return v; },
				count4e);
				
			var resE = c.lift_e(
					function (l, r) { return l + r; },
					count4e, 
					count4e2);
			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 2);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 4);
			return success;
		})(), true, 'bLiftE4');

	bLiftE &= sTest((function(){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
			var count5e = c.collect_ev(
				senderE,
				0,
				function (_, acc) { return acc + 1; });
			var count5e2 = c.map_ev(
				function (v) { return v; },
				count5e);
				
			var resE = c.lift_e(
					function (l, r) { return l + r; },
					count5e2, 
					count5e);
			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 2);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 4);
			return success;
		})(), true, 'bLiftE5');

	bLiftE &= sTest((function(){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
			var count6e = c.collect_ev(
				senderE,
				0,
				function (_, acc) { return acc + 1; });
			var lifteFn6 = 
				c.map_ev(
					function (p) {
						return function (c) { 
							return (p? 1 : -1) * c;
						};
					},
					c.collect_ev( 
						count6e,
						false,
						function (_, acc) { return !acc; }));
					
			var resE = c.lift_e(
					lifteFn6,
					count6e); 

			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 1);
			c.sendEvent(senderE, 'beep');
			success = success && (start === -2);
			return success;
		})(), true, 'bLiftE6');

	bLiftE &= sTest((function(){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
			var count7e = c.collect_ev(
				senderE,
				0,
				function (_, acc) { return acc + 1; });
			var lifteFn7 = 
				c.lift_e(
					function (p) {
						return function (l, r) { 
							return (p? 1 : -1) * (l + r);
						};
					},
					c.collect_ev(
						count7e,
						false,
						function (_, acc) { return !acc; }));
					
			var resE = c.lift_e(
					lifteFn7,
					count7e,
					count7e); 

			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 'beep');
			success = success && (start === 2);
			c.sendEvent(senderE, 'beep');
			success = success && (start === -4);
			return success;
		})(), true, 'bLiftE7');

	bLiftE &= sTest((function (){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
					
			var resE = c.lift_e(
					function (x, y) { return x + y; },
					senderE,
					senderE); 

			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 1);
			success = success && (start === 2);
			c.sendEvent(senderE, 2);
			success = success && (start === 4);
			return success;
		})(), true, 'bLiftE8');		
				
	bLiftE &= sTest((function (){
			var success = true;
			var start = 0;
			var senderE = c.createEventReceiver();
					
			var resE = c.lift_e(
					function (x, y, z) { return x + y + z; },
					senderE,
					senderE,
					senderE); 

			e.createNode([resE], function (s, p) { start=p.value; });
			success = success && (start === 0);
			c.sendEvent(senderE, 1);
			success = success && (start === 3);
			c.sendEvent(senderE, 2);
			success = success && (start === 6);
			return success;
		})(), true, 'bLiftE9');	
		

	a(bLiftE, 'liftFn');

	bComb &= bEventReceiver && bSendEvent && bMapEV && bFilterEV && bIfE;
	bComb &= bAndE && bOrE && bTimer && bMergeE && bLiftE && bOnceE;
		(bComb? info : error)('====flapjax.combinators====');


	/* behaviours  ____________________________________________*/
		info('====flapjax.behaviours====');
	var bBehaviour = true;
	var be = flapjax.behaviours;

	var beReceiverB = new be.Behaviour(
		c.createEventReceiver(),
		1);

	bBehaviour &= sTest( be.valueNow(beReceiverB), 1, 'basic behaviour');

	var bMapB = true;
	var beMapAdd1B = be.lift_b(
		function (v) {return 1 + v;},
		beReceiverB);
	bMapB &= sTest( be.valueNow(beMapAdd1B), 2, 'map init');
	bMapB &= sTest( (function () {
		c.sendEvent(be.changes(beReceiverB), 3);
		return be.valueNow(beMapAdd1B);})(),
		4, 'lift_b');
	a(bMapB, 'lift_b');

	var bConstantB = true; 
	var beConstant1 = be.createConstantB(1);
	bConstantB &= sTest(be.valueNow(beConstant1), 1, 'constant_b');
	var bIfB = true;
	
	var beIfReceiver = be.createConstantB(true);
	var beIfThenReceiver = be.createConstantB(1);
	var beIfElseReceiver = be.createConstantB(2);
	var beIf1 = be.if_b(
		 beIfReceiver, beIfThenReceiver, beIfElseReceiver); 
    bIfB &= sTest(be.valueNow(beIf1), 1, 'if_b1');
	bIfB &= sTest( (function () {
		be.sendBehaviour(beIfElseReceiver, 7);
		return be.valueNow(beIf1);})(), 
		1, 'if_b2');
	bIfB &= sTest( (function () {
		be.sendBehaviour(beIfThenReceiver, 11);
		return be.valueNow(beIf1); })(), 
		11, 'if_b3');
	bIfB &= sTest( (function () {
		be.sendBehaviour(beIfReceiver, false);
		return be.valueNow(beIf1);})(),
		7, 'if_b4');
	bIfB &= sTest( (function () {
		be.sendBehaviour(beIfElseReceiver, 13);
		return be.valueNow(beIf1);})(),
		13, 'if_b5');
	bIfB &= sTest( (function () {
		be.sendBehaviour(beIfReceiver, true);
		return be.valueNow(beIf1);})(),
		11, 'if_b6');

	a(bIfB, 'if_b');
	
	var bCondB = true;
	
	bCondB &= sTest( 
		be.valueNow(be.cond_b([])), 
		undefined,
		'cond_b1');

	bCondB &= sTest(
		be.valueNow(
			be.cond_b(
				[be.createConstantB(true), 1],
				[be.createConstantB(true), 2])),
		1, 'cond_b2');
		
	bCondB &= sTest(
		be.valueNow(
			be.cond_b(
				[be.createConstantB(false), 1],
				[be.createConstantB(true), 2])),
		2, 'cond_b3');
		
	bCondB &= sTest(
		be.valueNow(
			be.cond_b(
				[be.createConstantB(true), 1],
				[be.createConstantB(false), 2])),
		1, 'cond_b4');
		
	bCondB &= sTest(
		be.valueNow(
			be.cond_b(
				[be.createConstantB(false), 1],
				[be.createConstantB(false), 2])),
		undefined, 'cond_b5');
		
	bCondB &= sTest(
		be.valueNow(
			be.cond_b(
				[be.createConstantB(true), 
				 be.createConstantB(1)])),
		1, 'cond_b6');

	bCondB &= sTest( (function () {
			var receiverE = c.createEventReceiver();
			var condb = 
				be.cond_b(
					[be.hold(receiverE, false), 1]);
			c.sendEvent(receiverE, true);
			return be.valueNow(condb);
		})(),
		1, 'cond_b7');

	bCondB &= sTest( (function () {
			var receiverE = c.createEventReceiver();
			var condb = 
				be.cond_b(
					[be.hold(receiverE, false), 1]);
			c.sendEvent(receiverE, false);
			return be.valueNow(condb);
		})(),
		undefined, 'cond_b8');
		
	bCondB &= sTest( (function () {
			var receiverE = c.createEventReceiver();
			var condb = 
				be.cond_b(
					[true, 1],
					[be.hold(receiverE, false), 2]);
			c.sendEvent(receiverE, true);
			return be.valueNow(condb);
		})(),
		1, 'cond_b9');


	a(bCondB, 'cond_b');

	var bLiftB = true;
	var liftBConstF = function () { return 2; };
	bLiftB &= sTest(be.valueNow(be.lift_b(liftBConstF)),
		2, 'lift_b1');	

	var liftB1F = function (x) { return 1 + x; };
	var liftBa1 = be.createConstantB(3);
	var liftedB2 = be.lift_b(liftB1F, liftBa1);
	bLiftB &= sTest(be.valueNow(liftedB2), 4, 'lift_b2');
	bLiftB &= sTest( (function () {
		be.sendBehaviour(liftBa1, 5);
		return be.valueNow(liftedB2);
		})(), 6, 'lift_b3');
	var liftB2F = function (x, y) { return x + y; };
	var liftBa2 = be.createConstantB(3);
	var liftedB3 = be.lift_b(liftB2F, liftBa1, liftBa2);
	bLiftB &= sTest(be.valueNow(liftedB3), 8, 'lift_b4');
	bLiftB &= sTest( (function () {
		be.sendBehaviour(liftBa2, 7);
		return be.valueNow(liftedB3);
		})(), 12, 'lift_b4');
	bLiftB &= sTest( (function () {
		be.sendBehaviour(liftBa1, 11);
		return be.valueNow(liftedB3);
		})(), 18, 'lift_b5');
    bLiftB &= sTest( (function () {
        var hit = 0;
        var r = c.createEventReceiver();
        var fnB = be.hold(r, function (x) { hit = x; return x; });
        be.lift_b(fnB, 5);
        return hit;
        })(), 5, 'lift_b6');
   bLiftB &= sTest( (function () {
        var hit = 0;
        var r = c.createEventReceiver();
        var fnB = be.hold(r, function (x) { hit = x; return x; });
        be.lift_b(fnB, 5);
        c.sendEvent(r, function (y) { hit = y * 2; return y * 2; });
        return hit;
        })(), 10, 'lift_b7');
	a(bLiftB, 'lift_b');

	var bAndB = true;	
	var andB0 = be.and_b();
	bAndB &= sTest(be.valueNow(andB0), true, 'and_b1');
	var andBa1 = be.createConstantB( true);
	var andBa2 = be.createConstantB( false);
	var andB11 = be.and_b( andBa1);
	var andB12 = be.and_b( andBa2);
	var andB2 = be.and_b( andBa1, andBa2);
	bAndB &= sTest(be.valueNow(andB11), true, 'and_b2');
	bAndB &= sTest(be.valueNow(andB12), false, 'and_b3');
	bAndB &= sTest(be.valueNow(andB2), false, 'and_b4');
	bAndB &= sTest( (function () {
		be.sendBehaviour(andBa2, true);
		return be.valueNow(andB12);
		})(), true, 'and_b5');
	bAndB &= sTest(be.valueNow(andB2), true, 'and_b6');
	bAndB &= sTest( (function () {
		be.sendBehaviour(andBa1, false);
		return be.valueNow(andB11);
		})(), false, 'and_b6');
	bAndB &= sTest(be.valueNow(andB2), false, 'and_b7');

	a(bAndB, 'and_b');


	var bSwitchB = true;

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				return be.valueNow(switchedB);
			})(),
			0,
			'switch_b1');

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				return be.valueNow(switchedB);
			})(),
			1,
			'switch_b2');

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				c.sendEvent(stream1E, 2);
				return be.valueNow(switchedB);
			})(),
			2,
			'switch_b3');

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				c.sendEvent(streamReceiverE, stream2B);
				return be.valueNow(switchedB);
			})(),
			2,
			'switch_b4');

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				c.sendEvent(streamReceiverE, stream2B);
				c.sendEvent(stream1E, 1);
				return be.valueNow(switchedB);
			})(),
			2,
			'switch_b5');
			
	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				c.sendEvent(streamReceiverE, stream2B);
				c.sendEvent(stream2E, 3);
				return be.valueNow(switchedB);
			})(),
			3,
			'switch_b6');
			

	bSwitchE &=
		sTest(
			(function () {
				var stream1E = c.createEventReceiver();
				var stream1B = be.hold( stream1E, 1);
				var stream2E = c.createEventReceiver();
				var stream2B = be.hold( stream2E, 2);
				var streamReceiverE = c.createEventReceiver();
				var streamReceiverB = be.hold( streamReceiverE, be.createConstantB( 0));
				var switchedB = be.switch_b( streamReceiverB);				
				c.sendEvent(streamReceiverE, stream1B);
				c.sendEvent(streamReceiverE, stream2B);
				c.sendEvent(stream1E, 1);
				return be.valueNow(switchedB);
			})(),
			2,
			'switch_b1');

	a(bSwitchB, 'switch_b');
	//TODO test or_b
	
	bBehaviour &= bMapB && bConstantB && bIfB && bCondB && bLiftB && bAndB;
		(bBehaviour? info : error)('====flapjax.behaviour====');

	return true;
 }



