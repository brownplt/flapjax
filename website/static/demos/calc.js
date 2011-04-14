// currentB : EventStream 'a * 'a -> Behavior 'a
// A behavior carrying the value of the event stream, before it is updated.
function nowB(evt, init) {
  return evt
    .collectE({ prev: null, curr: init },
	      function(curr, acc) {
		return { prev: acc.curr, curr: curr };
	      })
    .mapE(function(acc) { return acc.prev; })
    .startsWith(init);
}

// extractEventsE : Array<HTMLElement> * String -> EventStream HTMLEvent
function extractEventsE(elts, eventName) {
  var streams = 
    map(function(elt) { return extractEventE(elt, eventName); }, elts);
  return mergeE.apply(null, streams);
}

// operators : { ".*" : Num * Num -> Num }
var operators = {
  "+": function(x, y) { return x + y; },
  "-": function(x, y) { return x - y; },
  "*": function(x, y) { return x * y; },
  "/": function(x, y) { return x / y; },
  "=": function(acc, r) { return acc; }
};

// start : -> Undef
function start() {

  var digitE = 
    extractEventsE(document.getElementsByClassName("digit"), "click")
    .mapE(function(evt) { return Number(evt.currentTarget.value); });

  var nextOperatorE =
    extractEventsE(document.getElementsByClassName("operator"), "click")
    .mapE(function(evt) { return operators[evt.currentTarget.value]; });
  
  var eventsE = mergeE(digitE, nextOperatorE);

  var nextRhsE = 
    eventsE
    .collectE(0, function(evt, acc) {
		return typeof evt === "number" ? 10 * acc + evt : 0;
	      });

  var rhs = nowB(nextRhsE, 0);

  var op = nowB(nextOperatorE, operators["+"]);

  var nextAccE =
    op
    .changes()
    // This is a stupid way to snapshot both.
    .snapshotE(liftB(function(op, rhs) { return { op: op, rhs: rhs }; },
		     op, rhs))
    .collectE(0, function(tuple, prevAcc) {
		return tuple.op(prevAcc, tuple.rhs);
	      });
   
  
  var acc = nowB(nextAccE, 0);

  var display = mergeE(nextAccE, nextRhsE).startsWith(0);

  // innerText does not work in Firefox 4
  insertValueB(display, document.getElementById("display"), "innerHTML");

}

window.addEventListener("load", start, false);
