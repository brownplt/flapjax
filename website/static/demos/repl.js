/*
 * Flapjax REPL
 *
 * Copyright (c) 2009, Arjun Guha.
 * All Rights Reserved.
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
 * * Neither the name of Brown University, the Flapjax Team, nor the names
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

// _eval evaluates strings in the current (i.e. global) lexical scope.
_eval = eval;

(function() {

var empty = {};


function cons(x, y) {
  return { first: x, rest: y } }


function first(lst) { 
  return lst.first }


function rest(lst) {
  return lst.rest }


function listToArray(lst) {
  var arr = [];
  while (lst != empty) {
    arr.push(lst.first);
    lst = lst.rest };
  return arr };


function firstN(n, lst) {
  if (lst == empty) { return lst }
  else if (n == 0) { return empty }
  else { return cons(lst.first, firstN(n - 1, lst.rest)) } };


function takeWhile(f, lst) {
  if (lst == empty) { return lst }
  else if (f(lst.first)) { return cons(lst.first, takeWhile(f, lst.rest)) }
  else { return empty } }


function mapPairs(f) { return function (lst) {
  if (lst == empty || lst.rest == empty) { return empty }
  else { return cons(f(lst.first, lst.rest.first), mapPairs(f)(lst.rest)) }}}


function theRealMap(f) { return function(lst) {
  if (lst == empty) { return empty }
  else { return (cons(f(lst.first), theRealMap(f)(lst.rest))) } }};


function tuple(x,y) { 
  return { fst : x, snd: y } }


function left(x) { return { left: x }}


function right(x) { return { right: x }}


function isRight(x) { return x.right != undefined }


function isAltEnterPressed(keyEvt) {
  return keyEvt.keyCode == 13 && keyEvt.altKey};


// We update our display at 10 millisecond intervals.  Every 10 milliseconds,
// we shift all values right, and sample event streams and behaviors.
var ticks = timerB(10);

function withinSecondsOf(now) { return function(x) {
  
  return now - x.fst <= (document.body.clientWidth * 7) }}


function renderBehavior(b) {
  
   var withTimes = liftB(tuple, ticks, b).changes()
                                         .collectE(empty, function(v, vals) {
    if (vals == empty || v.snd != vals.first.snd) {
      return takeWhile(withinSecondsOf(v.fst), cons(v, vals)) }
    else {
      return cons(tuple(v.fst, vals.first.snd), vals.rest) } });

   var withWidths = withTimes.mapE(mapPairs(function(next, prev) { 
    return tuple(Math.floor((next.fst - prev.fst) / 10), next.snd) }));


   var asSpans = withWidths.mapE(function(lst) {
    var left = 0;
    return theRealMap(function(p) {
      var myLeft = left;
      left = left + p.fst;
      return SPAN(SPAN({ style: { width: p.fst,
                                  border: "1px solid black",
                                  left: myLeft,
                                  margin: "0px",
                                  padding: "5px",
                                  position: "absolute",
                                  overflow: "hidden",
                                  backgroundColor: "pink" } }, 
                       p.snd))})
      (lst)});

  return asSpans.startsWith(empty).liftB(function(lst) {
    return DIV.apply(this, listToArray(lst)) }) }


function renderEvent(evt) {
  if (evt instanceof MouseEvent) {
    return IMG({ src: "/images/mouse.png", height: "50", width: "50" })}
  else if (evt instanceof EventStream) {
    return SPAN("EventStream");
  }
  else {
    return SPAN(toJSONString(evt)) }}


function renderEventStream(e) { 

  withBlanks = mergeE(ticks.changes().mapE(left), e.mapE(right));

  var withTimes = withBlanks.collectE(empty, function(v, vals) {
    if (v.right) {
      return cons(tuple(Date.now(), v.right), vals) }
    else {
      return takeWhile(withinSecondsOf(Date.now()), vals) }});

  var asSpans = withTimes.mapE(function(lst) {
    var now = Date.now();
    var i = 0;
    return theRealMap(function(v) { 
      var p = Math.floor((now - v.fst) / 10);

      return SPAN({ style: { margin: "0px", padding: "0px", 
                             position: "absolute", left: p, zIndex: i++ } },
                       renderEvent(v.snd)) })
      (lst)});


  var spans = asSpans.startsWith(empty).liftB(function(lst) {
    return DIV({ style: { position: "relative", height: "20px" } }, 
               DIV.apply(this, listToArray(lst))) });

  return spans }
 

function render(argE) {
  return argE.mapE(function(arg) { 
    if (arg instanceof Behavior) {
      return renderBehavior(arg, 5) }
    else if (arg instanceof EventStream) {
      return renderEventStream(arg) }
    if (typeof arg == "undefined") {
      return constantB(SPAN("")) }
    else if (arg.nodeType > 0) {
      return constantB(arg) }
    else {
      var str = toJSONString(arg);
      return constantB(SPAN({ style: { float: "left" } },
                             typeof str == "undefined"
                             ? "" : str));
    }
  })}


function compile(srcE) {
  return srcE.mapE(function(src) {
    if (src.doNotCompile) {
      return oneE(src.txt) }
    else { 
      return getWebServiceObjectE(oneE({
        url : "/fxserver/compile_expr",
        request: "rawPost",
        response: "plain",
        body:  src.txt,
        asynchronous: true }))}})};


function interact(compileB) {
  var input = DIV({ style: { minHeight: "24pt" }, contentEditable: true });
  var inputWrapper = DIV({ style: { minWidth: "90%", position: "absolute",
                                    display: "inline" }},
                         input);
  /* var input = DIV({ style: { minWidth: "90%", 
                             position: "absolute", 
                             display: "inline" }, 
                     contentEditable: true}, "");
  */
  
  var srcE = $E(input, "keydown").filterE(isAltEnterPressed)
             .snapshotE(compileB).mapE(function(doNotCompile) {
    input.contentEditable = false;
    return { doNotCompile: doNotCompile, txt: input.textContent } });


  var valE = compile(srcE).switchE().mapE(function(src) {
    if (src == "") { return "" }
    try {
      return _eval(src) }
    catch(e) {
      return SPAN({ style : { color: "red" } }, e.toString()) } });


  var outputE = render(valE);


  var nextE = srcE.mapE(function() { return interact(compileB) });
  
  var dom = DIV(
    // without this DIV, the prompt occurs on the previous line.
    DIV({ style: { color: "white" } }, "."), 
    BR(),
    DIV({ style: { fontSize: valE.constantE("14pt").startsWith("24pt"), 
                   position: "relative", 
                   width: "100%",  } },
        "> ", inputWrapper),
    outputE.startsWith(constantB("")).switchB(),
    nextE.startsWith(""));

  oneE(true).mapE(function() { input.focus() });

  return dom };

function startInteractions() {
  var compile = INPUT({ type: "checkbox"});
  var options = DIV({ style: { position: "fixed", 
                               top: "0px", 
                               right: "0px",
                               border: "1px solid black",
                               padding: "5px",
                               margin: "5px",
                               background: "white",
                               zIndex: 100,
                               fontFamily: "Verdana, sans-serif"
                             } }, 
                    DIV("Press Alt+Enter to execute."),
                    compile, "Skip Compiler. (Use Flapjax as a Library.)");
  
  document.body.appendChild(
    DIV({ style: { color: "#333333",
                   height: "100%",
                   fontFamily: "Bitstream Vera Sans Mono, Courier" } },
        options,
        interact($B(compile)))); }

window.addEventListener("load", startInteractions, true);

})();
