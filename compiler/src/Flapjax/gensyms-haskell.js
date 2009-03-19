var document = { cookie: '' } // dummy object, necessary to load fserver.js
var window = { location: { href: '' } } // dummy object, necessary to load flapjax.js

function setTimeout() { // Added by Arjun
  return 0;
}

load(['/u/arjun/projects/flapjax/proto/flapjax.js']);
load(['/u/arjun/projects/flapjax/proto/fserver.js']);
var f = flapjaxInit();
initFlapjaxServerAPIs(f, 'http://dummy.url/');

var compilerFuns = ['mixedInsertDom_eb', 'mixedInsertValue_eb'];

var noContext   = [].concat(compilerFuns);
var withContext = [];

var evNoContext   = [];
var evWithContext = [];
var beNoContext   = [];
var beWithContext = [];

var coreFuns = [];

var noLiftFuns = [];
var noLiftMethods = [];
function addNoLift(t) {
        if (t[0].e === true || t[0].b === true) {
            noLiftMethods.push(t[1]);
        }
        if (t[0].np !== true) {
            noLiftFuns.push(t[1]);
        }
}

var flapjaxDefs = $___defs.concat($___sdefs); // all flapjax definitions

// load up general lists
forEach(function (t) {
    if (t[0].np !== true) { 
        (t[0].c ? withContext : noContext).push(t[1]); 
    }

    if (t[0].e === true) {
	(t[0].c ? evWithContext : evNoContext).push(t[1]);
    }

    if (t[0].b === true) {
        (t[0].c ? beWithContext : evNoContext).push(t[1]);
    }
}, flapjaxDefs);

forEach(function (t) {
    // get a list of core flapjax functions
    if (t[0].np !== true) { coreFuns.push(t[1]); }

    // check flapjax defs
    if (t[0].ul === true) { addNoLift(t); }          
}, $___defs);

// every fserver def is nolift
forEach(function (t) {
    if (t[0].np !== true) { addNoLift(t); }
}, $___sdefs);

function arrayToString(a) {
    var s = '';

    for (var i = 0;i < a.length - 1;i++) {
        s += '\"' + a[i] + "\",";
    }

    if (a.length != 0) {
        s += '\"' + a[i] + '\"';
    }

    return s;
}

function arrayToSchemeList(a) {
    return "[" + arrayToString(a) + "]";
}

function schemeGeneralSymbolList(type, ls) {
    print("flapjax" + type + " = " + arrayToSchemeList(ls) + "\n");
}

function schemeSymbolList(type, noContext, withContext) {
    print("flapjaxNocontext" + type + " = " + arrayToSchemeList(noContext) + "\n");
    print("flapjaxContext" + type + " = " + arrayToSchemeList(withContext) + "\n");
    schemeGeneralSymbolList(type, noContext.concat(withContext));
}

// create the Scheme module
print("-- Generated code; edit with care.\n");
print("module Flapjax.Symbols where\n\n")
schemeSymbolList("Funs", noContext, withContext);
schemeSymbolList("EventMethods", evNoContext, evWithContext);
schemeSymbolList("BehaviorMethods", beNoContext, beWithContext);
schemeSymbolList("Methods", evNoContext.concat(beNoContext), evWithContext.concat(beWithContext));
schemeGeneralSymbolList("CompilerFuns", compilerFuns);
schemeGeneralSymbolList("CoreFuns", coreFuns.concat(compilerFuns));
schemeGeneralSymbolList("NoliftFuns", noLiftFuns);
schemeGeneralSymbolList("NoliftMethods", noLiftMethods);

