/*
	fvlogger v1.0
	(c) 2005 davidfmiller
	http://www.fivevoltlogic.com/code/fvlogger/
	see readme.txt for documentation
*/

// version number
var FVLOGGER_VERSION = '1.0';

// turn logging on or off;
var FVL_LOG_ON = true;

// all logging statements that whose level is greater than or equal to FVL_DEFAULT_LOG_LEVEL will be processed;
// all others will be ignored
var FVL_DEFAULT_LOG_LEVEL = FVL_DEBUG;

// the id of the node that will have the logging statements appended to it
var FVL_LOG_ID = 'fvlogger';

// the element that should be wrapped around the log messages
var FVL_LOG_ELEMENT = 'p';

/* the code that follows is */

// constants for logging levels
var FVL_DEBUG = 0;
var FVL_INFO = 1;
var FVL_WARN = 2;
var FVL_ERROR = 3;
var FVL_FATAL = 4;

// the css classes that will be applied to the logging elements
var FVL_LOG_CLASSES = new Array("debug","info","warn","error","fatal");

/* */

// retrieves the element whose id is equal to FVL_LOG_ID
function getLogger(id) {
	if (arguments.length == 0) { id = FVL_LOG_ID; }
	return document.getElementById(id);
}

function showDebug() { FVL_showMessages(FVL_DEBUG); }
function showInfo() { FVL_showMessages(FVL_INFO); }
function showWarn() { FVL_showMessages(FVL_WARN); }
function showError() { FVL_showMessages(FVL_ERROR); }
function showFatal() { FVL_showMessages(FVL_FATAL); }
function showAll() { FVL_showMessages(); }

// removes all logging information from the logging element
function eraseLog(ask) {
	var debug = getLogger();
	if (! debug) { return false; }

	if (ask && ! confirm("Are you sure you wish to erase the log?")) {
		return false;
	}

	var ps = debug.getElementsByTagName(FVL_LOG_ELEMENT);
	var length = ps.length;
	for (var i = 0; i < length; i++) { debug.removeChild(ps[length - i - 1]); }
	return true;
}

function debug(message) { FVL_log("" + message, FVL_DEBUG); }
function warn(message) { FVL_log("" + message, FVL_WARN); }
function info(message) { FVL_log("" + message, FVL_INFO); }
function error(message) { FVL_log("" + message, FVL_ERROR);}
//function fatal(message) { FVL_log("" + message, FVL_FATAL);}

function windowError(message, url, line) {
	FVL_log('Error on line ' + line + ' of document ' + url + ': ' + message, FVL_FATAL);
	return true; //
}

// only override the window's error handler if we logging is turned on
if (FVL_LOG_ON) {
	window.onerror = windowError;
}

// 
function FVL_showMessages(level, hideOthers) {

//	alert('showing ' + level);

	var showAll = false;
	// if no level has been specified, use the default
	if (arguments.length == 0) { level = FVL_DEFAULT_LOG_LEVEL; showAll = true; }
	if (arguments.length < 2) { hideOthers = true; }

	// retrieve the element and current statements
	var debug = getLogger();
	if (! debug) { return false; }
	var ps = debug.getElementsByTagName("p");
	if (ps.length == 0) { return true; }

	// get the number of nodes in the list
	var l = ps.length; 

	// get the class name for the specified level
	var lookup = FVL_LOG_CLASSES[level]; 

	// loop through all logging statements/<p> elements...
	for (var i = l - 1; i >= 0; i--) {

		// hide all elements by default, if specified
		if (hideOthers) { hide(ps[i]); } 

		// get the class name for this <p>
		var c = getNodeClass(ps[i]);
//		alert(c);
//		alert("Node #" + i + "'s class is:" + c);
		if (c && c.indexOf(lookup) > -1 || showAll) { show(ps[i]); } 
	}
}

// appends a statement to the logging element if the threshold level is exceeded
function FVL_log(message, level) {

	// check to make sure logging is turned on
	if (! FVL_LOG_ON) { return false; } 

	// retrieve the infrastructure
	if (arguments.length == 1) { level = FVL_INFO;}
	if (level < FVL_DEFAULT_LOG_LEVEL) { return false; }
	var div = getLogger();
	if (! div) { return false; }

	// append the statement
	var p = document.createElement(FVL_LOG_ELEMENT);

	// this is a hack work around a bug in ie
	if (p.getAttributeNode("class")) {
		for (var i = 0; i < p.attributes.length; i++) {
			if (p.attributes[i].name.toUpperCase() == 'CLASS') {
				p.attributes[i].value = FVL_LOG_CLASSES[level];
			}
		}
	} else {
		p.setAttribute("class", FVL_LOG_CLASSES[level]);
	}
	var text = document.createTextNode(message);
	p.appendChild(text);
	div.appendChild(p);
	return true;
}

function exampleLogs() {
	// describe the four types of logging messages
	debug('Scatter debug messages throughout your code to provide a high-level overview of what your code is doing, such as which function is currently in scope and the values of loop counters.');
	info('Information messages are the meat and potatoes of logging messages; sprinkle them around to reveal more detailed information about your script\'s execution, such as the values of variables and function/method return values.');
	warn('Warning messages are used to indicate potentially hazardous situations, such as missing function arguments...');
	error('While error messages are used to indicate that something bad is about to happen; note that these kinds of errors are considered to be run-time errors, which are a different type of beast from the parse errors mentioned below.');

// generate an error to demonstrate the fatal error in ie and mozilla browsers
	a 
}

// show a node
function show(target) {
	target.style.display = "";
	return true;
}

// hide a node
function hide(target) {
	target.style.display = "none";
	return true;
}

// returns the class attribute of a node
function getNodeClass(obj) {
	var result = false;

	if (obj.getAttributeNode("class")) {
		result = obj.attributes.getNamedItem("class").value;
	}
	return result;
}