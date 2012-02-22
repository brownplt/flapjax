"use strict";

goog.require('F');
goog.provide('F.dom');


/**
 * @param {Object} obj
 * @param {string} prop
 * @returns {function(*):}
 */
F.dom.staticEnstyle_ = function(obj, prop) {
  return function(val) {
    if (val === F.X) {
      delete obj[prop];
    }
    else {
      obj[prop] = val;
    }
  };
};

F.dom.enstyle_ = function(target, obj) {
  Object.keys(obj).forEach(function(key) {
    var val = obj[key];
    if (val instanceof F.Node) {
      F.app(F.dom.staticEnstyle_(target, key), val);
    }
    else if (typeof val === 'object') {
      F.dom.enstyle_(target[key], val);
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
 */
F.dom.insertAfter_ = function(parent, newChild, refChild) {
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
F.dom.swapChildren_ = function(parent, existingChildren, newChildren) {
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
      F.dom.insertAfter_(parent, newChildren[i], newChildren[i - 1]);
    }
  }
};

F.dom.appendDynChild_ = function(parent, child) {
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
      F.dom.swapChildren_(parent, lastVal, childV);
    }
    lastVal = childV;
  }, child);
};

F.dom.appendChild_ = function(parent, child) {
  if (child instanceof Array) {
    child.forEach(function(ch) { F.dom.appendChild_(parent, ch); });
  }
  else if (child instanceof F.Node) {
    F.dom.appendDynChild_(parent, child);
  }
  else {
    parent.appendChild(child);
  }
};

/**
 * @param {string} tagName
 * @param {Object} attribs
 * @param {Array.<F.Node|Node>} children
 * @returns {HTMLElement}
 */
F.dom.elt = function(tagName, attribs, var_args /* ... */) {
  var children = Array.prototype.slice.call(arguments, 2);
  var elt = document.createElement(tagName);
  F.dom.enstyle_(elt, attribs);
  children.forEach(function(child) {
    F.dom.appendChild_(elt, child);
  });
  return elt;
}

/**
 * @param {F.Node|string} text text to display
 * @returns {TextNode}
 */
F.dom.text = function(text) {
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
}
