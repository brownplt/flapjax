/**
 * @param {Object} obj
 * @param {string} prop
 * @returns {function(*):}
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
}

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
}

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
