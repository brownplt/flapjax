goog.require('goog.testing.jsunit');
goog.require('F');

function testDom1() {
  var elt = F.elt('p', { }, document.createTextNode('hello, world'));
  assertEquals('hello, world', elt.innerText);

}

function testDom2() {
  var t = F.receiver('hello, world');
  var elt = F.elt('p', { }, F.text(t));
  assertEquals('hello, world', elt.innerText);
  t.send('goodbye');
  assertEquals('goodbye', elt.innerText);

}
function testDom3() {
  var t = F.receiver([F.text('A'), F.text('B')]);
  var elt = F.elt('p', { }, t);
  assertEquals('AB', elt.innerText);
  t.send([F.text('hello world')]);
  assertEquals('hello world', elt.innerText);
  t.send([F.text('C'), F.text('D')]);
  assertEquals('CD', elt.innerText);

}

function testDom3() {
  var t = F.receiver([F.text('A'), F.text('B')]);
  var elt = F.elt('p', { }, F.text(F.receiver('__')), t,
                      F.text('$$'));
  assertEquals('__AB$$', elt.innerText);
  t.send([F.text('hello world')]);
  assertEquals('__hello world$$', elt.innerText);
  t.send([F.text('C'), F.text('D')]);
  assertEquals('__CD$$', elt.innerText);

}

function testDomAttrib1() {
  var elt = F.elt('div', { style: { fontSize: '16pt' } }, 
                      F.text('hello world'));
  assertEquals('16pt', elt.style.fontSize);
}

function testDomAttrib2() {
  var fs = F.receiver('16pt');
  var elt = F.elt('div', { style: { fontSize: fs } }, 
                      F.text('hello world'));
  assertEquals('16pt', elt.style.fontSize);
  fs.send('18pt');
  assertEquals('18pt', elt.style.fontSize);
}
