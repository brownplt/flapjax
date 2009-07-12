<html>
<head>
<title>Compositional UI demo</title>

<script type="text/javascript">
var mapKeys = function(f,obj) {
  var arr = [ ];
  for (var ix in obj) {
    if (Object.prototype && Object.prototype[ix]) continue;
    arr.push(f(ix,obj[ix]));
  }
  return arr;
};


</script>
<script type="text/flapjax">
var formatCandidate = function(c) {
  return P(c.name + " scored " + c.score.toString() + " and is " + c.gender);
};

var makeCandidatesDOM = function(cs) { 
  return DIV(map(formatCandidate,cs));
};

var candidates =
  [ { name: 'Frederick Algernon Trotteville', score: 1, gender: 'Male' },
    { name: 'Lawrence Daykin', score: 3, gender: 'Male' },
    { name: 'Margaret Daykin', score: 6, gender: 'Female' },
    { name: 'Philip Hilton', score: 2, gender: 'Male' },
    { name: 'Elizabeth Hilton', score: 4, gender: 'Female' } ]


var pickGender = function() {
  var ui = SELECT(
    OPTION({ value: 'Female' }, "Female"),
    OPTION({ value: 'Male' }, "Male"));
  
  return {
    dom: ui,
    pred: function(person) {
      return person.gender == $B(ui);}}};

var pickScore = function() {
  var ui = INPUT({ type: 'text', size: 5 });
  
  return {
    dom: ui,
    pred: function(person) {
      return person.score == parseInt($B(ui));}};};

var filters = {
  'Gender': pickGender,
  'Score': pickScore 
};

var pickFilter = function() {
  var options = mapKeys(function(k,v) {
    return OPTION({ value: k }, k);},
    filters);
  var sel = SELECT(options);
  var subFilter = filters[$B(sel)]();

  return { 
    dom: SPAN(sel, " is ", subFilter.dom),
    pred: subFilter.pred };};


var filterButton = function(innerFilter) {
  z = innerFilter;
  var button = A({ href: "javascript: undefined" }, "Update");
  var filterB = innerFilter.pred;
  var filterNone = function(_) { return true; };

  return {
    dom: DIV(innerFilter.dom, button),
    pred: clicksE(button).snapshotE(filterB).startsWith(filterB.valueNow())
  };
};

var filterObj = filterButton(pickFilter());
var filteredCandidates = filter(filterObj.pred, candidates);
</script>
</head>

<body>

Filter by:{! filterObj.dom !}

<br>
<p>The candidates are:</p>
{! makeCandidatesDOM(filteredCandidates) !}

</body>

</html>

