<html>
<head>
<link rel="stylesheet" href="/demo.css"/>

<title>Flapjax Demo: Delicious Bookmarks (FilterRepeats &amp; Calm)</title>

<script type="text/flapjax">

var linksInfoE = evalForeignScriptValE(
  extractValueE('userName').    		
  filterE(function(str){return str.length > 0;}).
  calmE(450).mapE(function (name) {
    return {
      url: 'http://del.icio.us/feeds/json/' + name,
      globalArg: 'Delicious' }}));
  		
var linksInfoB = linksInfoE.startsWith({ posts: [ ] });
	
var postToLI = function (post) {
  return LI(A({href: post.u}, post.d))};

  		
insertDomB(UL(map(postToLI,linksInfoB.posts)), "userLinks");
</script>
</head>
<body>

<div style="text-align: center">

<span style="padding: 2px">
<a id="mylink" href="http://del.icio.us">del.icio.us</a>
</span> 

user name: <input type="text" id="userName"/><br/><br/>
<ul id="userLinks">delicious user links go here</ul>

</div>

</body>
</html>
