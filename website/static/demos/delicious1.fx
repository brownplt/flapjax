<html>
<head>
<link rel="stylesheet" href="/demo.css"/>

<title>Flapjax Demo: Delicious Bookmarks</title>
<script type="text/flapjax">

// EventStream { posts: [ { u: String, d: String }, ... ] }
var linksInfoE = evalForeignScriptValE(
  extractValueE('userName').mapE(function (name) {
    return { 'url': 'http://del.icio.us/feeds/json/' + name,
             'globalArg': 'Delicious' } }));

var linksInfoB = linksInfoE.startsWith({ posts: [] });

var postToLI = function (post) { 
  return LI(A({href: post.u}, post.d)) };

insertDomB(UL(map(postToLI,linksInfoB.posts)),"userLinks");
</script>
</head>

<body>

<div style="text-align: center">
  
<span style="padding: 2px">
<a id="mylink" href="http://del.icio.us">del.icio.us</a>
</span> 

user name: <input type="text" id="userName"/><br/><br/>

<ul id="userLinks">If javascript was on, delicious user links would go here</ul>
  
</div>
</body>
</html>

