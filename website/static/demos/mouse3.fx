<html>
<head>


<link rel="stylesheet" href="/demo.css"/>
<title>Flapjax Demo: Follow the Mouse with a Jittering Tail</title>

<script type="text/flapjax">
var wag = function() {
  return timerE(100).mapE(function() { 
    return Math.round(10 * Math.random() - 5) }).startsWith(0); };
</script>
</head>

<body>
<div id="mouse" class="contrastBlock"
     style={! { position: "absolute",
                left: mouseLeftB(document),
                top: mouseTopB(document) } !}>
the mouse!
</div>
<div id="tail" class="contrastBlock"
     style={! { color: "#ff0000", position: "absolute",
                left: delayB(mouseLeftB(document) + $("mouse").offsetWidth,300),
                top: delayB(mouseTopB(document),300) } !}>
it&apos;s tail!
</div>
<div class="contrastBlock"
     style={! { color: "#ffff00", position: "absolute",
                left: delayB(mouseLeftB(document) + 
                             $("mouse").offsetWidth + 
                             $("tail").offsetWidth,300),
                top: delayB(mouseTopB(document),450) + wag() } !}>
is happy!
</div>
</body>

</html>
