<html>
<head>

<link rel="stylesheet" href="/demo.css"/>
<title>Flapjax Demo: Follow the Mouse with a tail</title>
</head>

<body>
<div id="mouse" class="contrastBlock"
     style={! { position: "absolute",
                left: mouseLeftB(document),
                top: mouseTopB(document) } !}>
the mouse!
</div>
<div class="contrastBlock"
     style={! { color: "#ff0000", position: "absolute",
                left: delayB(mouseLeftB(document) + $("mouse").offsetWidth,300),
                top: delayB(mouseTopB(document),300) } !}>
it&apos;s tail!
</div>
</body>

</html>
