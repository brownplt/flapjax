<html>
<head>
<title>Catch Up</title>
<link rel="stylesheet" href="/demo.css"/>

<script type="text/javascript">
var toInt = function (v, def) { 
  try { 
    return parseInt(v); } 
  catch (_) { 
    return def; }};
</script>
<script type="text/flapjax">

var DIAM1 = 100;
var DIAM2 = 220;
var OFFSET2 = 23;
var SPEED = 1000; //slows when higher - irrespective of framerate


var timeB = timerB(toInt(extractValueB('frameRate'),50));

var delayTimeB = toInt(extractValueB('delayRate'),500);

//CATCH box    
insertDomB( 
  DIV(
    {id: 'catch',
     style: {backgroundColor: '#000000',
         color: '#FFFFFF',
         fontSize: '3em',
         position: 'absolute',
         padding: '5px',
         left: 
           mouseLeftB(document) + Math.round(DIAM1 * Math.cos(timeB / SPEED)),
         top: 
           mouseTopB(document) + Math.round(DIAM1 * Math.sin(timeB / SPEED))
         }},
     'catch'),
  'body', 'beginning');
  
  //UP box, changes color on mouseover
  var upB =
    tagRec( 
      ['mouseover', 'mouseout'],
      function (mouseOverE, mouseOutE) {
        return DIV(
        {id: 'up',
         style: {
          padding: '5px',
          backgroundColor:
            mergeE(
              mouseOverE.constantE('#FF0000'),
              mouseOutE.constantE('#000')).startsWith('#000'),
           color: '#FFFFFF',
           fontSize: '3em',
           position: 'absolute',
           left: 
             OFFSET2 + mouseLeftB(document).delayB(delayTimeB) + Math.round(DIAM2 * Math.cos(timeB / SPEED)),
            top: 
              mouseTopB(document).delayB(delayTimeB) + 
              Math.round(DIAM2 * Math.sin(timeB / SPEED)) }},
         'up')});
    
  insertDomB( upB, 'catch', 'before');
  
// Count the number of times we catch up.
var caughtUpB = 
  extractEventE(upB, 'mouseover').collectE(0,
    function (_,p) { return p + 1;}).startsWith(0);

</script>
</head>

<body id="body" style="text-align: center; margin: 0px; overflow: hidden;">

<div class="block">
<div>
update every <input type="text" id="frameRate" value="10" size="5"/> ms 
with a delay of <input type="text" id="delayRate" value="800" size="5"/> ms
</div>

<div>
<h1>You caught up
<span style="color: white; background-color: black">
{! caughtUpB.toString() !}</span>
&nbsp;times</h1>
hit up with your mouse
</div>
</div>

</body>
</html>
