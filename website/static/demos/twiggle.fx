<html>

<head>


<link rel="stylesheet" href="/demo.css"/>
<script type="text/javascript" src="/fx/flapjax.js"></script>

<!-- Contains getForeignWebServiceObject_e -->
<script type="text/javascript" src="/fx/fxws.js"></script>

<!-- Key for http://fxinternal.cs.brown.edu/ -->
<script type="text/javascript" src="http://www.google.com/jsapi?key=ABQIAAAARESS6jJKYBfaMZPBBTSULRTJ5VdHXjH0QrfBukiiaTWNKqxyThSqgiHvz1GPkYd7riA99s6j9OWkhQ"></script>
    
<title>Yaggle 2.0</title>
		<style>
			#main {	float:left; width:50%; }
			#sidebar { float:right; width:50%; }
		</style>

<script type="text/javascript">
google.load("maps", "2");
</script>

<script type="text/flapjax">


var efscArg;
// makeGoogleGeocoder :: EventStream {data: a, location: string } 
//                    -> EventStream { data: a, point: point or false }
// 'location' is a string and point is suitably formatted for pinning on the
// map.  The 'data' field is passed through.
function makeGoogleGeocoderE(requestsE) {
  var geocoder = new google.maps.ClientGeocoder();
  var resultsE = receiver_e();

  var getResult = function(data) { return function(point) {
    resultsE.sendEvent({ data: data, point: point })}};
  
  requestsE.lift_e(function(req) {
    if (req && typeof(req.location) == "string") {
      geocoder.getLatLng(req.location,getResult(req.data));
    }
    else {
      getResult(req.data)(false); }});

  return resultsE; };

// getTwitterPublicTimelineE :: -> EventStream Tweet
// where Tweet is something that nobody truly understands.
function getTwitterPublicTimelineE() { 

  efsc = function(arg) { efscArg = arg; };

  // Running the script at the following URL will call the specified callback
  // with a list of tweets.  This is very insecure, but that's mashups!
	var twitterUrlArg = {
    url: 'http://twitter.com/statuses/public_timeline.json?callback=efsc',
		globalArg: 'efscArg'
	};

  // evalForeignScriptVal_e reifies the mess into an event stream.
	var twitterRequestE = 
   merge_e(one_e(twitterUrlArg),
           timer_e(60000).constant_e(twitterUrlArg));
  var twitterResponseE = evalForeignScriptVal_e(twitterRequestE);

  // twitterResponseE carries a list of tweets.  Transform it into an event
  // stream of individual tweets.
  var tweetE = receiver_e();
  return twitterResponseE.bind_e(function(tweets) {
    var tweetsEs = fold(function(tweet,acc) {
      acc.evts.push(one_e(tweet).delay_e(acc.delay));
      acc.delay = acc.delay + 2500; // just for fun, delay by 2.5s
      return acc;}, { evts: [], delay: 0 }, tweets ? tweets : []);
    return merge_e.apply(this,tweetsEs.evts);
  });};

// makeGoogleMapOverlayE :: GoogleMap (EventStream { point: point, data: html }
//                       -> EventStream Boolean
function makeGoogleMapOverlayE(googleMap,requestE) {
  requestE.lift_e(function(req) {
    var marker = new google.maps.Marker(req.point);
    googleMap.addOverlay(marker);
    marker.bindInfoWindowHtml(req.data);
    // someone should check for errors
    return true;});};

var googleMap = new google.maps.Map2(document.getElementById("map"));
googleMap.setCenter(new google.maps.LatLng(0, 0), 2);

var tweetE = getTwitterPublicTimelineE();

var pointsE = makeGoogleGeocoderE(tweetE.lift_e(function(tweet) {
  return { data: tweet.text, location: tweet.user.location };}));

// Avoid spurious errors if the point was not found.
makeGoogleMapOverlayE(googleMap,pointsE.filter_e(function(x) {
  return x.point; }));};

</script>
  </head>
  <body onunload="GUnload()">
    <h1>Yaggle 2.0</h1>
    <form>
      <div id="main">
        <div id="map" style="width: 1000px; height: 600px"></div>
      </div>
    </form>
  </body>
</html>
