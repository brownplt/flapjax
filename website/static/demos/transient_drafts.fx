<html>
<head>


<link rel="stylesheet" href="/demo.css"/>

<title>Flapjax Demo: Basic Draft Saving</title>

<script lang="text/flapjax">
var saveClickE = clicksE('saveButton');

var savedTimeE = saveClickE.mapE(function (_) {
  var date = new Date();
  return date.toLocaleString();});

var draftTextE = saveClickE.snapshotE($B('theText'));
</script>
</head>

<body>

<p>Click 'Save Now' to save the draft.  The draft is stored locally, and is not sent to the server.
Last Saved: <span id='savedTime'>{! savedTimeE.startsWith("") !}</span>
</p>

<form>
<textarea id='theText'></textarea> <br/>
<input type='button' id='saveButton' value='Save Now'/>
</form>
    
<p>The currently saved draft is:</p>

<div>
<span class="block" id='curDraft'>{! draftTextE.startsWith("") !}</span>
</div>

</body>
</html>
