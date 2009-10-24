
var username = "";
var password = "";

var host = "twitter.com";
var host = "localhost"; // for emergency
 
function mkTimelineRequest(since) {
    var fields = since == 0 ? { } : { "since_id" : since };
    return {
      url: "/redirect/" + username + ":" + password + 
           "@" + host + "/statuses/friends_timeline.json",
      request: "get",
      response: "json",
      fields: fields
    }
}

function mkUpdateRequest(status) {
    return {
      url: "/redirect/" + username + ":" + password +
           "@" + host + "/statuses/update.json",
      request: "rest",
      response: "json",
      fields: { "status": status }
    }
}

// timelineMaxId :: Timeline -> Int
function timelineMaxId(timeline) {
  if (typeof timeline[0] == "undefined") {
    return 0;
  }
  else {
    return timeline[0].id || 0;
  }
}

function formatTweet(tweet) {
  return DIV({ className: "msg"}, B(tweet.user.name, " says "), tweet.text);
}


function getTweetsSince(whenE, sinceB) {
  var reqE = whenE.snapshotE(sinceB)
                  .mapE(mkTimelineRequest);
  return getWebServiceObjectE(reqE);
}

function getTweets(whenE) {
  return recE(function(resultE) {
    return getTweetsSince(
      whenE, 
      resultE.collectE(0, function(timeline, lastMaxId) {
        return timelineMaxId(timeline) || lastMaxId }).startsWith(0));
  });
}


// formatTweets : EventStream Timeline -> Behavior Element
function formatTweets(tweetsE) {

    var divsE = tweetsE.collectE(DIV(), function(timeline, prev) {

        var d = DIV({ className: "tweetBlock" }, map(formatTweet, timeline));
                    d.style.opacity = 1;
        return DIV(d, prev);
  });

    return DIV({ className: "tweets" }, divsE.startsWith(DIV("Loading")));
}

// auth :: -> EventStream({ username: String, password: String })
function auth() {
  var usernameB = $B("username");
  var passwordB = $B("password");
  var credentialsB = liftB(function(username, password) {
    return { username: username, password: password } },
    usernameB, passwordB);

  var loginE = clicksE("loginButton");
  var logoutE = clicksE("logoutButton");
  insertValueB(mergeE(loginE.constantE("none"),
                      logoutE.constantE("block"))
               .startsWith("block"),
               "loginPanel", "style", "display");
  return loginE.snapshotE(credentialsB).mapE(function(cred) {
    username = cred.username;
    password = cred.password;
  });
};

var hist = [];

function start() {

  var authE = auth();

  var refreshE = mergeE(authE, timerE(30000)).filterE(function(_) {
    return username !== "" && password !== "" });

  insertDomB(formatTweets(getTweets(refreshE)), "tweets");

  var entryB = $B("newstatus");

  var entryLengthB = entryB.liftB(function(txt) { return txt.length });

  var entryColorB = entryLengthB.liftB(function(len) {
    if (len > 140) { return "red" }
    else if (len > 100) { return "orange" }
    else { return "green" } });

  var lengthElt = 
    SPAN({ className: "length", style: { color: entryColorB } },
         entryLengthB);
        
  insertDomB(lengthElt, "length");

  var cannotTweetB = entryLengthB.liftB(function(len) { 
    return len > 140 });

  insertValueB(cannotTweetB, "tweetButton", "disabled");

  var statusUpdatesE = clicksE("tweetButton").snapshotE($B("newstatus"));

  getWebServiceObjectE(statusUpdatesE.mapE(mkUpdateRequest))
  .mapE(function(x) { console.log(x); });
    
}

window.addEventListener("load", start);
