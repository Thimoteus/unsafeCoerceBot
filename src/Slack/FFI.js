var Slack = require("@slack/client");

exports.newRTMClientFFI = function (token) {
  // return function () {
    return new Slack.RTMClient(token);
  // }
}

exports.startRTMFFI = function (client) {
  // return function () {
    client.start();
  // }
}

exports.newWebClientFFI = function (token) {
  // return function () {
    return new Slack.WebClient(token);
  // }
}

exports.listChannelsFFI = function (web, onErr, onSucc) {
  web.channels.list().then(function(res) {
    onSucc(res);
  })
  .catch(function (err) {
    onErr(err);
  });
}

exports.sendMessageFFI = function (rtm, msg, id, onErr, onSucc) {
  rtm.sendMessage(msg, id, function (err, res) {
    if (err) {
      return onErr(err);
    }
    return onSucc(res);
  });
}

exports.onMessageFFI = function (rtm, cb) {
  rtm.on("message", function (msg) {
    cb(msg);
    return;
  })
}

exports.activeUserId = function (rtm) {
  return rtm.activeUserId;
}