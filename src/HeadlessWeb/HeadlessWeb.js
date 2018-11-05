const util = require('util');
const KQML = require('KQML/kqml.js');
const TripsModule = require('TripsModule/trips-module.js');
const WebProcedure = require('./WebProcedure.js');

// Google won't give us the #resultStats if it thinks we're jsdom, so say we're
// Firefox instead (the rest of the string is the same as the default)
const googleUserAgent = "Mozilla/5.0 (" + process.platform + ") AppleWebKit/537.36 (KHTML, like Gecko) Firefox/57.0"

function HeadlessWeb(argv, oninit) {
  TripsModule.call(this, argv);
  this.name = 'HeadlessWeb';
  var that = this;
  this.init(function() {
    that.addHandler(
      KQML.parse('(request &key :content (do-web-procedure . *))'),
      that.doWebProcedure
    );
    that.addHandler(
      KQML.parse('(request &key :content (get-number-of-google-hits . *))'),
      that.getNumberOfGoogleHits
    );
    that.addHandler(
      KQML.parse('(request &key :content (get-top-google-excerpt . *))'),
      that.getTopGoogleExcerpt
    );
    if (oninit !== undefined) {
      oninit.call(that);
    }
  });
}
util.inherits(HeadlessWeb, TripsModule);

[ // begin HeadlessWeb methods

  function doWebProcedure(msg) {
    new WebProcedure(this, msg).startNextStep();
  },

  function getNumberOfGoogleHits(msg) {
    var content = KQML.keywordify(msg.content);
    if (!('search' in content && typeof content.search == 'string' && 
          KQML.isKQMLString(content.search))) {
      this.replyToMsg(msg, { 0: 'error', comment: '"Expected string in :search"' });
      return;
    }
    var search = KQML.kqmlStringAsJS(content.search);
    msg.content.push(':steps', [
      "(go-to-uri :uri \"https://www.google.com/\" :user-agent \"" +
        googleUserAgent + "\")",
      "(fill-field :select \"input[name=\\\"q\\\"]\" :value \"\\\"" +
        search + "\\\"\")",
      "(submit-form :select form)",
      "(get-text :select \"#topstuff:contains('No results found for')\")",
      "(get-text :select \"#resultStats\")"
    ].map(KQML.parse));
    var that = this;
    new WebProcedure(this, msg, function(results) {
      if (results.length != 2) {
	this.onError('Wrong number of results?!');
      } else if (results[0].length > 0) { // no google results for original search, resultStats are for suggested replacement search
	that.replyToMsg(msg, { 0: 'tell', content: 0 });
      } else if (results[1].length != 1) {
	this.onError('Expected 1 #resultStats, but got ' + results[0].length);
      } else {
	var resultString = results[1][0];
	var match = /About ([\d,]+) results/.exec(resultString);
	if (match == null) {
	  this.onError('Got strange #resultStats from Google: ' + resultString);
	} else {
	  var numResults = parseInt(match[1].replace(/,/g,''));
	  that.replyToMsg(msg, { 0: 'tell', content: numResults });
	}
      }
    }).startNextStep();
  },

  function getTopGoogleExcerpt(msg) {
    var content = KQML.keywordify(msg.content);
    if (!('search' in content && typeof content.search == 'string' && 
          KQML.isKQMLString(content.search))) {
      this.replyToMsg(msg, { 0: 'error', comment: '"Expected string in :search"' });
      return;
    }
    var search = KQML.kqmlStringAsJS(content.search);
    msg.content.push(':steps', [
      "(go-to-uri :uri \"https://www.google.com/\" :user-agent \"" +
        googleUserAgent + "\")",
      "(fill-field :select \"input[name=\\\"q\\\"]\" :value \"" +
        search + "\")",
      "(submit-form :select form)",
      "(get-text :select \"div.kpd-ans\")", // try this first, for e.g. pop. #s
      "(get-text :select \"span.st\")" // more general case
    ].map(KQML.parse));
    var that = this;
    new WebProcedure(this, msg, function(results) {
      var excerpt = 'nil';
      if (results.length != 2) {
	this.onError('Wrong number of results?!');
	return;
      } else if (results[0].length > 0) {
	excerpt = results[0][0];
      } else if (results[1].length > 0) {
	excerpt = results[1][0];
      }
      that.replyToMsg(msg, { 0: 'reply', content: { 0: 'report', content: { 0: 'answer', excerpt: excerpt } } });
    }).startNextStep();
  }

].map(function(fn) { HeadlessWeb.prototype[fn.name] = fn; });

new HeadlessWeb(process.argv.slice(2), function() { this.run(); });

