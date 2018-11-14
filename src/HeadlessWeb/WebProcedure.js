const fs = require('fs');
const jsdom = require('jsdom');
const { JSDOM } = jsdom;
const KQML = require('KQML/kqml.js');
// we don't actually use sax directly, but jsdom does, and we need to fix its
// concept of entities so it doesn't break every website ever (e.g. &nbsp;
// won't parse *headdesk*)
const sax = require('sax');
sax.XML_ENTITIES = sax.ENTITIES;

// get jquery source code so we can inject it in webpages with jsdom
const jquery = fs.readFileSync(process.env.TRIPS_BASE + '/etc/node_modules/jquery/dist/jquery.min.js', 'utf-8');

function WebProcedure(tripsModule, requestMessage, onSuccess, onError) {
  this.tripsModule = tripsModule;
  this.requestMessage = requestMessage;
  this.replyContent = [];
  this.onSuccess = onSuccess || function(results) {
    this.tripsModule.replyToMsg(this.requestMessage,
      { 0: 'tell', content: results });
  };
  this.onError = onError || function(comment) {
    this.tripsModule.replyToMsg(this.requestMessage,
      { 0: 'error', comment: '"' + KQML.escapeForQuotes(comment) + '"'});
  };
  var content = KQML.keywordify(requestMessage.content);
  // make sure requestMessage has a list of lists whose first element is a string in :steps
  if ('steps' in content && Array.isArray(content.steps) &&
      content.steps.map(function(s) {
	return (Array.isArray(s) && s.length > 0 && typeof s[0] == 'string');
      }).reduce(function(a,b) { return (a && b); }, true)) {
    this.stepsRemaining =
      content.steps.
      map(function(step) {
	return KQML.keywordify(step);
      });
  } else {
    this.onError('Expected list of steps in :steps');
  }
}

[ // begin WebProcedure methods

  function startNextStep() {
    try {
      if (this.stepsRemaining.length == 0) {
	console.log('finished procedure, replyContent:');
	console.log(this.replyContent);
	this.onSuccess(this.replyContent);
      } else {
	var nextStep = this.stepsRemaining.shift();
	var functionName = KQML.camelize(nextStep[0]);
	console.log('starting step ' + functionName);
	if (functionName == 'constructor' ||
	    functionName == 'startNextStep' || // stepception
	    functionName == 'onSuccess' ||
	    functionName == 'onError' ||
	    functionName == 'getJQ' ||
	    functionName == 'selectAndMapToReply' ||
	    /ToKQML$/.test(functionName)) {
	  this.onError('Not a web procedure step name: ' + nextStep[0]);
	} else if (!(functionName in this)) {
	  this.onError('Unknown web procedure step name: ' + nextStep[0]);
	} else {
	  // call the named function with nextStep as an argument, but use
	  // setTimeout so we don't blow the stack for long procedures
	  var that = this;
	  setTimeout(function() {
	    that[functionName].call(that, nextStep);
	  }, 0);
	}
      }
    } catch (e) {
      console.log(e.stack);
    }
  },

  function getJQ(args, qualifier) {
    if ('select' in args) {
      var select = args.select;
      if (KQML.isKQMLString(select)) {
	select = KQML.kqmlStringAsJS(select);
      }
      var jq = this.$(select);
      if (qualifier == '?') { // 0 or 1
	if (jq.length > 1) {
	  this.onError(
	    'Expected 0 or 1 elements, but got ' + jq.length +
	    ', selected by ' + select
	  );
	  return null;
	}
      } else if (qualifier == '*') { // 0 or more
        // do nothing, it's all good
      } else if (qualifier == '+') { // 1 or more
	if (jq.length == 0) {
	  this.onError(
	    'Expected 1 or more elements, but got 0, selected by ' + select
	  );
	  return null;
	}
      } else { // no qualifier, exactly 1
	if (jq.length != 1) {
	  this.onError(
	    'Expected exactly 1 element, but got ' + jq.length +
	    ', selected by ' + select
	  );
	  return null;
	}
      }
      return jq;
    } else {
      this.onError(
        'Missing or malformed :select argument in web procedure step ' +
	args[0]
      );
      return null;
    }
  },

  function goToUri(args) {
    console.log('in goToUri');
    if ('uri' in args &&
        typeof args.uri == 'string' &&
	KQML.isKQMLString(args.uri)) {
      console.log('args ok');
      var uri = KQML.kqmlStringAsJS(args.uri);
      var opts = { runScripts: 'outside-only' };
      if ('userAgent' in args &&
	  typeof args.userAgent == 'string' &&
	  KQML.isKQMLString(args.userAgent)) {
	opts.userAgent = args.userAgent;
      }
      JSDOM.fromURL(uri, opts).then(
        dom => {
	  try {
	    dom.window.eval(jquery);
	    if (!('$' in dom.window)) {
	      this.onError('Failed to make jquery $ object for URI ' + uri);
	    } else {
	      console.log('successfully got URI ' + uri);
	      this.window = dom.window;
	      this.$ = this.window.$;
	      this.startNextStep();
	    }
	  } catch (err) {
	    this.onError('Error processing response from URI ' + uri + ': ' + err.message);
	  }
        },
        err => {
	  this.onError('Error going to URI ' + uri + ': ' + err.message);
        }
      );
    } else {
      console.log('error');
      this.onError(
        'Missing or malformed :uri argument in web procedure step go-to-uri'
      );
      console.log('reported error');
    }
    console.log('goToUri returning');
  },

  function followLink(args) {
    var jq = this.getJQ(args);
    if (jq) {
      if ('href' in jq[0]) {
	// translate this into a go-to-uri
	this.steps.unshift({
	  0: 'go-to-uri',
	  uri: jq[0].href
	});
	this.startNextStep();
      } else {
	this.onError(
	  'Missing href on link element in web procedure step follow-link'
	);
      }
    }
  },

  function fillField(args) {
    var jq = this.getJQ(args);
    if (jq) {
      if ('value' in args && typeof args.value == 'string') {
	if (KQML.isKQMLString(args.value)) {
	  jq.val(KQML.kqmlStringAsJS(args.value));
	} else {
	  jq.val(args.value);
	}
	this.startNextStep();
      } else {
	this.onError(
	  'Missing or malformed :value in web procedure step fill-field'
	);
      }
    }
  },

  function submitForm(args) {
    var jq = this.getJQ(args);
    if (jq) {
      var action = jq.attr('action') || this.window.href;
      var queryString = jq.serialize();
      var method = jq.attr('method') || 'GET';
      //var that = this;
      this.$.ajax({
	url: action,
	data: queryString,
	method: method.toUpperCase(),
	success: (data, textStatus, jqXHR) => {
	  try {
	    var dom = new JSDOM(data, { url: jqXHR.responseURL, runScripts: 'outside-only' });
	    dom.window.eval(jquery);
	    this.$ = dom.window.$;
	    this.startNextStep();
	  } catch (err) {
	    this.onError('Error processing response to form submission: ' + err.message);
	  }
	},
	error: (jqXHR, textStatus, errorThrown) => {
	  console.log({ action: action, queryString: queryString, method: method });
	  console.log(errorThrown);
	  this.onError('Error submitting form: textStatus=' + textStatus + '; errorThrown=' + errorThrown);
	}
      });
    }
  },

  /** Use the :select argument to select a set of elements using jQuery, and
   * then apply fn to each element to map it to an element of a list to push
   * onto the replyContent list. If asString is true, escape and quote the
   * return value of fn before pushing it onto the list. Also start the next
   * step in the procedure.
   */
  function selectAndMapToReply(args, asString, fn) {
    var jq = this.getJQ(args, '*');
    if (jq) {
      this.replyContent.push(
	jq.toArray().map(function(element) {
	  var mapped = fn(element);
	  if (asString) {
	    return '"' + KQML.escapeForQuotes(mapped) + '"';
	  } else {
	    return mapped;
	  }
	})
      );
      this.startNextStep();
    }
  },

  function attributesToKQML(elem) {
    var attrs = [];
    for (var i = 0; i < elem.attributes.length; i++) {
      var a = elem.attributes[i];
      attrs.push(':' + a.name, '"' + KQML.escapeForQuotes(a.value) + '"');
    }
    return attrs;
  },

  function childrenToKQML(elem) {
    var children = [];
    for (var i = 0; i < elem.childNodes.length; i++) {
      var c = elem.childNodes[i];
      if (c.nodeType == 1) { // element node
	children.push(this.elementToKQML(c));
      } else { // assume it's a text node
	children.push('"' + KQML.escapeForQuotes(c.data) + '"');
      }
    }
    return children;
  },

  function elementToKQML(elem) {
    var ret = { 0: elem.tagName };
    var attributes = this.attributesToKQML(elem);
    if (attributes.length > 0) {
      ret.attributes = attributes;
    }
    var children = this.childrenToKQML(elem);
    if (children.length > 0) {
      ret.children = children;
    }
    return ret;
  },

  /** Get a Lispy representation of the DOM tree for the selected element. */
  function getTree(args) {
    this.selectAndMapToReply(args, false, this.elementToKQML.bind(this));
  },

  /** Get the HTML inside the selected element (excluding the start and end
   * tags).
   */
  function getInnerHtml(args) {
    this.selectAndMapToReply(args, true, function(elem) {
      return elem.innerHTML;
    });
  },

  /** Get the HTML for the selected element, including the start and end tags.
   */
  function getOuterHtml(args) {
    this.selectAndMapToReply(args, true, function(elem) {
      return elem.outerHTML;
    });
  },

  /** Get the concatenated text nodes descended from the selected element. */
  function getText(args) {
    var that = this;
    this.selectAndMapToReply(args, true, function(elem) {
      return that.$(elem).text();
    });
  },

  /** Get attributes of the selected element as a keyword-argument list. */
  function getAttributes(args) {
    this.selectAndMapToReply(args, false, this.attributesToKQML);
      
  }

].map(function(fn) { WebProcedure.prototype[fn.name] = fn; });

module.exports = WebProcedure;

