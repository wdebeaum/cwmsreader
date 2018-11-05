HeadlessWeb - headless web browser for performing simple procedures on the web
William de Beaumont
2017-01-23

HeadlessWeb is a "headless" web browser in the sense that it does not have a
graphical interface, only a KQML interface. It also does not execute any
JavaScript from the pages it visits. It does, however, inject its own copy of
jQuery into each page. You can therefore use jQuery selectors to select
elements on the page to manipulate during a web procedure.

HeadlessWeb currently services three kinds of KQML request message:
do-web-procedure, get-number-of-google-hits, and get-top-google-excerpt. The
first is more general, while the others are sort of special cases. 

Get-number-of-google-hits gets the number of hits reported by google for a
given search phrase (in quotes so that the individual words aren't searched for
too):

  (request :content (get-number-of-google-hits :search "ASPP2 protein"))
  (tell :content 754)
  (request :content (get-number-of-google-hits :search "ASPP2 protein family"))
  (tell :content 0)

Get-top-google-excerpt gets the excerpt text from the top hit of a google
search:

  (request :content (get-top-google-excerpt :search "what are the main crops in south sudan"))
  (reply :content (report :content (answer :excerpt "While the country produces and consumes a wide range of agricultural commodities, with the passage of time some commodities have become prominent in the national pattern of consumption. Cereals, primarily sorghum and maize, millet and rice are the dominant staple crops in South Sudan.")))

Do-web-procedure accepts a :steps argument listing the steps to take. A step
can take any of the following forms:

  (go-to-uri :uri "http://example.com/")
    Open the webpage at the given URI.
  (follow-link :select "a[id=\"foo\"]")
    Follow the selected link.
  (fill-field :select "input[name=\"foo\"]" :value "bar")
    Set the value of the selected form field to the given value.
  (submit-form :select "form")
    Submit the selected form.
  (get-tree :select "div[class=\"foo\"]")
    Get a Lispy representation of the DOM tree for the selected element.
  (get-inner-html :select "div[class=\"foo\"]")
    Get the HTML inside the selected element (excluding the start and end tags).
  (get-outer-html :select "div[class=\"foo\"]")
    Get the HTML for the selected element, including the start and end tags.
  (get-text :select "div[class=\"foo\"]")
    Get the text inside the selected element (with no tags).
  (get-attributes :select "div[class=\"foo\"]")
    Get the attributes of the selected element as a keyword-argument list.

The :select parameter that many of these steps have takes a jQuery selector.
The get-* steps each add a single element to a result list, which becomes the
:content of the reply to the original do-web-procedure request. The result of
each get-* step is a list of results, one for each element selected by the
:select argument. For the other steps that take :select, they are expected to
select exactly one element; if not, an error message is sent in reply.
