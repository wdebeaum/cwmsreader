
;;;;
;;;; File: test.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jun 19 14:35:09 2012
;;;; Time-stamp: <Sun Sep 15 14:11:24 EDT 2019 james>
;;;;

(unless (find-package :trips)
   (load (make-pathname :directory ;; '(:absolute "Users" "James" "Desktop" "Work" "Code" "cwms" "src" "config" "lisp")
			'(:relative :up :up "config" "lisp")
		       :name "trips")))
;;;
;;; Load our TRIPS system
;;;
(load #!TRIPS"src;Systems;cwms;system")

;;;
;;; Load core testing code
;;;
(load #!TRIPS"src;Systems;core;test")

;;;
;;; In USER for convenience of testing
;;;
(in-package :common-lisp-user)

(setf *texttagger-split-mode* nil)

;;; Sample dialogues
;;;
(setq *sample-dialogues*
      '(
	(cwms-test-user-init-value .
	 ("Let's analyZe the temperature. The location is Sudan."
	  ;; OK  What is the time
	  "June 1998"   ;; did this as "1998" is not currently parsed as a year!!
	  ;; what about we use malnutrition as the matric?
	  "OK"
	  ;;   I propose that we make a plan
	  "OK"
	  ))

	(cwms-test-system-clarification .
	 ("Let's analyze the temperature in 1998"
	  ;;  where?    ;; system initiated clarifgication on a required parameter
	  "In Sudan"
	  ;; "OK?" "How about using data from waether.com as our metric  ;; system inited suggestion of a value for a parameter
	  "OK"
	  ;; I propose that we create a plan
	  "OK"
	  ))
	 
	(cwms-new-demo .
	 ("I want to understand the Ebola outbreak in Congo."
	  ;;  OK
	  "Here's an article about it."
	  ;;  OK. I've already read that one
	  "Show me information related to Ebola."
	  ;;  puts up display on concepts and relations in sentences that mention Ebola
	  ;; "OK. I want to know why we haven't followed up on many of the people at risk."
	  ;;"OK. Add the fact that the attacks caused the lockdown"
	  "Add the fact that the lockdown prevents the follow up"
	  ;; OK. I'll remember that.
	  "What do we know about the response teams?"
	  ;; the response teams are on lockdown
	  "What do we know about the response teams?"
	  ;; OK. I'll remember that
	  ))
    ;; V 0.1  tests basic functionality
    ;; Send an email message (basically a macro, can be done just
    ;; monitoring and sending keystrokes)
    (cwms-first-demo .  
     ( "Analyze food insecurity in Sudan for two years." ;;in 2018"
      ;; OK. Should we look at Child Malnourishment rates?
      "OK"
      ;; OK. do you want to look at a baseline estimate?
      "Yes."
      ;;  the PCT of Malnouriushed is 39%
      "What is the impact of El Nino in Sudan?" ;; "what if there is an El Nino in 2018" 
      ;; headless web,  put up parse, put up causal graph, and say
      ;;  El nino causes rainfall shortges and drought 
      "Let's run a simulation of the El Nino scenario."
      ;; SYSTEM build plan to estimate PCT_MALNOUR,
      ;;   Invoke some built in execution behavior to compute PROD with and without El Nino, to compute D_CONSUM, to %MALNOUR
      ;; I can estimate the effect using the following plan. Is that OK?
      "Yes"
      ;; The PCT of malnourished increases by 5%
     "What can we do to mitigate the increase?"  ;; "what could we do?"
      ;;  here is what I know of causal influences
      "How does this relate to planting date?"
      ;; here is what I know. Shall I try to quantify the effect?
      "yes"
      ;;  let me run some simulations
      ;;   put up charts showing PROD and PCT_MALNOUR varied by planting window
      
      ))
     (cwms-alt-demo .  
     ( "what happens to the price of wheat if we cut the amount of fertilizer by 50%"
       ))
     (cwms-new-demo .
	("I want to understand the Ebola outbreak in Congo."
	 ;;  OK
	 "Here's an article about it."
	 ;;  OK. I've already read that one
	 "Show me information related to Ebola."
	 ;;  puts up display on concepts and relations in sentences that mention Ebola
	 ;; "OK. I want to know why we haven't followed up on many of the people at risk."
	 "OK. Add the fact that the attacks caused the lockdown"
	 ;; OK. I'll remember that.
	 "What do we know about the response teams?"
	 ;; the response teams are on lockdown
	 "What do we know about the response teams?"
	 ;; OK. I'll remember that
	 ))

     (cwms-fertilizer .
	(" I want to know whether we should provide fertilizer to farmers in Ethiopia to increase crop yield."
	 ;; OK. We can explore that. What locations are your considering?    [Put up map of Ethiopian states]
	 "The southern part of the country."
	 ;; I think these are the top five crops in this area:   Teff, ...   Do you want to focus on them all?
	 "Only teff."
	 ;; We can analyze this?  Do you have a level of fertilizer in mind, or should try a range?
	 ;; Here's the data we have on typical use? [show map of fertilizer as kg/ha]
	 "Let's add 10kg/ha."
	 ;; Shall I run a model?
	 "ok"
	 ;; Based on the past 30 years of data, you would see an average increase of 10% in yield.
	 ;; Here's a chart showing the variability (Heat map of gains in yield over region)
	 "How much fertilizer would we need?"
	 ;; ...
	 ))

	(cwms-fertilizer-simple .
	(" I want to increase crop yield in Sudan."
	 ;; OK. We can explore that. When?
	 "next year"
	 ;;  How about we use production (PROD) as the measure?
	 "OK"
	 ;;  How about we use DSSAT
	 "OK"
	 ;;Let's identify the parameters for the simulation
	 "OK"
	 ;; What crops are you modeling?
	 "Sorghum"
	 ;; Let's first specify the locatrion exactly
	 "OK"
	 ;;   Are you finished setting the location
	 "No"
	 ;;What locations are your considering?    [Put up map of Ethiopian states]
	 "The southern part of the country."
	 ;; I think these are the top five crops in this area:   Teff, ...   Do you want to focus on them all?
	 "Only teff."
	 ;; We can analyze this?  Do you have a level of fertilizer in mind, or should try a range?
	 ;; Here's the data we have on typical use? [show map of fertilizer as kg/ha]
	 "Let's add 10kg/ha."
	 ;; Shall I run a model?
	 "ok"
	 ;; Based on the past 30 years of data, you would see an average increase of 10% in yield.
	 ;; Here's a chart showing the variability (Heat map of gains in yield over region)
	 "How much fertilizer would we need?"
	 ;; ...
	 ))
     
     )
  )

;; see test-utterance-demo sample dialogue below
(defun arbitrary-function-to-be-called ()
  (format t "the test-utterance-demo sample dialogue called this arbitrary function~%")
  ;; Note: use COMM:send and not dfc:send-msg, since we're not in the context
  ;; of a defcomponent TRIPS module.
  (COMM:send 'test '(tell :content (message from arbitrary function)))
  ;; For the same reason, we don't have dfc:send-and-wait. Instead, loop over
  ;; COMM:recv and discard messages until you get the reply.
  (COMM:send 'test '(request
		      :receiver lexiconmanager
		      :content (get-lf w::end)
		      :reply-with test123))
  (loop for incoming = (COMM:recv 'test)
  	while incoming
	until (eq 'test123 (util:find-arg-in-act incoming :in-reply-to))
	finally (format t "(get-lf w::end) returned ~s~%"
			(util:find-arg-in-act incoming :content))
	)
  )

#||
;; demo extra capabilities of test-utterance function
;; Note: we have to push this separately because including the #'function
;; doesn't work in a quoted context like the *sample-dialogues* list above.
;; Everything else does.
(push 
  `(test-utterance-demo . (
      "Send a string to be parsed."
      (tell :content (send a single arbitrary message))
      ( (tell :content (send arbitrary list of))
        (tell :content (kqml messages))
	)
      ,#'arbitrary-function-to-be-called
      ))
  *sample-dialogues*)||#

;; Default sample dialogue for this domain
(setf *test-dialog*
      (cdr (assoc 'cwms-fertilizer-simple  *sample-dialogues*)))
  ;;(cdr (assoc 'cwms-new-demo  *sample-dialogues*)))

;(setf *test-dialog*
;  (cdr (assoc 0.1 *sample-dialogues* :test #'eql)))

(defun ptest (key)
  "Make the sample dialogue given by KEY the global *TEST-DIALOG*, then
call TEST. Reports available KEYs on error."
  (let ((dialogue (cdr (assoc key *sample-dialogues* :test #'eql))))
    (cond
     ((not dialogue)
      (format t "~&ptest: unknown sample dialogue: ~S~%" key)
      (format t "~&ptest: possible values: ~S~%" (mapcar #'car *sample-dialogues*)))
     (t
      (setf *test-dialog* dialogue)
      (test)))))


(defun enable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (enable-display))))

(defun disable-graphviz-display ()
  (COMM::send 'test '(request :receiver graphviz :content (disable-display))))

;; This function probably belongs in core/test.lisp
(defun test-all ()
  "Invoke TEST on all utterances of *TEST-DIALOG* in order.
This function does not pause between utterance, wait for results to be
finished, or any other smart thing. It simply pumps the messages in using
TEST."
  (loop for x in *test-dialog*
     do (test x)
       ;; add a wait for procesing
       ;(loop for i from 1 to 2
	;  do ;(format t ".")
	 ;   (sleep 1))
       ))

;; Ditto
(defun test-all-of (key)
  "Set *TEST-DIALOG* to the dialog identified by KEY on *SAMPLE-DIALOGUES*,
then invoke TEST-ALL to test all its utterances."
  (setf *test-dialog* (cdr (assoc key *sample-dialogues*)))
  (test-all))
