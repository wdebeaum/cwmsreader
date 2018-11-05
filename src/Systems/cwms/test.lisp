
;;;;
;;;; File: test.lisp
;;;; Creator: George Ferguson
;;;; Created: Tue Jun 19 14:35:09 2012
;;;; Time-stamp: <Tue Jan 30 11:48:49 MST 2018 james>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
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
      "What is the impact of El Nino in Sudan?" ;;"what if there is an El Nino in 2018"
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
  (cdr (assoc 'cwms-first-demo  *sample-dialogues*)))

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
