;;  Code for managing the collaborative probem solving state

(in-package :cwmsAgent)

(defun handle-generate (msg arg)
  (let ((utt (quick-generator arg)))
    (if utt
	(say utt)
	(progn
	  (format t "Speech act is ~S" msg)
	  (say "I don't know how to say that act yet")
	  )
	)))
  

(defun say (msg  &optional channel)
  (send-and-wait `(REQUEST :content (SAY ,msg))))


(Defun quick-generator (msg)
  "this checks for certain common, or prededtermined situations and returns a string - por nothing if there is no match"
  (let* ((content (find-arg msg :content))
	 (context1 (find-arg-in-act content :context))
	 (context2 (find-arg msg :context))
	 (context (append (if (consp context1) context1)
			  (if (consp context2) context2)))
	 (content1 (find-arg-in-act content :content))
	 )
    ;; quick hack as sometimes we get a multiply nested propose
    (if (and (eq (car content) 'ont::propose)
	     (consp content1)
	     (member (car content1) '(ont::ask-if ont::ask-wh ont::clarify-goal ont::request ont::propose ont::answer ont::unacceptable ont::tell ont::assertion
				      )))
	(setq content content1))
    
    (trace-msg 2 "Quick generator processing ~S" content)
    (case (car content)
	((ask-if ont::ask-if)
	 (let* ((query (find-lf-in-context (or (find-arg-in-act content :query)
					       (find-arg-in-act content :what)) context))
		(code (find-arg query :code))
		(arg (find-arg query :arg))
		)
	   (if code
	       ;; pre anticipated conditions using codes
	       (case code
		 (pct? "Shall we look at percentage of children who are malnourished (:PCT_MALNOUR_CHILD)?")
		 (do-baseline? (format nil "Do you want me to compute a baseline analysis for ~A?" (if arg (gen-description arg)
												       "Sudan")))
		 (run-plan? (format nil "I can estimate this effect using the following plan. Is it OK?"))
		 (displayed (format nil "Here is a graph of causal infleunces."))
		 (execute-plan? "Here are the influences. Shall I try to quantity the effect?"))
	       ;; no magic code, we attempt to generate the LF
	       (case (find-arg query :instance-of)
		 (ont::HAVE-PROPERTY
		  (let ((prop (find-lf-in-context (find-arg query :formal) context)))
		    (case (find-arg prop :instance-of)
		      (ONT::COMPLETED
		       (pick-one '("Are we finished with this subtask?" "Is this subtask completed?")
				 ))
		      ))
		  )
		 ))))
	(ONT::CLARIFY-GOAL
	 "What are you trying to do?")
	
	((ask-wh ont::ask-wh)
	 (let ((query (find-lf-in-context (find-arg-in-act content :query) context))
	       (what (find-lf-in-context (find-arg-in-act content :what) context)))
	   (if  query
	       (let ((ground (find-lf-in-context (find-arg what :ground) context)))
		 (if ground
		     (concatenate 'string "What is the value of " (symbol-name (find-arg query :instance-of))
				  " associated with " (or (symbol-name (find-arg ground :instance-of))
							  (symbol-name (find-arg what :instance-of))))
		     (concatenate 'string "What is the value of " (symbol-name (find-arg query :instance-of))
				  ))
		     )))
	   )
	((ont::request request ont::propose propose)
	 (format t "~%Generating request: ~S:" content)
	 (let ((content1 (find-arg-in-act content :content)))
	   (if (Consp content1)
	       ;; "This may be obsolete"
	       (case (car content1)
		 (ONT::PROPOSE-GOAL
		  "What do you want to do?")
		 ((ONT::adopt adopt)
		  (let* ((id (find-arg-in-act content1 :what))
			 (prop (find-lf-in-context id context)))
		    (case (find-arg prop :instance-of)
		      (ont::YOU-SUGGEST-SOMETHING
		       (pick-one '("what do you want to do?" "what shall we do?" "what is your goal?" )))
		      )))
		 )
	       ;;  standard generation
	       (let* ((what (find-arg-in-act content :what))
		      (whatlf (find-lf-in-context what context))
		      )
		 (cond ((om::subtype (find-arg whatlf :instance-of) 'ont::event-of-action)
			(let ((whatlfarg (find-lf-in-context (or (find-arg whatlf :affected)
								 (find-arg whatlf :neutral)
								 (find-arg whatlf :affected-result))
							     context)))
			  (concatenate 'string "I propose that we "
				       (symbol-name (find-arg whatlf :instance-of)) " "
				       (if whatlfarg (symbol-name (find-arg whatlfarg :instance-of))))
			  ))
		       ((eq (find-arg whatlf :instance-of) 'ont::be)
			(let* ((n1 (find-arg whatlf :neutral))
			      (n2 (find-lf-in-context (find-arg whatlf :neutral1) context))
			      (assoc-val (find-lf-in-context (find-arg n2 :assoc-with) context)))
			  (concatenate 'string "How about we make "
				       (symbol-name (find-arg n2 :instance-of))
				       " be "
				       (if assoc-val (symbol-name (find-arg assoc-val :instance-of))))
				       
			))
		       ))
	       )))
		     
	 
	((ont::accept accept)
	 (pick-one '("OK!" "Sure!" "You bet!")))

	(ont::something-is-wrong
	 (pick-one "I didn't understand that" "Sorry, I didn't understand that" ))

	(ont::propose-goal
	 (pick-one "What do you want to do?" "What shall we do?" "What should we do next?"))

	(ont::greet
	 (pick-one '("Hi" "Hello" "Greetings!")))

	(ont::answer
	 (let ((value (find-arg-in-act content :value)))
	   value))
	
	((unacceptable ont::unacceptable)
	 (pick-one `("Sorry, I didn't understand" "I didn't get that" "I didn't understand what you said")))

	((tell ont::tell)
	 (let ((content1 (find-arg-in-act content :content)))
	   (if (Consp content1)
	       (case (car content1)
		 (ONT::FAIL
		  (pick-one `("Sorry, I didn't understand" "I didn't get that" "I didn't understand what you said")))

		 (ONT::DONE
		  (pick-one '("we're done" "We are finished")))

		 (ONT::CLOSE-TOPIC
		  (pick-one '("We've complted that task" "we accomplished the goal")))
		 
		 (ONT::WAITING
		  (pick-one `("I'm waiting for you" "you still there?" "I'm waiting for you to speak" "I'm waiting")))
		 ))
	   ))
	
	((assertion ont::assertion)
	 (let ((what (find-lf-in-context (find-arg-in-act content :what) context)))
	   (case (find-arg what :instance-of)
	     ((ont::results results)
	      (let ((value (find-arg what :value)))
		(concatenate 'string "The results are " (format nil "~S" value))))
	     (displayed
	      (format nil "Here is a graph of causal influences."))
	     (execute-plan?
	      "I'll execute the plan")
	     (know-nothing
	      "Sorry I don't know anything about that")
	     (read-paper-already
	      "OK. I already read that paper. What do you want to look at?")
	     (this-is-what-i-know
	      "OK. This is what I know")
	     (remember-that
	      "OK. I'll remember that")
	     )
	   ))
	)
    ))

(defun gen-description (arg)
  (if (symbolp arg)
      (case arg
	(:ss "South Sudan")
	(:sd "Sudan")
	(otherwise (symbol-name arg))
	)
      "UNKNOWN"))
      

(defun prep-parser-if-necessary (speechact)
  "this checks if we are asking a quesrtion, and if so preset the parser copst-table for the answer"
  (let ((act (if (eq (car speechact) 'ONT::PROPOSE)
		 (find-arg-in-act speechact :content)
	       speechact)))
    (case (car act)
      (ASK-IF
       (send-msg '(request :content (adjust-cost-table :mods ((ONT::SA_RESPONSE 1))))))
      (ASK-WH
       (send-msg '(request :content (adjust-cost-table :mods ((ONT::SA_IDENTIFY 1) (ONT::SA_FRAGMENT 1.5) (w::ADJP 1.1) (w::ADVBL 1.1))))))
      ))
  )
					  
(defun expand-args (msg)
  "this examines the msg for variables that store the CPS state and replaces them with the values"
  (when msg  
    (if (consp msg) 
	(cond 
	  ((and (eq (car msg) 'v) (keywordp (cadr msg)))
	   (or (lookupCPSvar (cadr msg)) msg))
	  ((and (eq (car msg) 'vv) (keywordp (cadr msg)))
	   (let ((lv (lookupCPSvar (cadr msg))))
	     (or (cadr lv) lv  msg)))
	  (t
	   (cons (expand-args (car msg)) (expand-args (cdr msg)))))
	msg)))


(defun pick-one (elements)
  (nth (random (list-length elements)) elements))
