;;  Code for managing the collaborative probem solving state

(in-package :cwmsAgent)

(defvar *evaluation-record* nil)

(defun memo-evaluation-result (status id ps-act data)
  (push (list id status ps-act data)
	*evaluation-record*))

(defun get-evaluation-result (id)
  (cdr (assoc id *evaluation-record*)))
    

(defun process-reply (msg args result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    (if reply-with
	(send-msg (append (list 'reply :receiver sender :content (clean-for-sending result))
			  (list :in-reply-to reply-with)))
	(send-msg (list 'tell :content (clean-for-sending result))))))

(defun clean-for-sending (msg)
  (when msg
    
    (cond ((or (symbolp msg) (numberp msg) (stringp msg))
	   msg)
	  ((or (symbolp (car msg)) (numberp (car msg)) (stringp (car msg)))
	   (reuse-cons (car msg) (clean-for-sending (cdr msg)) msg))
	  ((consp (car msg))
	   (reuse-cons (clean-for-sending (car msg)) (clean-for-sending (cdr msg)) msg))
	  (t
	   (format t "~%ERROR: bad content in message: ~S" msg)
	   (cons 'content-removed (clean-for-sending (cdr msg)))))))

(defun clean-out-unbound-vars (xx)
  (format t "~%Expression to clean is ~S" xx)
  xx)
  
(defun process-evaluate (msg args)
  (let ((content (find-arg args :content))
	(context (find-arg args :context)))
    (dagent::set-result nil)
    (pop-active-goal-if-necessary)
    (dagent::invoke-state 'evaluate-handling nil (dagent::lookup-user 'desktop) nil nil
			  (cons
			   (append `(REQUEST XX EVALUATE :content ,content :context ,context :ps-id)
				   content)
			   context))
    (let ((res (clean-out-unbound-vars (dagent::get-latest-result))))
      (trace-msg 2 "~% EVALUATE returns ~S" res)
      (if res 
	  (case (car res)
	    (modify-goal
	     (let* ((goal-id (gen-symbol 'G))
		    (goal-desc (second res))
		    (desc-id (second goal-desc))
		    (ps-act `(ADOPT :ID ,goal-id
				   :what ,desc-id
				   :as (modification :of ,(find-arg-in-act content :id))))
		    (new-context (cons goal-desc context)))
	       (add-to-goal-symbol-table goal-id (create-goal-from-description goal-id goal-desc context))
	       (memo-evaluation-result 'acceptable goal-id ps-act new-context)
	       (list 'REPORT :content `(acceptable :what ,content
						   :effect ,ps-act
						   :context ,new-context))))
	    ((add-subgoal new-goal)
	     (let* ((goal-id (gen-symbol 'G))
		    (goal-desc (second res))
		    (reln (case (car res)
			    (add-subgoal 'subgoal)
			    (new-goal 'goal)))
		    (desc-id (second goal-desc))
		    (ps-act `(ADOPT :ID ,goal-id
				   :what ,desc-id
				   :as ,(if (eq reln 'goal)
					   '(goal)
					   (list reln :of (goal-id (get-active-goal))))))
		    (new-context (cons goal-desc context)))
	       (add-to-goal-symbol-table goal-id (create-goal-from-description goal-id goal-desc context))
	       (memo-evaluation-result 'acceptable goal-id ps-act new-context)
	       (list 'REPORT :content `(acceptable :what ,content
						   :effect ,ps-act
						   :context ,new-context))))

	    (;(set-parameter CONFIRM-PARAMETER)
	     (clarify-parameter CONFIRM-PARAMETER confirm-plan)
	     (format t "~%Processing SET-PARAMETER ...")
	     (let ((id (find-arg-in-act res :id))
		   (value (extract-value (find-arg-in-act res :value) context))
		   (result (find-arg-in-act res :result)))
	       (if (eq result 'acceptable)
		   (progn 
		     (memo-evaluation-result 'acceptable id
					     (list (car res) :id id :value value)
					     nil)
		     (list 'REPORT
			   :content (list 'acceptable :what content)
			   :context context)
		     )
		 (list 'REPORT
		   :content (list 'unacceptable :type 'INVALID-ANSWER :what content :reason (list 'expected-type (find-arg-in-act res :ptype)) )
		   :context context)
		 )
	       ))

	    ;; default
	    (acceptable
	     (list 'REPORT
		   :content (list 'acceptable :what (find-arg-in-act res :content))
		   :context context))
	    (otherwise
	     `(REPORT :content (unacceptable :type FAILED-TO-INTERPRET :what ,(find-arg-in-act res :content))
	       :context ,context))
	    )
	  `(REPORT :content (unacceptable :type FAILED-TO-INTERPRET :what ,content) :context ,context)
	  )
      )))
 
(defun extract-value (id context)
  (let* ((lf (get-from-context id context))
	 (value (or (find-arg lf :value)
		    (find-arg lf :year)
		    (find-arg lf :param-code) id))
	 (value-is-lf (get-from-context value context)))
    (if (and value-is-lf (not (eq id value)))
	(extract-value value context)
	value)))
    
(Defun reply-to-message (msg result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    (send-msg (append (list 'reply :receiver sender :content result) (list :in-reply-to reply-with)))))

(defun process-commit (msg args)
  ;; We commit the goal, which typically updates the plan state in some way
  (let* ((content (find-arg args :content))
	 (effect (find-arg args :effect))
	 (cps-act (if (and effect (not (eq effect '-)))
		      effect
		      content))
	 (goal-id (find-arg-in-act cps-act :id))
	 (as (find-arg-in-act cps-act :as))
	 (goal (get-goal-with-id goal-id)))   ;; we should have already built the goal when we did the evaluate
    
    ;; We update our problem solving state according to the new CPS-ACT
    (case (car cps-act)
      (ADOPT 
       (case (car as)
	 ;; a top level goal
	 ((goal modification) ;; the modification is a short cut here until I figure out how better to handle it
	  (adopt-new-goal cps-act goal-id goal))
	 (subgoal
	  (adopt-new-subgoal cps-act goal-id goal (find-arg-in-act as :of)))
	 ))
      (ANSWER
       ;; we've already processed this, so install cached value
       (let* ((id (find-arg-in-act content :to))
	      (pv (get-evaluation-result id))
	      (answer (cadr pv))
	      (context (caddr pv)))
	 (case (car answer)
	   (clarify-parameter
	    (install-parameter-value-in-plan (find-arg-in-act answer :id)
					     (find-arg-in-act answer :value)))
	   (confirm-parameter
	    (case (find-arg-in-act answer :value)
	      (ONT::TRUE
	       ;;  step was confirmed so we're fine, what for the WHAT-NEXT
	       nil)
	      (otherwise
	       (break "can't handle confirmation denial yet!")
	       
	       )))
	   )))
	 
	 (otherwise
	  (break "  IN COMMIT"))
	 )))
    
    #||(if goal
    (multiple-value-bind (parameters problems)
    (identify-parameters (goal-context goal))
	  (setf (goal-parameters goal)
		(append (mapcar #'(lambda (x)
				    (append '(PARAMETER :status :known)  (cdr x)))
				parameters)
			(mapcar #'(lambda (x)
				    (append '(PARAMETER :status :unknown) (cdr x)))
				problems)))
	  (push-top-level-goal goal))
	  	  
	'(REPORT :content (FAILURE :type UNKNOWN-GOAL))
	)))
||#
(defun identify-parameters (context)
  "Returns two values: a list of parameters with IDs successfully identify,
       and a list of terms that cannot be identified"
  (when context
    (let ((term (car context)))
      ;; if not a term that we would associate a parameter with, we skip it
      (if (or (member (car term) '(ont::RELN ont::event))
	      (member (find-arg term :instance-of) '(ont::NUMBER ont::person ont::quantity ont::set)))
	  (identify-parameters (cdr context))
	  ; Otherwise, we try to identify it
	  ;; first do the recursive call to get the rest
	  (multiple-value-bind 
		(rest-of-parameters rest-of-problems)
	      (identify-parameters (cdr context))
	    ;; now process the first case
	    (multiple-value-bind 
		  (dependent-terms remaining-context)
		(remove-unused-context term context)
	      (let ((reply (send-and-wait `(REQUEST :content (IDENTIFY-PARAMETER :context ,dependent-terms)))))
		(if (eq (car reply) 'parameter-identified)
		    (values (cons reply rest-of-parameters)
			    rest-of-problems)
		    (values rest-of-parameters
			    (cons reply rest-of-problems))))))))))
  
(defun what-next (msg args)
  (let* ((active-goal-id-in-msg (find-arg args :active-goal))
	 (what-i-think-is-active-goal (planner-state-content *active-goal*))
	 (active-goal (get-goal-with-id what-i-think-is-active-goal)))
    (if (not (and (eq active-goal-id-in-msg what-i-think-is-active-goal)
		  (goal-p active-goal)))
	(format t "~%~%ERROR - goals don't match: ~S and ~S" active-goal-id-in-msg what-i-think-is-active-goal)

	(progn
	  (trace-msg 2 "~% Continuing planning on goal ~S" active-goal)
	  (continue-planning active-goal)
	  )
	  #||;; not sure this following is ever used -- the value would be installed at the COMMIT
	  ((find-arg args :to)
	   ;; we have a question
	   (let* ((act-parameter (get-parameter-being-clarified (find-arg args :to)))
		  (act (car act-parameter))
		  (parameter (cadr act-parameter))
		  (value (extract-value (find-arg args :value) (find-arg args :context))))
	     
	     ))||#
	  )))
						 
(defun find-lf-in-context (context id)
  (find id context :key #'cadr))

(defun find-lf-in-context-tmp (context id)  ; id not used
  (find-if #'(lambda (x) (member (find-arg x :instance-of)
				 '(ONT::CREATE ONT::PUT-B6-ON-THE-TABLE ONT::Please-put-B7-on-B6 ONT::PUT
				   ONT::EXECUTE)))
		 context))

(defun restart-cwms-if-not-from-myself (msg)
  (let ((sender (find-arg-in-act msg :sender)))
    (when (not (eq sender 'cwmsagent))
	(restart-cwms))))

(defun restart-cwms nil
  (setq *current-plan* nil)
  (setq *parameters* nil)
  (setq *service-library* nil)
  (setq *service-input-table* (make-hash-table :test #'equal))
  (setq *service-output-table* (make-hash-table :test #'equal))
  (setq *parameter-graph* nil)
  (send-msg '(REQUEST :content (RESTART)))
  (send-msg '(TELL :content (I-AM-HERE :who CWMSAGENT)))
  )

;;  here are some various functions that control the GUI display

(defun put-up-location-display (param)
  (let ((country-code (simplify-value (parameter-in-plan-value param))))
    ;(format t "~%Need to call SPACEMAP, get the file and then call the display to put up the map: code is ~S" param)
    (if country-code
	(let* ((reply (send-and-wait `(request :content (get-geographic-region :description (ISO ,(im::stringify country-code) ) :format (raster "GTiff" 720 360) :output (location))) ))
	      (location-file (find-arg-in-act (find-arg-in-act reply :content) :location))
	      )
	  (send-msg `(request :content (display :file ,location-file)))
	  )
      ; else...
      )
    )
  )

(defun put-up-table-display (table args y-label)
  (let* ((revised-table (convert-times-to-strings
			 (sort-values-by-year
			  (reduce-table (cadr table) (cddr table) args )))))
    (send-msg `(request :content (chart :data ,revised-table :y-axis-label ,y-label)))
    ))

(defun reduce-table (header values argnames)
  "This eliminates any parameters and values that are not includes in args"
  (let ((arg-not-needed (find-if #'(lambda (x) (not (member x argnames))) header)))
    (if (null arg-not-needed)
	(list*'table header values)
	;; we have a arg to eliminate
	(let ((posn (position arg-not-needed header)))
	  (reduce-table (remove-nth header posn)
			(mapcar #'(lambda (x) (remove-nth x posn))
				values)
			argnames)
	  ))
    ))

(defun sort-values-by-year (table)
  (let ((values (cddr table))
	(posn (or (position :years (cadr table)) (position :year (cadr table)) (position :plyr (cadr table)))))
    (if posn 
	(list* 'table (cadr table) (sort-by-posn posn values))
	table)))

(defun sort-by-posn (posn tuples)
  (sort tuples #'< :key #'(lambda (x) (nth posn x))))
  
(defun convert-times-to-strings (xx)
  (when xx
    (if (and (numberp xx) (> xx 1970) (< xx 2050))
	(format nil "~S" xx)
	(if (consp xx)
	    (cons (convert-times-to-strings (car xx))
		  (convert-times-to-strings (cdr xx)))
	    xx))))
	
(defun remove-nth (ll n)
  (if (<= n 0)
      (cdr ll)
      (cons (car ll) (remove-nth (cdr ll) (- n 1)))))
