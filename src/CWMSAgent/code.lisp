;;  Code for managing the collaborative probem solving state

(in-package :cwmsAgent)

(defvar *evaluation-record* nil)
(defvar *current-state* 'top-level)

(defun update-current-state (x)
  (setq *current-state* x))

(defun memo-evaluation-result (status id ps-act data possible-tasks)
  (push (list id status ps-act data possible-tasks)
	*evaluation-record*))

(defun get-evaluation-result (id)
  "returns values: STATUS PS-ACT DATA and possible tasks "
  (let ((res (or (cdr (assoc id *evaluation-record*))
		 (cdar *evaluation-record*))))
    (values (car res) (cadr res) (caddr res) (cadddr res))))
    

(defun process-reply (msg args result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    (if reply-with
	(send-msg (append (list 'reply :receiver sender :content (clean-for-sending result))
			  (list :in-reply-to reply-with)))
	(send-msg (list 'tell :content (clean-for-sending result))))))

(defun clean-for-sending (msg)
  "makes sure everthing is either a symbol, number, string, or list"
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
  (clean-for-sending xx)
  )
  
(defun process-evaluate (msg args)
  (let* ((content (find-arg args :content))
	 (what (find-arg-in-act content :what))
	 (as (find-arg-in-act content :as))
	 (goal-id (find-arg-in-act content :id))
	 (context (find-arg args :context))
	 )
       (multiple-value-bind
	     (status new-goal-id interps newcontext extra-info)
	   (evaluate-ps-act content context)
	 (memo-evaluation-result status new-goal-id interps newcontext extra-info)
	 (case status
	   ((acceptable ambiguous)    
	    (list 'REPORT :content `(,status :what ,content
					     :context ,newcontext))
	    )
	   (otherwise
	    
	    (list 'REPORT :content `(UNACCEPTABLE :what ,content :type FAILED-TO-INTERPRET
						  :reason ,interps
						  :context ,newcontext))
	    )
	   ))
       ))

(defun evaluate-ps-act (content context)
  (let* (
	 (what (find-arg-in-act content :what))
	 (as (find-arg-in-act content :as))
	 (goal-id (find-arg-in-act content :id))
	 )
    (case (car content)
      (adopt
       (multiple-value-bind
	     (status interps)
	   (evaluate-adopt-goal what context as)
	 (let* ((itasks (mapcar #'(lambda (x)
				  (instantiate-task (car x) (cadr x) goal-id)) interps))
	       (newcontext (append (build-formula (car itasks)) context)))
	   (values status goal-id interps newcontext itasks))))
      (answer
       (multiple-value-bind
	     (status interps)
	   (evaluate-answer content context)
	 (values status (find-arg-in-act content :to) interps context)))

      (assertion
       (multiple-value-bind
	     (status interps)
	   (evaluate-assertion content context)
	 (values status (find-arg-in-act (find-arg-in-act content :as) :goal)
		 interps context)))
      )))
       
				
	     

(defun build-formula (task)
  "Takes an instantiated task and build a proposition like description with bound arguments"
  (when (task-p task)
    (cons (task-name task)
	  (get-bound-args (task-arguments task)))))

(defun get-bound-args (args)
  (when args
    (if (not (im::var-p (argument-value (car args))))
	(list* (argument-name (car args))
	       (argument-value (car args))
	       (get-bound-args (cdr args)))
	(get-bound-args (cdr args))
	)))

(defun extract-result (res)
  (values (car res)
	  (clean-out-unbound-vars (find-arg-in-act res :goal))
	  (find-arg-in-act res :continuation)))
	  
 
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
	 (goal-id (find-arg-in-act content :id))
	 (as (find-arg-in-act content :as))
	 (toptask (top-of-task-stack))
	 )
    
    (if (and toptask
	     (eq (task-status toptask) 'proposed)
	     (eq goal-id (task-id toptask))
	     )
	;;  special case, the COMMIT is accepting a goal the system just proposed
	(case (task-name toptask)
	  (IDENTIFY-PARAMETER
	   ;;  User has agreed to an argument value, we just return successfully from the top task
	   (trace-msg 2 "USer accepted the proposal, we are popping the stack")
	   (return-from-top-task nil)
	   )
	  (otherwise
	   ;;  the task is not done yet, probably, we switch status to ACTIVE and await a WHAT-NEXT
	   (setf (task-status toptask) 'active)
	  ))
	      ;;  for identify-parameter with a 
	;;  The usual case, a user proposal that was judged acceptable has been committed
	(multiple-value-bind (status ps-act data possible-tasks)
	    (get-evaluation-result goal-id)     
	  ;; We update our problem solving state according to the new CPS-ACT
	  (case (car content)
	    (ADOPT 
	     (case (car as)
	       ;; a top level goal
	       ((goal modification) ;; the modification is a short cut here until I figure out how better to handle it
		(push-task (car possible-tasks)))
	       (subgoal
		))
	     )
	    (ANSWER 
	     ;; we've already processed this, so install cached value
	     (multiple-value-bind (status record)
		 (get-evaluation-result (find-arg-in-act content :to))
	       (let ((task (caar record))
		     (bndgs (cadr (car record))))
		 (if (eq status 'acceptable)
		     ;; the question is satisfied, we pop the clarificaiton task after
		     ;;  installing the answer below
		     (return-from-top-task bndgs)
		     (format t "~% problem in processing answer"))
		 )))
	    (ASSERTION  ;; assertions just add bindings to the top task
	     ;; we've already processed this, so install cached value - leaving top task intact
	     (multiple-value-bind (status record)
		 (get-evaluation-result (find-arg-in-act content :to))
	       (let ((task (caar record))
		     (bndgs (caadr (car record))))
		 (if (eq status 'acceptable)
		     ;; the question is satisfied, we pop the clarificaiton task after
		     ;;  installing the answer below
		     (bind-top-task bndgs)
		     ))
	       ))
	    
	    )))))
	   
(defun what-next (msg args)
  (if (top-of-task-stack)
      (let* ((active-goal-id-in-msg (find-arg args :active-goal))
	     (active-goals (mapcar #'task-id *task-stack*))
	     (context (find-arg args :context))
	     )
	(if (not (member active-goal-id-in-msg active-goals))
	    ;; the goal must have been popped off the stack, so we indicate it is done
	  (progn
	    (format t "~%~%WARNING - goal ~S doesn't match active goals ~S. Returning that ~S is done" active-goal-id-in-msg active-goals active-goal-id-in-msg)
	    `(REPORT :content (EXECUTION-STATUS :goal ,active-goal-id-in-msg
						:status ONT::DONE :silent T))
	    )
	  (or (what-next-in-task (top-of-task-stack) context)
	      (progn (trace-msg 2 "   Nothing to do - so we wait ...")
		     '(REPORT :content (WAIT)))
	      )))
      `(REPORT :content (FAILURE :what `(find-arg args :active-goal) :reason No-TASK-KNOWN))
      ))
      
  
(defun find-lf-in-context (id context)
  (find id context :key #'cadr))


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
