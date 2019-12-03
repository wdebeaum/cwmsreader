;;  The decision functions associated with the tasks
;;    All functions take a input of form (<op> <task>) - where <op> is one of WHAT-NEXT, ...
;;    and return a list of three  values   <result message> <stack op> <updated task>
;;       where <reswult message> is what the code returns to the dialogue agent, the <stack op> is PUSH, POP, REPLACE, and <updated task> is the updated task

(in-package :cwmsAgent)

(defun propose-qre-and-indicator (op arg task)
  (trace-msg 2 "Calling PROPOSE-QRE-AND-INDICATOR function to find a value for arg ~S" (argument-name arg))
  (let* ((commodity (find-actual-argument 'commodity task))
	 (v-id (gen-symbol "V"))
	 (t-id (gen-symbol "T"))
	 (res
	  (case (argument-name arg)
	    (indicator
	     (let ((suggested-indicator (find-possible-indicator commodity)))
	       (when suggested-indicator
		 (list (build-propose-arg-binding arg
						  (build-value-expr v-id (argument-type arg)
								    suggested-indicator) t-id task)
		       'PUSH
		       (build-agree-on-value-task arg suggested-indicator t-id)))
	       )
	     )
	    (qre
	     (list (build-propose-arg-binding arg (build-value-expr v-id (argument-type arg) 'DSSAT) t-id task)
		   'PUSH
		   (build-agree-on-value-task arg 'DSSAT t-id)))
	    )))
    (if (null res)
	(progn
	  (trace-msg 2 "No value found to propose")
	  nil)
	(progn
	  (trace-msg 2 "Decided to suggest ~S for  ~S" (car res) (argument-name arg))
	  res))
    ))


(defun do-analyze-situation (op task)
  "System takes initiative to suggest indicators if non currently known"
  (let ((indicator (find-actual-argument 'indicator task))
	(aspect (find-actual-argument 'situation-aspect task))
	(v-id (gen-symbol "V")))
    (if (and indicator aspect (im::var-p (argument-value indicator)))
	(let ((suggested-indicator (find-possible-indicator (argument-kr-value aspect))))
	  (when suggested-indicator
	    (trace-msg 2 "Decided to suggest value ~S for argument ~S" suggested-indicator (argument-value aspect))
	    (let ((t-id (gen-symbol "T")))
	      (values (build-propose-arg-binding indicator (build-value-expr v-id suggested-indicator) t-id task)
		      'PUSH
		      (build-agree-on-value-task  indicator suggested-indicator t-id)))
	    ))))
  )

(defun propose-indicator (op arg task)
  (let ((indicator (find-actual-argument 'indicator task))
	(aspect (find-actual-argument 'situation-aspect task))
	(v-id (gen-symbol "V")))
    (if (and indicator aspect (im::var-p (argument-value indicator)))
	(let ((suggested-indicator (find-possible-indicator (argument-kr-value aspect))))
	  (when suggested-indicator
	    (trace-msg 2 "Decided to suggest value ~S for argument ~S" suggested-indicator (argument-value aspect))
	    (let ((t-id (gen-symbol "T")))
	      (values (build-propose-arg-binding indicator (build-value-expr v-id
									     (argument-type arg)
									     suggested-indicator) t-id task)
		      'PUSH
		      (build-agree-on-value-task  indicator suggested-indicator t-id)))
	    ))
	))
  )


(defun find-possible-indicator (aspect)
  "should return a (value kr-value) pair"
  (case (argument-name aspect)
    (commodity
     '(PROD PROD))
    (otherwise
    '(ONT::DISEASE (MALNUTRITION)))))

(defun build-value-expr (wh-id argtype indicator)
  (if (symbolp indicator)
      (setq indicator (list indicator indicator)))
  `((ONT::DEFINITE ,wh-id :instance-of ,argtype :assoc-with yy)
    (ONT::DEFINITE yy :instance-of ,(car indicator) :CWMS (,(cadr indicator)))))

(defun system-plan (op arg task)
  (format t "~% would call the planner here")
  (setf (argument-value arg) 'myplan)
  (values))

;;  ARGUMENT VALUE INTERPRETATION

(defun canonicalize-temporal-expression (arg context)
  (let* ((v (argument-value arg))
	 (expr (find-lf-in-context v context))
	 (year (find-arg expr :year))
	 ;; MORE HERE
	 (result (or year 2020
		     )))
    (trace-msg 3 "Setting TIME argument to value ~S" result)
    (if result
	(im::match-with-subtyping (argument-kr-value arg) result)
	)
    ))
	 
    
	 


;;    MAP/LOCATION FUNCTIONS

(defun identify-gridded-locs (op arg task)
  (let* ((country-code "SDN") ;; should pull this out of the task
	 (updated-task (subst-in-task task (im::match-with-subtyping (argument-value arg) 'done))))
    (put-up-map-display country-code task)
    (list nil 'update updated-task)
    )
  )


(defun put-up-map-display (code task)
  (let ((desired-map '(file :name "/Users/james/Desktop/Work/Code/cwms/src/Data/ssd_spam_harvest_sorghum.tif"
		       :format (raster "GTIFF" 135 106)))
	(scale (argument-kr-value (find-actual-argument 'scale task)))) ;; eventually need to compute this
    (im::send-msg `(request :receiver imagedisplay :content
		    (display
		     :file ,desired-map
		     :scale "500%"))
		  ))
  )

(defun change-scale (op arg task)
 )
  
  #|(let* ((reply (send-and-wait `(request :content (get-geographic-region :description (ISO ,code ) :format (raster "GTiff" 720 360) :output (location))) ))
	 (location-file (find-arg-in-act (find-arg-in-act reply :content) :location))
	 )
    (send-msg `(request :content (display :file ,location-file)))
    )|#
  
