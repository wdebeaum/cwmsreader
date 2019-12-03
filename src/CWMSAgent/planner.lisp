;;  The CWMS PLANNER


(in-package :cwmsAgent)

;(defvar *current-plan* nil)
;(defvar *parameters* nil)
;(defvar *service-library* nil)
;(defvar *service-input-table* nil)
;(defvar *service-output-table* nil)
(defvar *top-level-cps-goals* nil)   ;; a list of active top-level goals
(defvar *active-goal* nil)
  
(defvar *goal-symbol-table* nil)  ;; all goal under discussion, whether committed or now

(defun add-to-goal-symbol-table (id goal)
  (trace-msg 3 "~%Defining new goal with ID ~S (not committed yet): ~S" id goal)
  (push (cons id goal)
	*goal-symbol-table*))

(defun get-goal-with-id (id)
  (cdr (assoc id *goal-symbol-table*)))

(defun create-goal-from-description (id description context)
  "create a goal description normalizing extent, time and locations"
  (let* ((newc (remove-unused-context-with-root id context))
	 (extent (or (find-arg description :extent) (find-arg description :extent1) (find-arg description :extent2)))
	 (time (or (find-arg description :time) (find-arg description :time1) (find-arg description :time2)))
	 (loc (or (find-arg description :loc) (find-arg description :loc1) (find-arg description :loc2)))
	 (new-description (remove-args description '(:extent :extent1 :extent2 :time :time1 :time2 :loc :loc1 :loc2))))
    (if extent (setq new-description (append new-description (list :extent extent))))
    (if time (setq new-description (append new-description (list :time time))))
    (if loc (setq new-description (append new-description (list :loc loc))))
    
    (make-goal :description new-description 
	       :type (find-arg new-description :instance-of)
	       :id id
	       :context context
	       :parameters (build-params-from-context context)
	       :status 'created)))

(defun get-arg-ids (ll)
  (every-other-one
   (cdr (remove-args (cddr ll) '(:instance-of :rule :spec :param-code :locmod :timemod :name :name-of :force :tense :type :assoc-with :epi :lex :assoc :assoc1 )))))  ;; all the ids of the arguments 


(defun build-params-from-context (context)
  (remove-if #'null
	     (mapcar #'(lambda (x)
			 (build-param x context))
		     context)))

(defun get-extent (term context)
  (let ((unit (find-arg term :unit))
	(amt (extract-value (find-arg term :amount) context)))
    (if (numberp amt)
	(/ amt 100))))

(defun build-param (term context)
  (when term
    (case (find-arg term :instance-of)
      (ont::decrease
       (let ((aff (get-from-context (find-arg term :affected) context))
	     (extent (get-from-context (find-arg term :extent) context))
	     (args (refine-arg-values (get-arg-ids
				       (remove-args term '(:agent :affected :extent :assoc :assoc1 :assoc-with )))
				      context)))
	 (multiple-value-bind (id-code value)
	     (identify-variable-code aff context)
	   (when id-code
	     (make-parameter-in-plan :id (second term)
				     :id-code (list :delta id-code)
				     :name (find-arg term :name)
				     :arguments (refine-arg-values args context)
				     :ont-type (find-arg term :instance-of)
				     :value (get-extent extent context))))))
      (ont::time-loc
       (make-parameter-in-plan :id (second term)
			       :id-code :year
			       :name (find-arg term :name)
			       :ont-type (find-arg term :instance-of)
			       :value (find-arg term :year)))
      ;; we trigger durations of the predicate rather than the quantity term
      (ont::time-duration-rel
       (let ((ground (get-from-context (find-arg term :ground) context)))
	 (make-parameter-in-plan :id (second term)
				 :id-code :year
				 :ont-type 'ONT::YEAR
				 :value (cons 'list
					      (generate-next-n-years 2018 (find-arg ground :amount))))
	 ))
      (otherwise  ;; the default case
       (multiple-value-bind (id-code value)
	   (identify-variable-code term context)
	 (let ((args (get-arg-ids term)))
	   (if (or id-code value)
	       (make-parameter-in-plan :id (second term)
				       :id-code id-code
				       :name (find-arg term :name)
				       :arguments (refine-arg-values args context)
				       :ont-type (find-arg term :instance-of)
				       :value value)
	       (format t "~%Not mapping ~S to a parameter" term))))))))


(defun refine-arg-values (ids context)
  "some arguments are transformed -- typically some relationa value is mapped ti its ground"
  (mapcar #'(Lambda (x)
	      (refine-arg-value x context)) ids))

(defun refine-arg-value (id context)
  (let ((term (get-from-context id context)))
    (case (find-arg term :instance-of)
      ((ont::time-duration-rel ont::in-loc)
       (or (find-arg term :ground)
	   id))
      (otherwise id)
      )))


(defun identify-variable-code (lf context)
  (let ((param-code (find-arg lf :param-code)))
    (if param-code
	(return-trips-code param-code)
	;; otherwise, we may chain for a few generic types
	(if (member (find-arg lf :instance-of) '(ont::QTY))
	    (identify-variable-code (get-from-context (find-arg lf :entity) context) context)
	    nil))))

(defun return-trips-code (match)
  (let ((var-codes (find-arg-in-act match :variable))
	(codes (find-arg-in-act match :code)))
    (values (keywordify (cadr (or (assoc 'trips var-codes)
				  (assoc 'icasa var-codes))))
	    (keywordify (or (cadr (assoc 'trips codes))
			    (cadr (assoc 'icasa codes)))))))

(defun get-trips-value (match)
  "find the CODE value in a MATCH if there is one"
  (if (consp match)
      (multiple-value-bind (var value)
	  (return-trips-code match)
	(or value match))
      match))

(defun generate-next-n-years (year N)
  (if (numberp N)
      (if (<= n 1)
	  (list year)
	  (cons year (generate-next-n-years (+ year 1) (- N 1))))
      (list year)))

(defun get-from-context (id context)
  (find-if #'(lambda (x) (eq (second x) id)) context))
 
(defun show-top-level-goal nil
  (let ((g (car *top-level-cps-goals*)))
    (format t "~%~%Active CPS goal is~%~S" g)
    (when (planner-state-p g)
      (let ((dg (get-goal-with-id (planner-state-content g))))
	(format t "~% with goal~S: ~S ~% type=~S  ~%:status= ~S ~%parameters=~S " 
		(goal-id dg) (goal-description dg) (goal-type dg) (goal-status dg) (goal-parameters dg))
	(when (goal-plan dg)
	  (show-plan (goal-plan dg)))))))
	  

(defun push-top-level-cps-goal (goal)
  (trace-msg 1 "~%~%Pushing top level goal: ~S~%"  goal)
  (trace-msg 2 "~%  Domain goal is ~S" (get-goal-with-id (planner-state-content goal)))
  (trace-msg 3 "~%Rest of stack is ~S" (cdr *top-level-cps-goals*))
  (push goal *top-level-cps-goals*)
  (setq *active-goal* goal)
  (setf (planner-state-status goal)  'active))

(defun push-domain-subgoal (sub parent)
   (trace-msg 1 "~%~%Pushing domain sub goal: ~S"  sub)
   (trace-msg 3 "~%Parent is ~S" parent)
   (push sub (goal-plan parent))
   (setf (goal-parent sub) parent))

(defun push-cps-subgoal (sub parent)
   (trace-msg 1 "~%~%Pushing CPS sub goal: ~S"  sub)
   (trace-msg 3 "~%Parent is ~S" parent)
   (push sub (planner-state-plan parent))
   (setf (planner-state-parent sub) parent)
   (push-domain-subgoal ))   ;; to be worked out


(defun adopt-new-goal (cps-act goal-id goal)
  (push-top-level-cps-goal
   (make-planner-state :type :achieve-domain-goal
		    :status :active
		    :content goal-id
		    )))

(defun adopt-new-subgoal (cps-act goal-id goal parent)
  (push-top-level-cps-goal
   (make-planner-state :type :achieve-domain-goal
		       :status :active
		       :content goal-id
		       :parent parent)))

(defun get-active-plan nil
  (let* ((cps-goal (get-goal-with-id (planner-state-content *active-goal*)))
	 (goal (goal-plan cps-goal)))
    (values goal cps-goal)))

(defun get-active-goal nil
  (when (planner-state-p  *active-goal*)
    (let* ((cps-goal (get-goal-with-id (planner-state-content *active-goal*)))
	   )
      cps-goal)))

(defun pop-active-goal-if-necessary nil
  (if (and (planner-state-p *active-goal*))
      (when (eq (planner-state-status *active-goal*) 'completed)
	(pop *top-level-cps-goals*)
	(setq *active-goal* (car *top-level-cps-goals*))
	(pop-active-goal-if-necessary))
      *active-goal*))

(defun mark-cps-goal-completed nil
  (setf (planner-state-status *active-goal*) 'completed))

(defun install-parameter-value-in-plan (answer-id value)
  (let* ((param (cadr (get-parameter-being-clarified answer-id)))
	 (param-id (parameter-in-plan-id param)))
    (multiple-value-bind
	  (active-plan active-goal)
	(get-active-plan)
      (if active-goal
	  (if active-plan
	      ;;  we have an ongoing plan, so add the parameter binding there
	      ;; we're ok as the parameter is in the plan
	      (let ((newplan (make-new-version-of-plan active-plan))
		    (newparam (copy-parameter-in-plan param))
		    (actual-value (if (and (consp value) (eq (car value) 'match))
				      (get-trips-value value)
				      value)))
		;; we set the value in the parameter
		
		(setf (parameter-in-plan-value newparam) value)
		(add-to-plan-symbol-table newparam newplan)
		(update-states-in-plan-with-new-bound-parameters (list param-id) 'known newplan)
		(trace-msg 2 "~% Updated value of parameter ~S.~%  New plan is ~S" newparam newplan)
		(setf (goal-plan active-goal) newplan)
		(format t "~% active goal is ~S" active-goal)
		)
	      ;; no plan so far, we are defining parameters in the goal
	      (setf (parameter-in-plan-value param) value)
	      )
	  ;; No active goal!!!!!
	  (break "NO ACTIVE GOAL: ~S" param-id)
	  )
      ;;(break "finished install parameter value in plan: plress 0 to continue")
      )))


;;  Here's the top eval CPS planning algorithm 
;;    Each different CPS goal has a cusotmized strategy

(defun continue-planning (active-goal)
  (trace-msg 4 "~%~% Continuing planning on goal: ~S" (goal-plan active-goal))
  (case (goal-type active-goal)
    (analyze-situation
     (analyze-situation active-goal))
    (read-paper
     (read-paper active-goal))
    (get-related-info
     (get-related-info active-goal))
    (add-link
     (add-link active-goal))
   
   (compute-delta-effect 
     (plan-act-compute-delta-effect active-goal))
    (analyze-aspect
     (analyze-aspect active-goal))
    (weather-effect
     (weather-effect active-goal))
    (general-question
     (general-question active-goal))
    ((solve-problem ont::solve-problem)
     (solve-problem active-goal))
    ((relate-planting-date ont::relate-planting-date)
     (relate-planting-date active-goal))
    (otherwise
     `(report :content
	      (FAILURE :what ,(goal-id active-goal) :type cannot-perform
		       :reason (unknown-goal))))))

(defun plan-is-executable (plan)
  (update-gui 'completed-plan plan)
  (trace-msg 2 "checking if plan is executable (NOTE NOT IMPLEMENTED YET!): ~S:" plan)
  T)

(defun compile-and-execute-plan (plan)
  (let* ((compiled-plan (compile-plan plan))
	(results (execute-plan compiled-plan nil)))
    results))

(defvar *compiler-ST*)

(defun compile-plan (plan)
  (trace-msg 3 "ready to compile plan: ~S:" plan)
  (let* ((*compiler-ST* nil)
	 (operators (mapcar #'(lambda (x) (get-from-symbol-table x plan))
			    (partial-plan-graph plan)))
	 
	 (code (clean-up-data-incompatabilities (gen-plan operators plan))))
    (trace-msg 2 "~%Compiled plan is ~S" code)
    code
    ))

(defun clean-up-data-incompatabilities (code)
  (when code
    (let ((next (car code)))
      (case (car next)
	(psims
	 (check-psims next (cdr code)))
	(compute-food-shock
	 (cons (replace-arg-in-act
		(replace-arg-in-act next :output (dekeywordify (find-arg-in-act next :output)))
		:SC_YEAR 2013)
	       (clean-up-data-incompatabilities (cdr code))))
	(compute-food-security
	 (let ((newact (replace-arg-in-act next :commodity '(MAZ WHT CWP LEN POT RIC OKR BNN BAR CSV CHP  CCN CFE PGP RAP SBN SWP YAM)))
	       (dconsum (find-arg-in-act next :D_CONSUM)))
	   (when (and (consp dconsum) (eq (car dconsum) 'table))
	     (setq dconsum (nth (position :D_CONSUMPTION (cadr dconsum))
				(caddr dconsum))))
	   (cons (replace-arg-in-act newact :d_consum dconsum)
		 (clean-up-data-incompatabilities (cdr code)))))
	(compute-price
	 (let ((dprod (find-arg-in-act next :D_PROD))
	       (output (find-arg-in-act next :output)))
	   (cons `(compute-price :crid wht :loc_affected wd :location wd :year 2016
				 :shock_year 2016
				 :D_prod ,dprod :output ,(dekeywordify output))
		 (clean-up-data-incompatabilities (cdr code)))))
	(otherwise
	 (cons next
	       (clean-up-data-incompatabilities (cdr code)))))
      )))

(defun check-psims (call rest)
  (let* ((loc (find-arg-in-act call :location-file))
	 (crid (find-arg-in-act call :crid))
	 (newvar (gen-symbol "?LOC"))
	 (plyr (find-arg-in-act call :plyr))
	 (newcall (replace-arg-in-act call :crid 'MAZ))
	 (output (find-arg-in-act call :output)))
    (setq newcall (replace-arg-in-act newcall :output (dekeywordify output)))
    (if (and (consp plyr) (eq (car plyr) 'list))
	(setq newcall (replace-arg-in-act newcall :plyr (cdr plyr))))
    (if (symbolp loc)
	;; we have a country code, need to call spaceman
	(cons `(get-geographic-region
	       :description (ISO ,(symbol-name loc))
	       :format (raster "GTiff" 180 90)
	       :output (:location ,newvar))
	      (cons (replace-arg-in-act newcall :location-file newvar)
		    (clean-up-data-incompatabilities rest)))
	(cons newcall
	       (clean-up-data-incompatabilities rest)))))

(defun dekeywordify (x)
  (when x
    (list* (convert-to-package (car x) *cwms-package* :convert-keywords t)
	   (cadr x)
	   (dekeywordify (cddr x)))))

(defun gen-plan (ops plan)
  (when ops
    (let* ((op (car ops))
	   (args (gen-arg-list (causal-link-from-map op) plan))
	   (output (gen-arg-list (causal-link-to-map op) plan)))
      (cons (cons (causal-link-service op)
		  (append args
			  (list :output output)))
	    (gen-plan (cdr ops) plan)))))

(defun lookup-compiler-var (id)
  (cadr (assoc id *compiler-ST*)))

(defun add-to-compiler-ST (id val)
  (push (list id val) *compiler-ST*))

(defun gen-arg-list (map plan)
  "This generates an argument list for the call, replacing the PARAMETER-IN-PLANs
     with their compiled form (a value or a variable)"
  (when map
    (let ((argname (caar map))
	  (value (or (lookup-compiler-var (cadar map))
		     (gen-parameter-value (get-from-symbol-table (cadar map) plan)))))
      (list* argname value
	     (gen-arg-list (cdr map) plan)))))

(defun gen-parameter-value (p)
  "we know the parameter is not yet in the symbol table - if it has a value we ad that, otherwise we generate a new variable"
  (if (parameter-in-plan-p p)
      (let ((value (if (parameter-in-plan-value p)
		       (convert-to-package (parameter-in-plan-value p) *cwms-package* :convert-keywords t)
		       (gen-symbol (concatenate 'string "?" (symbol-name (parameter-in-plan-id p)))))))
	(add-to-compiler-ST (parameter-in-plan-id p) value)
	value)
      (progn
	(break "~%Bad parameter found in plan: ~S" p)
	'UNKNOWN)))
      
  
;;  ACTUAL PROBLEM SOLVING CAPABILITIES

(defun plan-act-compute-delta-effect (active-goal)
  ;; we are building a plan to answer a question about the effect of some change
  (cond 
    ((eq (goal-status active-goal) 'created)
     (process-new-compute-delta-goal active-goal))
    ((and (consp  (goal-status active-goal)) (eq (car (goal-status active-goal)) 'clarification))
     (check-for-required-clarifications active-goal))
    (t (refine-plan active-goal))
    ))

(defun process-new-compute-delta-goal (active-goal)
  ;; starting planning to compute the change in the affected parameter
  ;; given thhange specified in the delta-event
  (let* ((affected-parm (get-parm active-goal :affected-parameter))
	 (affected-args (if affected-parm (remove-if #'null
						     (mapcar #'(lambda (x) (find-parm-defn-in-goal x active-goal))
							     (parameter-in-plan-arguments affected-parm)))))
	 (delta-parm (get-parm active-goal :delta-event))
	 (delta-args (if delta-parm (remove-if #'null
					       (mapcar #'(lambda (x) (find-parm-defn-in-goal x active-goal))
						       (parameter-in-plan-arguments delta-parm)))))
	 (full-affected-parameter-list (expand-with-obligatory-arguments (cons affected-parm affected-args)))
	 (full-delta-parameter-list (expand-with-obligatory-arguments (cons delta-parm delta-args)))
	 (init-state (build-state-from-parms 'initial full-delta-parameter-list))
	 (goal-state (build-state-from-parms 'goal full-affected-parameter-list))
	 (id (if affected-parm (parameter-in-plan-id-code affected-parm)))
	 (goal-code (if (and (consp id) (eq (car id) :delta))
			id
			(list :delta id))) ;; this is the code for the DELTA variable
	 ;;(possible-QSEs (find-services-from-outputs (list goal-code)))
	 )
    ;;  change the CODE for the output to be the DELTA value if necessary!
    (if (and affected-parm delta-parm)
	;; generate the empty plan
	(let* ((new-plan (create-partial-plan
			  init-state
			  goal-state
			  (append full-affected-parameter-list full-delta-parameter-list)))
	       )
	  (setf (parameter-in-plan-id-code affected-parm) goal-code)
	  (setf (goal-plan active-goal) new-plan)
	  ;;   now we check for information we know we will need, basically the time and location for the
	  ;;      affected parameter and the delta parameter
	  (or (return-clarification-for-time-and-location-if-necessary (cdr full-affected-parameter-list) (car full-affected-parameter-list)
								       goal-state new-plan active-goal)
	      (return-clarification-for-time-and-location-if-necessary (cdr full-delta-parameter-list) (car full-delta-parameter-list)
								       init-state new-plan active-goal)
	      ;; if we're good we try to plan!
	      (refine-plan active-goal)
	      ))
	;;  something was wrong, either affected or delta parms was not found
	`(report :content
		 (FAILURE :what ,(goal-id active-goal) :type cannot-perform :reason (ill-formed-delta-problem ,(goal-description active-goal))))
	)
    ))


(defun analyze-situation (active-goal)
  (if (goal-p active-goal)
      (case (goal-status active-goal)
	(created
	 (setf (goal-status active-goal) 'established)
	 (return-acknowledgement))
    
	
	)))


(defun read-paper (active-goal)
  (if (goal-p active-goal)
      (case (goal-status active-goal)
	(created
	 (setf (goal-status active-goal) 'established)
	 (return-tell-act-to-do 'read-paper-already active-goal nil)
	 ))))

(defun get-related-info (active-goal)
  (let ((graph (query-ekb 'ont::ebola)))
    (if graph
	(progn
	  (put-up-display graph)
	  (return-tell-act-to-do 'this-is-what-i-know active-goal nil))
	(return-tell-act-to-do 'know-nothing active-goal 'ont::ebola))))

(defun add-link (active-goal)
  (add-arc-to-display 'V62286 'V69716 'causes)
  (return-tell-act-to-do 'remember-that active-goal 'ont::ebola)
  )
      

(defun query-ekb (concept)
  (let ((reply (send-and-wait '(request :content (query-ekb :query (all :type ONT::EBOLA))))))
    (format t "~%~%REPLY IS ~S" reply)
    (when (consp reply)
      (let* ((result (if (eq (car reply) 'done) (find-arg-in-act reply :result))))
	result))))

(defun put-up-display (graph)
  (send-msg `(request :receiver graphdisplay :content
			   (display-ekb :ekb ,graph))))

(defun add-arc-to-display (source goal label)
  (send-msg `(request :receiver graphdisplay :content
			   (add-edge :from ,source :to ,goal :label ,label))))

	
(defun analyze-aspect (active-goal)
  (if (and (consp (goal-status active-goal))
	   (eq (car (goal-status active-goal)) 'clarification))
      (progn
	(setf (goal-status active-goal) 'created)
	(analyze-aspect active-goal))
      (case (goal-status active-goal)
	(created
	 (let ((parameter (refine-general-aspect-to-parm-id (get-parm active-goal :parameter)))
	       (parameters (expand-with-obligatory-arguments (goal-parameters active-goal))))
	   (setf (goal-parameters active-goal) parameters)
	   (or (return-clarification-for-time-and-location-if-necessary parameters parameter nil nil active-goal)
	       (let* ((plan (create-partial-plan (build-state-from-parms 'initial nil)
						 (build-state-from-parms 'goal parameters)
						 parameters))
		      (loc (find-parameter-by-code :loc parameters)))
		 (put-up-location-display loc)
		 (setf (goal-plan active-goal) plan)
		 (return-confirm-act-to-do 'pct? active-goal (if (parameter-in-plan-p loc)
									 (parameter-in-plan-value loc)))))
	   ))
	(pct?
	 (return-confirm-act-to-do 'do-baseline? active-goal nil))
	
	(do-baseline?
	    (do-baseline active-goal)
	  )
	(failed-baseline-report
	 )
	(after-baseline-report
	 )
    
    )
  ))


;;  BASELINE
(defun do-baseline (active-goal)
  ;;(break "Ready to start planning for baseline. Type 0 to continue ...")
  (let* ((parameters (mapcar #'cadr (partial-plan-symbol-table (goal-plan active-goal))))
	 (default-time-parm (find-parameter-by-code :time parameters))
	 (default-loc-parm (find-parameter-by-code :loc parameters))
	 (default-time (if (parameter-in-plan-p default-time-parm)
			   (parameter-in-plan-value  default-time-parm)))
	 (default-loc (if (parameter-in-plan-p default-loc-parm)
			  (parameter-in-plan-value  default-loc-parm)))
	 )
    (send-msg '(request :content (GENERATE :CONTENT
				  "OK. I'm running an analysis ...")))
    (let ((new-plan
	   (set-defaults-for-baseline-run active-goal default-time default-loc)))
      (trace-msg 2 "~%Plan after setting default is ~S " new-plan)
      (if (plan-is-executable new-plan)
	  (let* ((results
		  (compile-and-execute-plan new-plan))
		 (id (gen-symbol "SA"))
		 (reln-id (gen-symbol "RELN")))
	      (setf (goal-plan active-goal) new-plan)
	      (trace-msg 2 "~%Results from execution are: ~S" results)
	      (case (car results)
		(FAILURE
		 (build-failure-message 'cannot-perform result (gen-symbol "ERR") 'ont::establish-baseline)
		 (setf (goal-status active-goal) 'failed-baseline-report))
		(otherwise
		 (let* ((result (car results))
			(param-name (second result))
			(value (third result))
			)
		   (format t "~% results =~S param-name=~S value= ~S" result param-name value)
		   (when (eq (car value) 'TABLE)
		     (put-up-table-display value (list param-name :time :years) 'Percent-Malnourished))
		   (setf (goal-status active-goal) 'after-baseline-report)
		   `(PROPOSE :content (TELL  :content
					     (ASSERTION :id ,id
							:what ,reln-id
							:as (contributes-to ,(goal-id active-goal)))
					     :context ,(list (list 'ont::reln reln-id
								   :instance-of 'ONT::RESULTS
								   :value value)))
			     )))
		))
	  (let ((id (gen-symbol 'err)))
	    (build-failure-message 'cannot-perform `(FAILED-ACTION :what ,id) id 'ont::establisg-baseline))
	  ))
    )
  )

(defun weather-effect (active-goal)
  (case (goal-status active-goal)
    (created
     (setup-pct-malnour-plan active-goal)
     (return-confirm-act-to-do 'run-plan? active-goal nil))
     
    (run-plan?
     (send-msg '(request :content (generate :content "OK. I'm running the plan ...")))
      (let* ((res (compile-and-execute-plan (goal-plan active-goal)))
	     (value (find-in-answers res :PCT-MALNOUR-CHILD))
	     (id (gen-symbol "SA"))
	     (reln-id (gen-symbol "RELN")))
	(if value
	    (PROGN
	      (setf (goal-status active-goal) 'completed)
	      (mark-cps-goal-completed)
	      
	      ;;(put-up-table-display value '(:years :pct-Malnour-child) :PCT_MALNOUR_CHILD)
	      
	      `(PROPOSE :content (TELL  :content
					(ASSERTION :id ,id
						   :what ,reln-id
						   :as (contributes-to ,(goal-id active-goal)))
					:context ,(list (list 'ont::reln reln-id
							      :instance-of 'ONT::RESULTS
							      :value value)))
			))
	    ;;  didn't find the answer, return error
	    res
	    )))))

(defun find-in-answers (answers param)
  (third (find-if #'(lambda (x) (and (consp x)
				     (eq (second x) param)))
	   answers)))

(defvar *answer-given* nil)

(defun general-question (active-goal)
  "Here we handle questions - currently by looking them up on the web!"
  (if (null *answer-given*)
      (progn
	(send-msg '(request :content (generate :content "Don't know. I'm looking it up with Google!")))
	(let* ((reply (send-and-wait '(request :content (get-top-google-excerpt :search "What is the impact of El Nino in Sudan?"))))
	       (answer (find-arg-in-act reply :content))
	       (text (find-arg-in-act answer :excerpt)))
	  ;;(parse-text text)
	  (setq *answer-given* t)
	  (setf (goal-status active-goal) 'completed)
	  (mark-cps-goal-completed)
	  ;; `(REPORT :content (ANSWER :to id :value ,(find-arg-in-act answer :excerpt)))))
	  `(REPORT :CONTENT (ANSWER :TO ID :VALUE "Historically El Nino causes drought in the rainy season."))))
      `(REPORT :content (WAIT))))

(defun parse-text (text)
  (let ((tt-split-mode user::*texttagger-split-mode*)
	(cps-control im::*cps-control*)
	(im-manager im::*current-dialog-manager*)
	)
    (setq user::*texttagger-split-mode* :split-sentences)
    (setq im::*current-dialog-manager* #'im::extractsequenceIM)
    (setq im::*cps-control* nil)
    (send-msg '(request :content
		(adjust-cost-table :mods
		 ((ont::SA_CONFIRM 1.3) (ont::SA_QUERY 2) (ont::SA_request 2.5)
		  (ont::SA_YN-QUESTION 2.5)
		  (ont::SA_WH-QUESTION 2.5)
		  (w::ISOLATED-CP 1.2)
		  (w::CP 1.5)  
		  (w::VP 2)
		  (W::SA_QUERY 5)
		  (SA_IDENTIFY 1.3)))))
    (let ((*package* (find-package "USER")))
      (user::test text))
    ;; now restore the settings
    (setq user::*texttagger-split-mode* tt-split-mode)
    (setq im::*current-dialog-manager* im-manager)
    (setq im::*cps-control* cps-control
    )))
  
(defun setup-pct-malnour-plan (active-goal)
  (let* ((parms (list
		 (make-parameter-in-plan :id 'id1 :id-code :pct_MALNOUR_CHILD )
		 (make-parameter-in-plan :id 'id2 :id-code :time :value 2018)
		 (make-parameter-in-plan :id 'id4 :id-code :crid :value 'MAZ);; WHT CWP LEN POT RIC OKR BNN BAR CSV CHP  CCN CFE PGP RAP SBN SWP YAM))
		 (make-parameter-in-plan :id 'id3 :id-code :loc :value :SD)))
	 (plan (create-partial-plan (build-state-from-parms 'initial nil)
				    (build-state-from-parms 'goal parms)
				    parms)))
    (setf (goal-plan active-goal)
	  plan)
    (refine-plan-to-completion active-goal 5)
    (goal-plan active-goal)
    ))

(defun setup-planting-day-plan (active-goal)
  (let* ((parms (list
		 (make-parameter-in-plan :id 'id1 :id-code :PROD :arguments '(id2 id3))
		 (make-parameter-in-plan :id 'id2 :id-code :plyr :value '(1980 2010))
		 (make-parameter-in-plan :id 'id4 :id-code :crid :value 'MAZ)
		 (make-parameter-in-plan :id 'id3 :id-code :loc :value :SD)
		 (make-parameter-in-plan :id 'id5 :id-code :pfday :value '(+ original 7))))
	 (plan (create-partial-plan (build-state-from-parms 'initial nil)
				    (build-state-from-parms 'goal parms)
				    parms)))
    (setf (goal-plan active-goal)
	  plan)
    (refine-plan-to-completion active-goal 5)
    (goal-plan active-goal)
    ))

(defun solve-problem (active-goal)
  "currently is only exploring what to do about malnourishment"
  (case (goal-status active-goal)
    (created
     (send-msg `(REQUEST :content
		 (display-causal-influences :data ,(get-parameter-subgraph '(:PCT_MALNOUR_CHILD))
			       :label-influences nil
			       :root :PCT_MALNOUR_CHILD)))
     (return-tell-act-to-do 'displayed active-goal nil))
    #||(displayed
     ;; this is 
     (setup-pct-malnour-plan active-goal)
     (return-tell-act-to-do 'execute-plan? active-goal nil)
     )
    (execute-plan?
     (let ((res (compile-and-execute-plan (goal-plan *active-goal*)))
    )||#
    (otherwise
       (break "Now what?")
       ))
  )

(defun relate-planting-date (active-goal)
  "currently is only exploring what to do about malnourishment"
  (case (goal-status active-goal)
    (created
     (send-msg `(REQUEST :content
		 (display-causal-influences :data ,(get-path-in-parameter-graph :PFDAY :PCT_MALNOUR_CHILD)
			       :label-influences nil
			       :root :PFDAY)))
     (return-confirm-act-to-do 'execute-plan? active-goal nil))
    #||(displayed
     ;; this is 
     (setup-pct-malnour-plan active-goal)
     (return-tell-act-to-do 'execute-plan? active-goal nil)
     )||#
    (execute-plan?
     #||(setup-planting-day-plan active-goal)
     (let ((res (compile-and-execute-plan (goal-plan active-goal)))
	   )
       (break "GOT HERE: res is ~S:" res)
     ))||#
     
       (let* ((weights (get-weather-weights *weather-scenario*))
	      (loc (find-arg-in-act
		    (third (send-and-wait '(request :content (get-geographic-region :description (ISO "SD")
							      :format (raster "GTiff" 180 90)
							      :output (:location))))) :location))
	      (xx (if (null loc) (break "!!!!!")))
	      (prod0 (weighted-average (call-psims-with-planting-date nil loc :prod) weights))
	      (prod7 (weighted-average (call-psims-with-planting-date '(+ original 7) loc  :prod) weights))
	      (prod14 (weighted-average (call-psims-with-planting-date '(+ original 14) loc  :prod) weights))
	      (prod21 (weighted-average (call-psims-with-planting-date '(+ original 21) loc  :prod) weights))
	      (prod28 (weighted-average (call-psims-with-planting-date '(+ original 28) loc  :prod) weights))
	      (prod-7 (weighted-average (call-psims-with-planting-date '(- original 7) loc  :prod) weights))
	      (prod-14 (weighted-average (call-psims-with-planting-date '(- original 14) loc  :prod) weights))
	      (prod-21 (weighted-average (call-psims-with-planting-date '(- original 21) loc  :prod) weights))
	      (prod-28 (weighted-average (call-psims-with-planting-date '(- original 28) loc  :prod) weights))
	      (pairs `((-28 ,prod-28)
		       (-21 ,prod-21)
		       (-14 ,prod-14)
		       (-7 ,prod-7)
		       (0  ,prod0)
		       (7 ,prod7)
		       (14 ,prod14)
		       (21 ,prod21)
		       (28 ,prod28))))
	      
	 (put-up-table-display (list* 'table '(:pfday :prod) pairs)
			       '(:pfday :prod)
			       :productivity)
	 (format t "~%pairs before sort are ~S" pairs)
	 (let* ((ordered (sort pairs #'> :key #'(lambda (x) (second x))))
	       (day (caar ordered))
	       (production (cadar ordered)))
	   (send-msg `(REQUEST :content (generate :content ,(format nil "MAX is first planting date ~S days later with yield of ~S." day production))))
	   (format t "~%pairs are ~S" pairs)
	   )))
     (otherwise
       (break "Now what?")
       ))
  )

(defun weighted-average (values weights)
  (let ((total (apply #'+ weights)))
    (/ (cross-product values weights) total)))

(defun call-psims-with-planting-date (pfday loc output)
  (let ((res (if pfday
		 (send-and-wait `(request :content (psims :plyr (1980 2010) :crid MAZ :pfday ,pfday :location-file ,loc
							  :output (PROD))))
		 (send-and-wait `(request :content (psims :plyr (1980 2010) :crid MAZ :location-file ,loc
							  :output (PROD)))))))
    (extract-value-from-table (third (find-arg-in-act res :content)) output)))
		 
  
  
(defun build-failure-message (type reason id act-type)
  `(REPORT :content (FAILURE :TYPE ,type :what ,id :REASON ,reason)
	   :CONTEXT ((ont::reln ,id :instance-of ,act-type))))

(defun set-defaults-for-baseline-run (active-goal default-time default-loc)
  "We are given a plan and try to instantiate all currently unknown parameters to default values"
  (refine-plan-until-done active-goal default-time default-loc)
  (let* ((plan (goal-plan active-goal))
	 (parms (gather-unknown-parameters plan)))
    (mapcar #'(lambda (x) (set-default-value-for-parameter x default-time default-loc plan))
	    parms)
    plan))

(defun refine-plan-until-done (active-goal default-time default-loc)
  "calls refine plan and sets defaults until completion"
  (let ((res (refine-plan active-goal)))
    (if (consp res)
	(case (car res)
	  (report
	   (case (car (find-arg-in-act res :content))
	     (failure
	      (setf (goal-status active-goal)
		    (cons 'failed-on (cdr res)))
	      ;; return failure
	      res)
	     (otherwise
	      (break "point (5)")
	      )))
	  (propose
	   (let* ((id (find-arg-in-act (find-arg-in-act res :content) :id))
		  (parm (cadr (get-parameter-being-clarified id)))
		  )
	     (if (parameter-in-plan-p parm)
		 ;; We have at least one unknown parameter
		 (mapcar #'(lambda (x ) (set-default-value-for-parameter x default-time default-loc (goal-plan active-goal)))
			 (gather-unknown-parameters (goal-plan active-goal)))
		 #||(progn
		   (set-default-value-for-parameter parm default-time default-loc (goal-plan active-goal))
		   (refine-plan-until-done active-goal default-time default-loc))||#
		 ;;  we are done
		 T)))
	  (otherwise
	   (break "~% point (7)"))
	  )
	 (break "~% point (8)"))
    ))

(defun refine-plan-to-completion (active-goal N)
  "calls refine plan as long as it keeps successfully adding an action, and stops
    upon completion, or if there is a clarificaiton request, of the we more than N times"
  (let ((res (refine-plan active-goal)))
    (if (consp res)
	(case (car res)
	  (report
	   res)
	  
	  (propose
	   (let* ((content (find-arg-in-act res :content))
		  )
	     (if (consp content)
		 (case (car content)
		   (ask-if
		    (if (> n 0)
			(refine-plan-to-completion active-goal (- n 1))))
		   (otherwise res))
		 res)))
	  (otherwise res))
	res)))


	
(defun gather-unknown-parameters (plan)
  "This finds all the unknown parameters in a plan, except for the goal parameters"
  (let* ((states (mapcar #'(lambda (x)
			     (get-from-symbol-table x plan))
			 (cons (partial-plan-initial-state plan) (partial-plan-intermediate-states plan))))
	 (unknowns (append-unknowns states)))
    (format t "~%states are ~S unknowns are ~S" states unknowns)
    (mapcar #'(lambda (x) (get-from-symbol-table x plan))
	    unknowns)))
	 

(defun append-unknowns (states)
  (when states
    (append (state-unknowns (car states))
	    (append-unknowns (cdr states)))))

(defun set-default-value-for-parameter (parm default-time default-loc newplan)
  (let ((id-code (if (parameter-in-plan-p parm)
		     (parameter-in-plan-id-code parm))))
    (trace-msg 2 "~%Setting default for parameter ~S" id-code)
    (if (and (consp id-code) (eq (car id-code) :delta))
	(progn
	  (set-parameter-to-value-in-plan parm (list 0 0) newplan))
	(let ((val (cond
		     ((compatible-codes id-code :CRID) '(MAZ WHT CWP LEN POT RIC OKR BNN BAR CSV CHP  CCN CFE PGP RAP SBN SWP YAM))
		     ((compatible-codes id-code :TIME) default-time)
		     ((compatible-codes id-code :loc) default-loc)
		     (T
		      (break "Need to set default for parameter ~S" parm)))))
	  (set-parameter-to-value-in-plan parm val newplan)
	  ))
	))

(defun set-parameter-to-value-in-plan (param value plan)
  "Note: plan is destructively updated, so it assumes that's OK"
  (let ((p (copy-parameter-in-plan param)))
    (setf (parameter-in-plan-value p) value)
    (add-to-plan-symbol-table (list p) plan)
    (update-states-in-plan-with-new-bound-parameters (list (parameter-in-plan-id p)) 'known plan)))

(defun return-confirm-act-to-do (code active-goal arg)
  "general purpose function to build a query of the right type, and set the goal status"
  (let ((id (gen-symbol "ID")))
    (setf (goal-status active-goal) code)
    (memo-request id (list 'CONFIRM id code))
    `(PROPOSE :content (ASK-IF :id ,id
			       :query ,id
			       :as (query-in-context :goal ,(goal-id active-goal)))
	      :context ,(list (list 'ont::reln id :instance-of code :code code :arg arg)
			      )
	      )))

(defun return-tell-act-to-do (code active-goal arg)
  "general purpose function to build a query of the right type, and set the goal status"
  (let ((id (gen-symbol "ID")))
    (setf (goal-status active-goal) code)
    (memo-request id (list 'CONFIRM id code))
    `(PROPOSE :content (ont::ASSERTION  :id ,id
				   :what ,id
				   :as (contributes-to ,(goal-id active-goal)))
	      :context ,(list (list 'ont::reln id :instance-of code :code code :arg arg)
			      )
	      )))


(defun refine-general-aspect-to-parm-id (parm)
  "Refines more general terms, like food insecurity, to concrete measures like PCT_MALNOUR"
  (case (parameter-in-plan-id-code parm)
    ((:food_insecurity food_insecurity)
     (setf (parameter-in-plan-id-code parm) :PCT_MALNOUR_CHILD)))
  parm)

(defun find-parameter-by-code (code parameters)
  (find-if #'(lambda (x) (compatible-codes (parameter-in-plan-id-code x) code))
	   parameters))

(defun expand-with-obligatory-arguments (args)
  "This checks each parameter to make sure it has a time and location argument when required"
  (let* ((time-arg-found (find-parameter-by-code :time args))
	 (loc-arg-found (find-parameter-by-code :loc args))
	 (remaining-args (remove-if #'(lambda (x) (member x (list time-arg-found loc-arg-found))) args)))
    ;;(break "~% time=~S loc=~S remaining-args=~S " time-arg-found loc-arg-found
	    ;; remaining-args) 
    (if (and time-arg-found loc-arg-found
	     (null remaining-args))
	args
	(let ((time-arg (or time-arg-found (make-parameter-in-plan :id (gen-symbol "T") :id-code :time :name "default time")))
	      (loc-arg (or loc-arg-found (make-parameter-in-plan :id (gen-symbol "L") :id-code :loc :name "default loc"))))
	  (Append (mapcar #'(lambda (x) (add-time-and-loc-if-necessary x (parameter-in-plan-id time-arg) (parameter-in-plan-id loc-arg)))
			  remaining-args)
		  (list time-arg loc-arg))))))

(defun add-time-and-loc-if-necessary (parm time-id loc-id)
  (if (is-a-situated-parm parm)
      (progn
	(when (not (member time-id (parameter-in-plan-arguments parm)))
	  (setf (parameter-in-plan-arguments parm)
		(cons time-id (parameter-in-plan-arguments parm))))
	(when (not (member loc-id (parameter-in-plan-arguments parm)))
	  (setf (parameter-in-plan-arguments parm)
		(cons loc-id (parameter-in-plan-arguments parm))))
	)
      )
  parm)

(defun is-a-situated-parm (p)
  (let ((id (parameter-in-plan-id-code p)))
    (or (and (consp id) (eq (car id) :delta))
	(member id '(:fen_tot :price :prod :consum :reserves :food_insecurity
		     :pct_malnour :pct_malnour_child)))))
  
	      
(defun check-for-required-clarifications (active-goal)
  "We're still trying to establish all the required information to define the goal"
  (let* ((active-plan (goal-plan active-goal))
	 (goal-state (get-goal-state active-plan))
	 (initial-state (get-initial-state active-plan))
	 (goal-unknowns (if goal-state (rank-goals (state-unknowns goal-state) active-plan)))
	 (goal (get-from-symbol-table (car goal-unknowns) active-plan))
	 (goal-unknown-args (if goal (intersection (parameter-in-plan-arguments goal) (state-unknowns goal-state))))
	 )
    (format t "~%Goal unknown=~S  initial unk =~S" goal-unknown-args (state-unknowns initial-state))
    (if (or goal-unknown-args (state-unknowns initial-state))
	;; there are still some unknown parameters
	(or (return-clarification-for-time-and-location-if-necessary
	     (mapcar #'(lambda (x) (get-from-symbol-table x active-plan)) goal-unknown-args)
	     goal goal-state active-plan active-goal)
	    (return-clarification-for-time-and-location-if-necessary
	     (mapcar #'(lambda (x) (get-from-symbol-table x active-plan))
		     (state-unknowns initial-state))
	     nil initial-state active-plan active-goal)
	 ;; we're ready to start planning!
	    (progn
	      (setf (goal-status active-goal) 'planning)
	      (refine-plan active-goal)))
	(progn
	  (setf (goal-status active-goal) 'planning)
	  (refine-plan active-goal))
	)))

(defun return-acknowledgement nil
  '(PERFORM :content (ACK)))

(defun return-clarification-for-time-and-location-if-necessary (args parameter state plan active-goal)
  (let* ((time-arg (find-parameter-by-code :time args))
	 (loc-arg (find-parameter-by-code :loc args)))
    (or (return-clarification-if-necessary time-arg parameter state plan active-goal)
	(return-clarification-if-necessary loc-arg parameter state plan active-goal))))

(defun return-clarification-if-necessary (arg parm state plan active-goal)
  (if arg
      (if (null (parameter-in-plan-value arg))
	  (return-clarification-request arg parm state plan active-goal))
      ;; make arg!
      ))
  
(defun return-confirmation-request (cps-id cps-act cg plan active-goal)
  "build the request for confirmation about the cps-act just performed"
  (let ((service (causal-link-service cg))
	(result (get-from-symbol-table (cadr (car (causal-link-to-map cg))) plan))
	(s-id (gen-symbol "S"))
	(e-id (gen-symbol "E")))
    (memo-request cps-id (list 'CONFIRM-PLAN cps-id service))
    (setf (goal-status active-goal) '(confirmation service))
    `(PROPOSE :content (ASK-IF :id ,cps-id
			       :query ,cps-id
			       :as (query-in-context :goal ,(goal-id active-goal)))
	      :context ,(list (list 'ont::term s-id :instance-of 'ONT::SERVICE
				    :name service)
			      (list 'ont::reln cps-id :instance-of 'cps-act :action s-id :effect e-id)
			      (list'ont::term e-id :instance-of 
					      (cdr (listify result plan))))
	      )
    ))

(defun return-clarification-request (info-needed context-parm-id state plan active-goal)
  "build the CPS act to clarify the value of a parameter in context of another: e.g., the year desried for the price parameter"
  (let* ((parameter (if (parameter-in-plan-p info-needed)
			info-needed
			(get-from-symbol-table info-needed plan)))
	 (param-id (if (parameter-in-plan-p parameter)
		       (parameter-in-plan-id parameter)
		       ))
	 (context-parameter (if (parameter-in-plan-p context-parm-id)
				context-parm-id
				(or (get-from-symbol-table context-parm-id plan)
				    (find-parameter-with-arg param-id state plan))))
	 (reln-id (gen-symbol "R"))
	 (q-id (gen-symbol "G")))
    (if (not (parameter-in-plan-p context-parameter))
	(break "~%Bad 0context parameter: ~S in ~S" context-parm-id plan))
    (memo-request q-id (list 'CLARIFY-PARAMETER parameter))
    (setf (goal-status active-goal) '(clarification info-needed))
    `(PROPOSE :content (ASK-WH :id ,q-id
			       :query ,reln-id ;,param-id
			       :what ,param-id ;,reln-id
			       :as (query-in-context :goal ,(goal-id active-goal)))
	      :context ,(list (list* 'ont::term param-id :spec 'wh-term
				     :instance-of (or (parameter-in-plan-ont-type parameter)
											 (parameter-in-plan-id-code parameter))
				     (cdr (listify parameter plan)))
			      (list 'ont::reln reln-id :instance-of 'ont::ASSOC-WITH :figure param-id
				    :ground (parameter-in-plan-id context-parameter))
			      (list* 'ont::term (parameter-in-plan-id context-parameter)
				     :instance-of  (or (parameter-in-plan-ont-type context-parameter)
						       (parameter-in-plan-id-code context-parameter))
				     (cdr (listify context-parameter plan))))
	      )
    ))

(defun find-parameter-with-arg (arg state plan)
  (cadr (find-if #'(lambda (xx)
	       (let ((x (cadr xx)))
		 (and (parameter-in-plan-p x)
		      (member arg (parameter-in-plan-arguments x)))))
	   (partial-plan-symbol-table plan))))

	    
(defvar *clarification-record* nil)

(defun memo-request (id args)
  (trace-msg 2 "Memo parameter binding: ~S ~S" id args)
  (push (cons id args) *clarification-record*))

(defun get-parameter-being-clarified (id)
  (cdr (assoc id *clarification-record*)))

(defun evaluate-QSEs (QSEs goals goal-state initial-state plan)
  "checks each QSE, mainly looking at whether we know all the information required to specify the output:
     e.g., Price of wheat in 2010 in Mexico   vs    price of wheat with rime and location missing"
  (let ((goalparm (get-from-symbol-table (car goals) plan))
	(other-goal-state-parms (mapcar #'(lambda (x)
			    (get-from-symbol-table x plan))
		       (append (state-knowns goal-state) (state-unknowns goal-state) (state-achieved goal-state)))
	  )
	(initial-parms (mapcar #'(lambda (x)
			    (get-from-symbol-table x plan))
		       (append (state-knowns initial-state) (state-unknowns initial-state) (state-achieved initial-state)))
	  ))
    (mapcar #'(lambda (x) (evaluate-qse x goalparm other-goal-state-parms initial-parms (state-status goal-state) plan))
	  QSEs)
  ))
  
(defun evaluate-QSE (qse goal other-goal-state-parms initial-state-parms goal-status plan)
  "Takes a candidate QSE and returns an analysis of how well the knowledge requirements are satisfied"
  ;; identify the output parameter that corresponds to the goal
  (let* ((desired-output (find-if #'(lambda (x)
				      (compatible-codes (parameter-id-code x)
							(parameter-in-plan-id-code goal)))
				  (service-output qse)))
	 )
    (format t "desired-output=~S" desired-output)
    (if desired-output
	;; find the parameters that are the arguments 
	(let* ((essential-arguments (mapcar #'(lambda (x)
					       (find-if #'(lambda (c)
							      (eq x (parameter-name c)))
							(service-input qse)))
					    (parameter-arguments desired-output)))
	       ;;  error check -- this occurs if there's a problem with the define-service message
	       ;;   we can try to limp along by ignoring the missing argument
	       (xx (when (some #'null essential-arguments)
		     (break "error: unable to find an essential argument in ~S"  (parameter-arguments desired-output))
		     (setq essential-arguments (remove-if #'null essential-arguments))))
	       (arguments-from-goal (remove-if #'null
					       (mapcar #'(lambda (x) (get-from-symbol-table x plan))
						       (parameter-in-plan-arguments goal)))))
	  (format t "~%arguments-from-goal=~S  ~%other-goal-state-parms=~S intial state parms=~S" arguments-from-goal  other-goal-state-parms initial-state-parms)
	  ;; now try to instantiate these arguments in the states
	  (multiple-value-bind (known-essentials unknown-essentials unaligned-essentials)
	      (align-arguments essential-arguments ;;(if (eq goal-status 'goal)
						    ;;   other-goal-state-parms
						       (append arguments-from-goal other-goal-state-parms initial-state-parms))
	    (trace-msg 2 "~% Essential arguments are ~S" essential-arguments)
	    (trace-msg 2 "~% maps for knowns= ~S  maps of unknowns = ~S~%   and unaligned essentials are ~S"
		       known-essentials unknown-essentials unaligned-essentials)
	    (let* ((remaining-required-inputs
		    (remove-if #'(lambda (x)
				   (or (member x essential-arguments)
				       (not (eq (parameter-requirements x) :required))))
			       (service-input qse))))
	      (trace-msg 2 "~% REmaining required inputs are ~S" remaining-required-inputs)
	      (multiple-value-bind
		    (known-required unknown-required unaligned)
		  (align-arguments remaining-required-inputs (append initial-state-parms other-goal-state-parms))  ;; not sure if we can add the goals here!
		(trace-msg 2 "~%   of which the knowns are ~S, unknowns are ~S~% and unaligned are ~S" known-required unknown-required unaligned)
		(let* ((maps-so-far (append known-essentials
				      known-required
				      unknown-essentials
				      unknown-required))
		       (missing-essential-maps (generate-remaining-required unaligned-essentials maps-so-far))
		       (all-unknown-essentials (remove-if #'parameter-in-plan-value (mapcar #'cadr (append unknown-essentials missing-essential-maps))))
		       (remaining-required-maps (generate-remaining-required unaligned maps-so-far)))
		  #||;; bind the 
		  (setf (parameter-in-plan-arguments goal)
			(mapcar #'cadr (append known-essentials all-unknown-essentials)))||#
		  (make-candidate-qse
		   :service (service-name qse)
		   :output (list (parameter-name desired-output) (parameter-in-plan-id goal))
		   :essential-info-needed all-unknown-essentials
		   :unknown-info-needed (mapcar #'cadr remaining-required-maps)
		   :score (+ (* 2 (list-length all-unknown-essentials))
			     (list-length remaining-required-inputs))
		   :input-map (append maps-so-far
				      (mapcar #'(lambda (x)
						  (list (car x) (parameter-in-plan-id (cadr x))))
					      (append missing-essential-maps remaining-required-maps))))
		   )))))
	    ;; should not happen
	(progn (break "~%  WARNING: evaluate-qse in service ~S found no matching output parameter for ~S" qse goal)
	       (make-candidate-qse
		:service (service-name qse)
		:score 10000)))
    ))


(defun update-active-goal-knowns (active-goal new-bindings)
  "we have a list of bindings between a parameter in the REQUIRED-INPUTS and a bound parameterin the KNOWNS.
   we update the parameter lists accordingly and transfer the value to the new bound required parm"
  (when new-bindings
    (let ((p (goal-plan active-goal)))
      (setf (partial-plan-unknown-inputs p)
	    (remove-if #'(lambda (x) (assoc x new-bindings))
		       (partial-plan-unknown-inputs p)))
      (setf (partial-plan-known-inputs p)
	    (append (mapcar #'transfer-value new-bindings)
		     (partial-plan-known-inputs p))))))

(defun transfer-value (bndg)
  "PAIR is of form (X Y). sets the value in the parameter X to value of Y and returns the newly modified X"
  (setf (parameter-in-plan-value (car bndg))
	(parameter-in-plan-value (cadr bndg)))
  (car bndg))
	
(defun get-parm (goal param-name)
  (let ((param-id (find-arg (goal-description goal) param-name)))
    (find-parm-defn-in-goal param-id goal)))

(defun find-parm-defn-in-goal (id goal)
    (find-if #'(lambda (x)
	       (eq id (parameter-in-plan-id x)))
	     (goal-parameters goal)))

(defun get-parm-from-service (param-name service)
  (find-if #'(lambda (x)
	       (eq param-name (parameter-name x)))
	   (service-input service)))

(defun add-service-to-plan (causal-gap service-id output-map input-map unknown-required newplan)
  "This DESTRUCTIVEY adds a service call to NEWPLAN to fill the causal gap where the mapping from the service parameters
    to the parameters-in-plan are specified in the output-map and input-map, and creates a new causal gap if necessary"
  ;;(let* ((newplan (make-new-version-of-plan plan)))
  (trace-msg 2 "Instantiating causal link ~S with service ~S" causal-gap service-id)
  (let* ((service (find-service-by-name service-id))
	 (end-state (get-from-symbol-table (causal-link-goal causal-gap) newplan))
	 (begin-state (get-from-symbol-table (causal-link-initial-state causal-gap) newplan))
	 (linkcopy (make-causal-link :id (causal-link-id causal-gap)
				    :service (service-name service)
				    :to-map (list output-map)
				    :goal (state-id end-state)
				    :from-map input-map)))

    ;; Now we do the update
    ;; we update the states with new status for the parameters that will be set by service
    (trace-msg 4 "~%Final state before =~S. output param map is=~S" end-state
	       output-map)
    (update-states-in-plan-with-new-bound-parameters (list (cadr output-map)) 'known newplan)
    (setf (partial-plan-causal-gaps newplan)
	  (remove-if #'(lambda (x) (eq x (causal-link-id causal-gap)))  (partial-plan-causal-gaps newplan)))
    (push (causal-link-id linkcopy) (partial-plan-graph newplan))
    (mapcar #'(lambda (x) (add-to-plan-symbol-table x newplan)) unknown-required)
    ;; we now check whether the input state completely covers the input parameters needed, if not we need to create an intermediate state
    (trace-msg 3 "~%CHecking if all preconditions are satisfied: input=~S  state parms=~S coverage=~S"
	       (mapcar #'cadr input-map) (append (state-knowns begin-state) (state-knowns end-state))
	       (every #'(lambda (x) (member x (append (state-knowns begin-state) (state-knowns end-state))))
		      (mapcar #'cadr input-map)))
    (if (every #'(lambda (x) (member x (append (state-knowns begin-state) (state-knowns end-state))))
	       (mapcar #'cadr input-map))
	;; we have filled the causal gap
	(progn
	  (setf (causal-link-initial-state linkcopy) (causal-link-initial-state causal-gap))
	  (add-to-plan-symbol-table (list linkcopy) newplan)
	  )
	;;  we have partially filled the gap, we need an intermediate state
	;;  first find the parameters that we still need to bind
	(let ((unknown-inputs (remove-if #'(lambda (x)
					     (let ((p (get-from-symbol-table x newplan)))
					       (and (parameter-in-plan-p p)
						    (parameter-in-plan-value p))))
					 (mapcar #'cadr input-map))))
	  (format t "~% unknown-inputs=~S input-map=~S unknown-required=~S:" unknown-inputs input-map unknown-required)
	  ;; see if we can bind any from the initial state
	  (multiple-value-bind
		(passed-in-knowns passed-in-unknowns)
	      (split-list #'parameter-in-plan-value unknown-required)
	    (multiple-value-bind
		  (knowns unknowns)
		(split-list #'(lambda (x) (member x (state-knowns begin-state))) unknown-inputs)
	      (let* ((int-state (make-state :id (gen-symbol "I")
					    :unknowns (remove-duplicates (append unknowns (mapcar #'parameter-in-plan-id passed-in-unknowns)))
					    :knowns (remove-duplicates (append knowns
									       (mapcar #'parameter-in-plan-id passed-in-knowns)
									       (state-knowns end-state)))  ;; we move all knowns backwards (except we may want to eliinate YEAR and LOC??))
					    :status :active))
		     (new-causal-gap (make-causal-link :id (gen-symbol "CL")
						       :initial-state (state-id begin-state)
						       :goal (state-id int-state))))
		(trace-msg 2 "~% adding new causal link ~S and intermediate state ~S" new-causal-gap int-state)
		(setf (causal-link-initial-state linkcopy) (state-id int-state))
		(push (state-id int-state) (partial-plan-intermediate-states newplan))
		(push (causal-link-id new-causal-gap) (partial-plan-causal-gaps newplan))
		
		(add-to-plan-symbol-table (list int-state linkcopy new-causal-gap) newplan)
		(add-back-links new-causal-gap newplan))))
	  ))
	  
	    
    newplan
    ))

(defun add-back-links (link plan)
  "Add the back pointers from parameters to the service, destructively modifies the plan"
  (let ((from-parms (mapcar #'cadr (causal-link-from-map link)))
	(to-parms  (mapcar #'cadr (causal-link-to-map link)))
	(link-id (causal-link-id link))
	(st (partial-plan-symbol-table plan)))
    ;; do the from links
    (mapcar #'(lambda (x)
		(let ((parm (get-from-symbol-table x st)))
		  (if (parameter-in-plan-p parm)
		      (let ((newparm (copy-parameter-in-plan parm)))
			(push link-id (parameter-in-plan-links-out newparm))
			(add-to-plan-symbol-table newparm plan))
		      (format t "~%ERROR: Parameter not foundin  symbol table: ~S" x))))
	    from-parms)
    ;; do the TO links
     (mapcar #'(lambda (x)
		(let ((parm (get-from-symbol-table x st)))
		  (if (parameter-in-plan-p parm)
		      (let ((newparm (copy-parameter-in-plan parm)))
			(push link-id (parameter-in-plan-links-in newparm))
			(add-to-plan-symbol-table newparm plan))
		      (format t "~%ERROR: Parameter not found in symbol table: ~S" x))))
	    to-parms))
  )
	  

(defun add-required-unknown-to-plan (parameter plan)
  "this takes an unknown PARAMETER from a service and adds a corresponding PARAMETER-IN-PLAN to the plan, and returns an argument map for it"
  (let* ((id (gen-symbol "P"))
	 (parm
	  (make-parameter-in-plan :id id :id-code (parameter-id-code parameter)
				  :arguments (parameter-arguments parameter))))
    (push id (partial-plan-unknown-inputs plan))
    (add-to-plan-symbol-table parm plan)
    ;; return a new mapping from service parameter name to this parameter in plan
    (list (parameter-name parameter)
	  id)))

(defun instantiate-service-into-plan (candidate causal-gap plan)
  (trace-msg 2 "~%Instantiating service ~S into link ~S in plan ~S" candidate causal-gap plan)
  (let ((newplan (make-new-version-of-plan plan))
	)
    (add-service-to-plan causal-gap
			 (candidate-qse-service candidate)
			 (candidate-qse-output candidate)
			 (candidate-qse-input-map candidate)
			 (append (candidate-qse-essential-info-needed candidate)
				 (candidate-qse-unknown-info-needed candidate))
			 newplan)
    (trace-msg 2 "~%Instantiated plan is ~S" newplan)
    newplan
   ))
#||
(defun create-map-to-parm (x plan)
  "takes an unbound parameter and creates a new paremeter-in-plan with the mapping in the parameter"
  (let* ((id (gen-symbol "P"))
	(p-in-plan
	 (make-parameter-in-plan :id id
				 :id-code (parameter-id-code x)
				 :arguments (parameter-arguments x)
				 :origin x)))
    (add-to-plan-symbol-table p-in-plan plan)
    (list (parameter-name x) id)))
||#
 
(defun align-arguments (service-parameters parms)
  "This takes a list of  parameters defined in a service and a list of known parameter-in-plan's
     and creates an alignment between the two of form ((<service-arg-id> <parameter-in-plan-id>)). It returns two values: the first being the alignment to known, and the second being the remaining unknown arguments"
  ;;  very simple algorithm first that assumes the id-code uniquely identifies them

  (when service-parameters
    (multiple-value-bind (known-maps unknowns unaligned)
       	(align-arguments (cdr service-parameters) parms)
      ;; now do the first one
      (let* ((service-param-id-code (parameter-id-code (car service-parameters)))
	     (value (parameter-constant-value (car service-parameters)))
	     (corresponding-goal-parm
	      (if (null value)  ;; we don't look for a corresponding one for constants
		  (find-parameter-by-code service-param-id-code parms))))
		  		  
	(if (and corresponding-goal-parm
		 (if (parameter-in-plan-value corresponding-goal-parm)
		     (compatible-value (parameter-in-plan-value corresponding-goal-parm)
				       (parameter-id-code-constraint (car service-parameters)))))
	    (if (parameter-in-plan-value corresponding-goal-parm)
		(values (cons (list (parameter-name (car service-parameters))
				    (parameter-in-plan-id corresponding-goal-parm)) known-maps) unknowns unaligned)
		(values known-maps (cons (list service-param-id-code (parameter-in-plan-id corresponding-goal-parm)) unknowns) unaligned))
	    
	    (values known-maps unknowns (cons (car service-parameters) unaligned))
	    )))))

(defun compatible-value (value constraint)
  (cond ((null constraint)
	 ;; no constraint so we're OK
	 T)
	((symbolp constraint)
	 (eq value constraint))
	    
	((consp constraint)
	 (if (and (member (car constraint) '(range :range)) (numberp (cadr constraint)) (numberp (caddr constraint)))
	     (value-in-range value constraint)
	     (or (member value  constraint)
		 (member :WD constraint)  ;; hacks for demo
		 (member :WHT constraint))
	     ))
	))

(defun value-in-range (value constraint)
  (if (numberp value)
      (and (<= (cadr constraint) value) (<= value (caddr constraint)))
      ;; else we may be a range or list of values
      (if (consp value)
	  (let ((vals (if (member (car value) '(list range :range))
			  (cdr value)
			  value)))
	    (every #'(lambda (x) (value-in-range x constraint))
		   vals)))))
	 	

(defun find-parameter-alignments (unknowns knowns)
  "This takes the parameters still unknown and tries to bind them to known
    parameters in the plan: returns the bindings and a list of those without bindings"

  (when unknowns
    (let* ((id-code-for-unknown (parameter-in-plan-id-code (car unknowns)))
	   (corresponding-parms  (remove-if-not #'(lambda (x) (compatible-codes id-code-for-unknown (parameter-in-plan-id-code x)))
					  knowns)))
      (multiple-value-bind (new-aligns still-unknown)
	  (find-parameter-alignments (cdr unknowns) knowns)
      (if corresponding-parms
	 (values (cons (list (car unknowns) (car corresponding-parms))
		       new-aligns)
		 still-unknown)
	 (values new-aligns
		 (cons (car unknowns) still-unknown)))
      )))
  )
#||
(defun build-argmap-for-other-required-args (service-inputs existing-argmap)
  "generates argmaps for remaining required parameters that are not arguments to the output parameter"
  (let ((remaining-required (remove-if-not #'(lambda (x)
					       (and (eq (parameter-requirements x) :required)
						    (not (assoc (parameter-name x) existing-argmap))))
					   service-inputs)))
    ;; we define a new PARAMETER-IN-PLAN for each of the new required inputs
    (generate-remaining-required remaining-required)))||#

(defun generate-remaining-required (params other-maps)
  "This takes a list of parameters (unbound) and generates a list of parameter-in-plans.
     OTHER-MAPS identifies a list of other known mappings from IDs to parms"
  (let ((first-pass
	 (mapcar #'(lambda (x) 
	      (let ((id (gen-symbol "P")))
		(list (parameter-name x)
		      (make-parameter-in-plan :id id :id-code (parameter-id-code x)
					      :arguments (parameter-arguments x)
					      :value (parameter-constant-value x)))))
		 params)))
    
    (mapcar #'(lambda (x) (fix-up-arguments x (mapcar #'cadr first-pass) (append other-maps
										 (mapcar #'(lambda (x)
											     (list (car x) (parameter-in-plan-id (cadr x)))) first-pass))))
	    (mapcar #'cadr first-pass))
    first-pass  ;; now fixed
    ))

(defun fix-up-arguments (parm parms other-maps)
  "If the PARM has arguments, we destructively replace the argument code with the new parameter ID"
  (if (parameter-in-plan-arguments parm)
      (setf (parameter-in-plan-arguments parm)
	    (mapcar #'(lambda (x) (replace-code-with-id x parms other-maps))
		    (parameter-in-plan-arguments parm)))
      )
  parm)

(defun replace-code-with-id (code parms other-maps)
  (or (cadr (assoc code other-maps))
      (let ((p (find-parameter-by-code code parms)))
	(if p
	    (parameter-in-plan-id p)
	    ))))
	

(defun compatible-codes (sub sup)
  "we often have to map generic codes to more specific codes defined by the services"
  (or (equal sub sup)
      (if (and (symbolp sub) (symbolp sup))
	  (case sup
	    ((:LOC :location :loc_affected) (member sub '(:LOC :LOC_AFFECTED :LOCATION)))
	    ((:YEAR :time :YEARS-AFFECTED :SHOCK_YEAR :PLYR) (member sub '(:TIME :YEARS_AFFECTED :SHOCK_YEAR :year :time :PLYR)))
	    ((:crid :commodity) (member sub '(:crid :commodity)))
	    )
	  
	  (and (consp sub) (consp sup)
	       (eq (car sub) (car sup))
	       (compatible-codes (cadr sub) (cadr sup)))))
  )

(defun refine-plan (active-goal)
  (let* ((plan (goal-plan active-goal))
	 (cg (get-next-causal-gap plan)))
    (if cg
	(let* ((goal-state (get-from-symbol-table (causal-link-goal cg) plan))
	       (initial-state (get-from-symbol-table (causal-link-initial-state cg) plan))
	       (goals (rank-goals (state-unknowns goal-state) plan))  ; note we are ignoring any unknown args if there are any
	       (possible-QSEs (find-services-from-outputs (mapcar #'(lambda (x)
								      (let ((goal (get-from-symbol-table x plan)))
									(parameter-in-plan-id-code goal)))

								  (list (car goals)))))
	       (service-names  (mapcar #'service-name possible-QSEs)))
	  (setq possible-QSEs (if (member 'weather-approx service-names)
				  (remove-if #'(lambda (x) (eq (service-name x) 'psims)) possible-QSEs)
				  possible-QSEs))
	  (trace-msg 2 "~% SELECTED following goals to work on: ~S ~% Possible relevant services are ~S"
		     goals
		     (mapcar #'service-name possible-QSEs))
	  (if possible-qses
	      (let* ((evaluated-QSEs (sort (evaluate-QSEs possible-QSEs goals
							  goal-state
							  initial-state
							  plan) #'< :key #'candidate-qse-score))
		     (best-candidate (car evaluated-QSEs))
		     
		     (essentials (mapcar #'(lambda (x)
					     (if (parameter-in-plan-p x) x
						 (get-from-symbol-table x plan)))
					 (candidate-qse-essential-info-needed best-candidate))))
		(trace-msg 2 "~%Choosing ~S as the best service" best-candidate)
		(if (or (find-parameter-by-code :time essentials)
			(find-parameter-by-code :loc essentials))
		    ;; some essential information is missing -- we set up a clarification after adding
		    ;;  the missing parameters to the plan
		    (let* ((ids (mapcar #'(lambda (x)
					    (if (parameter-in-plan-p x) (parameter-in-plan-id x) x))
					essentials)))
		      (setf (state-unknowns goal-state) (union (state-unknowns goal-state) ids))
		      (add-to-plan-symbol-table essentials plan)
		      (return-clarification-request (or (find-parameter-by-code :time essentials)
							(find-parameter-by-code :loc essentials));; (car essentials)
						    (cadr (candidate-qse-output best-candidate))
						    initial-state plan active-goal))
		    
		    ;; if all essential parameters are defined, we add the service to the plan
		    (let ((instantiated-plan (instantiate-service-into-plan best-candidate cg
									    plan))
			  (new-cps-id (gen-symbol 'CPS)))
		      (setf (goal-plan active-goal) instantiated-plan)
		      (trace-msg 2 "~%~%Found following service ~S:" (service-name (car possible-QSEs)))
		      (trace-msg 3 "~%new instantiated plan=~S" instantiated-plan)
		      (update-gui 'new-plan instantiated-plan)
		      (return-confirmation-request new-cps-id 'ont::add-action-to-plan cg instantiated-plan active-goal)
		      )
		    ))
	      (progn
		(setf (goal-status active-goal) 'no-service)
		(trace-msg 1 "~%No service found for g ~S" goal-state)
		`(report :content (FAILURE :what ,(goal-id active-goal) :type cannot-perform
					   :reason (no-service-found-for-id ,(mapcar #'(lambda (x) (parameter-in-plan-id-code (get-from-symbol-table x plan))) goals)))))
	      ))
	'(PLAN-COMPLETE)
	)))

(defun rank-goals (unknowns plan)
  "this takes a list of unknown and ranks them, putting unknowns that are arguments to others last"
  (let* ((parms (mapcar #'(lambda (x) (get-from-symbol-table x plan)) unknowns))
	 (arguments (flatten (mapcar #'parameter-in-plan-arguments parms)))
	 (unknown-args (intersection unknowns arguments))
	 (remaining-goals (set-difference unknowns unknown-args)))
    (values remaining-goals unknown-args)))
	 
    
	  
;;  Plan maintenance: various basic operations that update plans

(defun create-partial-plan (init-state end-state parms)
  "Creates a new plan to map a set if input parameters into a set of output parameters,
    with one link that is a causal gap between them"
  (let* ((validated-parms (validate-parameters parms))
	 (link-name (gen-symbol "CL"))
	 (link (make-causal-link :id link-name :initial-state (state-id init-state)
				 :goal (state-id end-state)))
	 (newplan
	  (make-partial-plan
	   :name (gen-symbol "PLAN")
	   :version 1
	   ;;:unknown-inputs (mapcar #'parameter-in-plan-id input-names)
	   :initial-state (state-id init-state)
	   :goal-state (state-id end-state)
	   :causal-gaps (list link-name)
	   )))
   ;; (update-gui 'new-plan (partial-plan-name newplan) 1 : inputs :goals outputs)
    (add-to-plan-symbol-table (append validated-parms
				      (list init-state end-state link))
			      newplan)
    newplan))

(defun update-states-in-plan-with-new-bound-parameters (param-ids new-status newplan)
  "this is destructive: the param-id is now known, so we update all states that need it.
    This justs adds updates states to the plan symbol table if needed!"
  (mapcar #'(lambda (x)
	      (update-state-if-necessary-with-new-knowns param-ids x new-status newplan))
	  (list* (get-from-symbol-table (partial-plan-initial-state newplan) newplan)
		 (get-from-symbol-table (partial-plan-goal-state newplan) newplan)
		 (mapcar #'(lambda (x) (get-from-symbol-table x newplan))
			 (partial-plan-intermediate-states newplan)))))

(defun update-state-if-necessary-with-new-knowns (param-ids state new-status newplan)
  (when (intersection param-ids (state-unknowns state))
    (let ((newstate (copy-state state)))
      (setf (state-unknowns newstate) (remove-if #'(lambda (x) (member x param-ids))
						 (state-unknowns newstate)))
      (case new-status
	(known (setf (state-knowns newstate)
		     (append param-ids (state-knowns newstate))))
	(achieved (setf (state-achieved newstate)
			(append param-ids (state-achieved newstate)))))
      (add-to-plan-symbol-table newstate newplan)
      )))
    
	      
(defun show-plan (pp)
  (let ((ST (partial-plan-symbol-table pp)))
    (format t "~%Plan: ~S" (partial-plan-name pp))
    (format t "~% unknown parameters: ~S" (partial-plan-unknown-inputs pp))
    (format t "~% Known parameters: ~S" (partial-plan-known-inputs pp))
    (format t "~% outputs: ~S" (partial-plan-outputs pp))
    (format t "~%LINKS:")
    (mapcar #'(lambda (x) (format-link (if (causal-link-p x) x
					   (get-from-symbol-table x ST))))
	    (partial-plan-graph pp))))
    
  

(defun format-link (ll)
  (when ll
    (format t "~% ~S --~S--> ~S" 
	    (causal-link-initial-state ll) 
	    (if (service-p (causal-link-service ll))
		(service-name (causal-link-service ll))
		(causal-link-service ll))
	    (causal-link-goal ll))))
  

	  
;;  OPERATIONS ON PLANS USED DURING PLANNING	      

(defun get-next-causal-gap (plan)
  "Gets the next causal gap"
  (get-from-symbol-table (car (partial-plan-causal-gaps plan)) (partial-plan-symbol-table plan)))

#||(defun add-instantiated-link-to-plan (link plan)
  "Adds a link that has been instantiated with a service to the plan"
  (let* ((link (verify-link link plan))
	 (p (make-new-version-of plan plan))
	 (st (partial-plan-symbol-table p)))
    (multiple-value-bind 
	  (known unknown)
	(split-list #'(lambda (x) (and (parameter-in-plan-p x)
				       (parameter-in-plan-value x)))
		    (mapcar #'(lambda (x) (get-from-symbol-table (cadr x) st))
			    (causal-link-from link)))
      ;;   (format t "~%STarting plan: ~S: " plan)
      ;;   (format t "~%Spitting list: ~S:" (mapcar #'(lambda (x) (get-from-symbol-table (cadr x) st))
      ;;       (causal-link-from link)))
      ;;   (format t "~%from ~S:" (causal-link-from link))
      ;;   (format t "~%Known+~S  unknown = ~S" known unknown)
      (setf (partial-plan-unknown-inputs p)
	    (union (partial-plan-unknown-inputs p) unknown))
      (setf (partial-plan-known-inputs p)
	    (union (partial-plan-known-inputs p) known))
      (push link (partial-plan-graph p))
      (push (list (causal-link-id link) link) (partial-plan-symbol-table p))
      p)))||#

    
(defun verify-link (link-name plan)
  (if (causal-link-p link-name) link-name
      (or (get-from-symbol-table link-name (partial-plan-symbol-table plan))
	  (warn "Unknown link: ~S" link-name))))

(defun find-required-inputs (s known-inputs)
  (mapcar #'(lambda (x)
	      (list 
	       (parameter-id-code x)))
	  (remove-if-not #'(lambda (x) 
			     (and (eq (parameter-requirements x) :required)
				  (not (member (parameter-id-code x) known-inputs))))
			 (service-input s))))

(defun find-possible-one-step-decompositions (link-name plan)
  (let* ((link (verify-link link-name))
	 (inputs (causal-link-initial-state link))
	 (outputs (causal-link-goal link))
	 (possible-services-from-inputs (find-services-from-inputs inputs))
	 (possible-intermediates-from-inputs (find-intersecting-lists 
					      (mapcar #'service-output 
						      possible-services-from-inputs )))
	 (possible-services-from-outputs (find-services-from-outputs outputs))
	 (possible-intermediates-from-outputs (find-intersecting-lists 
					       (mapcar #'service-input
						       possible-services-from-outputs )))
	 (intermediates 
	  (intersection possible-intermediates-from-inputs possible-intermediates-from-outputs)))
    
    (append (find-possible-services inputs intermediates)
	  (find-possible-services intermediates outputs))))


;;  PLAN EXECUTION
;;  This executes a compiled plan and returns the variable bindings upon completion

(defun execute-plan (plan vars)
  (trace-msg 1 "~%Executing plan: ~%~S~%   with vars ~S"
	     plan vars)
  (multiple-value-bind (possible-failure ignore results)
      (execute-plan+ plan vars nil)
    (or possible-failure results)))

(defun execute-plan+ (plan vars trace)
    (if plan
      (let ((next (car plan))
	    (service nil))
	(trace-msg 3 "~%executing ~S: " next)
	(cond 
	  ((eq (car next) 'instantiate)
	   (execute-plan+ (cdr plan) (append (cdr next) vars) (cons (car next) trace)))	  
	  ((eq (car next) 'map)
	   ;;  iteration construction over a TABLE value, and builds a TABLE
	   
	   (let* ((table (instantiate-if-necessary (find-arg-in-act next :input-table) vars))
		  (map-vars (find-arg-in-act next :mapvars))
		  (output-var (find-arg-in-act next :output-table))
		  (service (find-arg-in-act next :service))
		  (new-bndgs nil))   ;; need to fix this here!!!!!!!!!!!!!!
	     

	     (if (and table map-vars output-var service)
		 (multiple-value-bind (var-names var-indices newtrace)
		     (extract-var-indices-from-table map-vars table)
		   (execute-services-with-varmap (cddr table) var-indices var-names service vars trace)
		   (execute-plan+ (cdr plan) (append new-bndgs vars) trace)
		   )
		 (format t "~%Bad format of MAP command - missing at least one arg: ~S" next))))

	  ;;  the only option left is that this is a defined service
	  ((setq service (find-service-by-name (car next)))
	   (multiple-value-bind 
		 (service-call result-pattern)
	       (instantiate-service next vars)
	     (trace-msg 2 "~%~%~%Executing step: ~S" service-call)
	     (let* ((reply (if (eq (service-component service) 'builtin)
			       (apply (car service-call) (cdr service-call))
			       (send-and-wait `(REQUEST :content ,service-call))))
		    (content (or (find-arg-in-act reply :content) reply)))
	       (trace-msg 2 "~%Processing result of execution: ~S" content)
	       (case (car content)
		 (answer
		  (let ((new-bndgs
			 (extract-results reply result-pattern)))
		    
		    (trace-msg 2 "~%Binding results: ~S" new-bndgs)
		    (multiple-value-bind (failure resultvars newtrace)
			(execute-plan+ (cdr plan) (append new-bndgs vars) (cons content trace))
		      (values failure (append new-bndgs resultvars) newtrace))))
		 (otherwise
		  (format t "~%Plan execution failed: ~S" content)
		  content))
	       )))
	   		      
	   (t
	   (format t "~%BAD step in plan: ~S" (car next))
	   nil)))
      (values nil vars trace)))

(defun instantiate-if-necessary (arg vars)
  "if its a variable we return the value" 
  (or (cadr (assoc arg vars))
      arg))

(defun execute-services-with-varmap (table-rows var-indices var-names service symbol-table trace)
  "executes the service a number of times for each value in the var-map"
  (when table-rows
    (let* ((row (car table-rows))
	   (tempbindings (gen-temp-bindings-from-row var-names var-indices row)))
      
      (multiple-value-bind (allvars result newtrace)
	  (execute-plan (list service) (append tempbindings symbol-table) trace)
	(cons (append row result)
	      (execute-services-with-varmap (cdr table-rows) var-indices var-names service symbol-table newtrace))))))

(defun gen-temp-bindings-from-row (names indices row)
  (when names
    (cons (list (car names) (nth (car indices) row))
	  (gen-temp-bindings-from-row (cdr names) (cdr indices) row))))

(defun instantiate-service (call vars)
  (multiple-value-bind (args results)
      (instantiate-args (cdr call) vars)
    (values (cons (car call) args) results)))

(defun instantiate-args (args vars)
  (when args
    (multiple-value-bind (newargs results)
	(instantiate-args (cddr args) vars)
      (if (eq (car args) :output)
	  (values (list* :output (every-other-one (cadr args))
			 newargs) (cadr args))
	  ;; otherwise a standard input parameter - bind variable if present
	(let ((val (construct-param-value (car args) (cadr args) vars)))
	  (values (list* (car args) 
			 (or val (cadr args))
			 newargs)
		  results))))))

(defun construct-param-value (param val vars)
  "construct the value for PARM starting from the compile VAL -
which may be a variable that needs binding, which may be a table that needs simplifying"
  (let ((v (or (cadr (assoc val vars))   ;; first look up on var symbol table
	       val)))
    ;; now see if its a case where we need to extrct the desired value from a table
    (if (member param '(:D_CONSUM))
	(if (and (consp v) (eq (car v) 'TABLE))
	    (let ((p (position :D_CONSUMPTION (cadr v))))
	      (if p
		  (let ((newv (mapcar #'(lambda (x) (nth p x)) (cddr v))))
		    (trace-msg 3 "Simplifying value of ~S from table ~S" param v)
		    (if (and (consp newv) (eq (list-length newv) 1))
			(car newv)
			(cons 'list newv)))
		  v))
	    v)
	v)
    ))
		  
    

(defun extract-results (reply result-vars)
  (let ((answer-args (or (cdr (find-arg-in-act reply :content))
			 (cdr reply))))
    (if answer-args
	(extract-results+ answer-args
		      result-vars))))

(defun extract-results+ (outputs result-vars)
  (when result-vars
    (let ((result (cadr (member (keywordify (car result-vars)) outputs))))
      (cons (list (cadr result-vars) result)
	    (extract-results+ outputs (cddr result-vars))))))

(defun every-other-one (ll)
  (when ll
	(cons (car ll) (every-other-one (cddr ll)))))

(defun extract-var-indices-from-table (var-map table)
  "Extracts a list of the values for VAR in the TABLE"
  (if (eq (car table) 'TABLE)
      (find-var-index var-map (cadr table))
      (format t "~%extract-var-indices-from-table: Bad TABLE: ~S" table)))
	    
(defun find-var-index (map-vars table-vars)
  (when map-vars
    (multiple-value-bind (names indices)
	(find-var-index (cddr map-vars) table-vars)
    (values (cons (cadr map-vars) names)
	    (cons (position (car map-vars) table-vars)
		  indices)))))


(defvar *weather-scenario* 'elnino)

(defun weather-estimator (&key prod crid plyr plyr1 location-file output)
  (format t "~% prod=~S" prod)
  (if (and (consp prod) (eq (car prod) 'table))
      (let* ((n (position :prod (cadr prod)))
	     (yearposn (position :plyr (cadr prod)))
	     (prods (mapcar #'(lambda (x) (nth n x)) (cddr prod))))
	(multiple-value-bind (scenario-weight-vector scenario-sum)
	    (get-weather-weights *weather-scenario*)
	  (multiple-value-bind (weight-vector sum)
	      (get-weather-weights 'baseline)
	    (let ((scenario-estimate (/ (cross-product prods scenario-weight-vector) scenario-sum))
		  (baseline-estimate (/ (cross-product prods weight-vector) sum)))
	      (format t "~% elnino=~S baseline=~S" scenario-estimate baseline-estimate)
	      (send-msg '(request :content (GENERATE :CONTENT
					    "OK. This graph shows the expected difference in production.")))
	      (put-up-table-display `(table (:plyr :baseline :elnino)
					    (2018 ,baseline-estimate ,scenario-estimate))
				    '(:plyr :baseline :elnino) :PRODUCTION)
	      `(report :content
		       (answer :D_PROD ,(* 100 (/ (- scenario-estimate baseline-estimate) baseline-estimate)))
		       )))))))

(defun extract-value-from-table (table param)
  (if (and (consp table) (eq (car table) 'table))
      (let* ((n (position :prod (cadr table))))
	(mapcar #'(lambda (x) (nth n x)) (cddr table))))
  )

(defvar *Weather-estimate-distributions*
  '((baseline (.739 .747 .754 .62 .770 .777 .785 .793 .801 .809 .817 .826 .834 .842 .851 .860 .868 .877 .886 .895 .904 .913 .922 .932 .941 .951 .960 .970 .980 .99 1))
    (elnino (.739 .747 3.018 3.049 .770 .777 .785 3.174 .801 .809 .817 3.304 3.338 .842 .851 .860 .868 3.510 3.545 .895 .904 .913 .922 .932 .941 .951 .960 .970 .980 .99 4)))
  )

(defun get-weather-weights (type)
  (let ((v (cadr (assoc type *Weather-estimate-distributions*))))
    (values v (apply #'+ v))))

(defun cross-product (x y)
  (if (and x y)
    (+ (* (car x) (car y))
       (cross-product (cdr x) (cdr y)))
    0))
	
  

(defun update-gui (operation plan &key unknowns knowns goals operator param)
  "send message to GUI"
  (let* ((listified-plan (listify plan plan))
      #||	  (known-list (mapcar #'listify knowns))
	 (unknown-list (mapcar #'listify unknowns))
	 (goal-list (mapcar #'listify goals))
	 
	 (msg (case operation
		((new-plan new-parameters)
		 (list operation
		       :id plan-id
		       :version version
		      ;; :unknowns unknown-list
		       :knowns known-list
		       :goals goal-list))
		(new-operator
		 (list 'new-operator
		       :id plan-id
		       :version version
		       :operator (listify operator)
		       :goals goal-list))
		(bind-parameter
		 (list 'bind-parameter
		       :id plan-id
		       :version version
		       :param (listify param)))
		       
	 )))||#
	 (msg (cons 'new-plan listified-plan)))
    (format t "~%~%==========~~%~%~S" msg)
    (send-msg `(request :content (update-plan-gui :content ,msg)))))

(defun listify (x plan)
  (cond
    ((parameter-in-plan-p x)
     (list 'parameter 
	   :id (parameter-in-plan-id x)
	   :id-code (parameter-in-plan-id-code x)
	   :arguments (parameter-in-plan-arguments x)
	   :links-in (parameter-in-plan-links-in x)
	   :links-out (parameter-in-plan-links-out x)
	   :value (simplify-value (parameter-in-plan-value x))
	   ))
    ((parameter-p x)
     (list 'parameter 
	   :id (parameter-name x)
	   :id-code (parameter-id-code x)
	   :arguments (parameter-arguments x)
	   ))
    ((causal-link-p x)
     (list 'operator 
	   :id (causal-link-id x)
	   :from (causal-link-initial-state x)
	   :to (causal-link-goal x)
	   :service  (causal-link-service x)
	   :from-map (causal-link-from-map x)
	   :to-map (causal-link-to-map x)
	   ))
    ((state-p x)
     (list 'state
	   :id (state-id x)
	   :unknowns (mapcar #'(lambda (y) (listify (get-from-symbol-table y plan) plan))
			     (state-unknowns x))
	   :knowns (mapcar #'(lambda (y) (listify (get-from-symbol-table y plan) plan))
			   (state-knowns x))
	   :status (state-status x)))
    ((partial-plan-p x)
     (list 'plan
	   :id (partial-plan-name x)
	   :version (partial-plan-version x)
	   :goal-state (listify (get-from-symbol-table (partial-plan-goal-state x) plan) plan)
	   :initial-state (listify (get-from-symbol-table (partial-plan-initial-state x) plan) plan)
	   :intermediate-states (mapcar #'(lambda (z)
					   (listify z plan))
				       (mapcar #'(lambda (y) (get-from-symbol-table y plan))
					       (partial-plan-intermediate-states x)))
	   :causal-gaps (mapcar #'(lambda (z)
				    (listify z plan))
				(mapcar #'(lambda (y) (get-from-symbol-table y plan))
					(partial-plan-causal-gaps x)))
	   :graph (mapcar #'(lambda (z)
			      (listify z plan))
			  (mapcar #'(lambda (y) (get-from-symbol-table y plan))
				  (partial-plan-graph x)))
	   ))

    ))

(defun simplify-value (m)
  (if (and (consp m) (eq (car m) 'match))
      (multiple-value-bind (var value)
	  (return-trips-code m)
	(or value m))
      m))
	   
     
 ;;   What happens to the price of wheat 
(defvar *sample-plan* nil)

(setq *sample-plan*
      '((instantiate
	 (?CRID WHT)
	 (?PLYR 2009)
	 (?LOC_DESCRIPTION "United States")
	 (?LOC_AFFECTED_DESCRIPTION "Mexico")
	 (?YEAR_AFFECTED 2010 )
	 (?FEN_TOT  (/ original 2) )   ;; should change to (DELTA  (/ original 2))
	 )

	(get-geographic-region
	 :description ?LOC_DESCRIPTION
	 :format (raster "GTiff" 180 90)
	 :output (location ?LOCATION)              ;; planner has a variable LOCATION that stores this value
	 )

	(get-geographic-region
	 :description ?LOC_DESCRIPTION
	 :format (code "ISO")
	 :output (location ?LOC_CODE)
	 )

	(get-geographic-region
	 :description ?LOC_AFFECTED_DESCRIPTION
	 :format (code "ISO")
	 :output (location ?LOC_AFFECTED_CODE)            ;; planner has a variable LOCATION that stores this value
	 )
	
     ;; (ANSWER :LOCATION
     ;;         (FILE :NAME
     ;;              "/Users/cmteng/tripsSystems/etc/Spaceman/cache/impact/Region-US-180x90.gtiff"
     ;;                :FORMAT (RASTER "GTiff" 180 90))))
     ;;        :SENDER SPACEMAN)
	;;
	(psims
	 :location-file ?LOCATION
	 :crid ?CRID
	 :plyr ?PLYR
	 :fen_tot ?FEN_TOT  
	 :output (D_PROD  ?D_PROD)
	 )

	;;  (reply :content (report :content (answer
	;;   :file ".../cache/psims019.nc4"
	;;   :aggregated-file ".../cache/psims019.agg.nc4"
	;;   :prod (1.365953071 1.324798269 1.809672718)
	;;   :d_prod (TABLE (:d_prod :crid :plyr)
	;;                  ((-14.950682094463055 WHT 2008) (-13.855892219576718 WHT 2009)
	;;                   (-24.34794112614599 WHT 2010))

#||	(MAP :mapvars (:d_prod ?D :plyr ?Y) 
	 :input-table ?D_PROD
	 :output-table ?D_CONSUM
	 :service||#
	 (compute-price 
	      :crid ?CRID
	      :location WD 
	      :shock_year ?PLYR
	      :d_prod -8.6
	      :year ?YEAR_AFFECTED
	      :output (price ?PRICE d_consumption ?CP-CONSUM))

  
	;;  does multiple calls and produces a new table 
	;;   ?D_CONSUMPTION  (TABLE (:d_consumption :d_prod :sc_year)  ....
	;;                         

	 (compute-food-security
	  :D_consum -.39
	  :commodity ?CRID
	  :shock-years ?PLYR
	  :years ?YEAR_AFFECTED
	  :cty ?LOC_AFFECTED_CODE
	  :output (POP_HUNGER ?POP_HUNGER PCT_PREVALENT_HUNGER ?PCT_PREVALENT_HUNGER 
			      PCT_MALNOUR_CHILD ?PCT_MALNOUR_CHILD))

	 #||	(MAP :mapvars (:d_prod ?D :plyr ?Y)  
	 :input-table ?D_PROD
	 :output-table ?D_PROD1
	 :service||#
	 (COMPUTE-FOOD-SHOCK 
	  :CRID ?CRID 
	  :LOCATION ?LOC_CODE
	  :SC_YEAR 2009
	  :D_PROD -8.6
	  :LOC_AFFECTED ?LOC_AFFECTED_CODE
	  :OUTPUT (D_CONSUMPTION  ?CFS_CONSUM))

#||
	 (display 
	  :values (?POP_HUNGER ?PCT_PREVALENT_HUNGER ?PCT_MALNOUR_CHILD)
	  :as MAP
	  :location ?LOC_AFFECTED-CODE
	  )||#
	 ))
	
