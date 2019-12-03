;;  Managing the TASK Library

(in-package :cwmsAgent)

;;=====================================
;; Reading the Library

(defmacro define-task (&rest args)
  `(apply #'actual-define-task ',args))

(defun actual-define-task (name &key (description nil) (status nil)
				  (task-type nil) (patterns nil) (arguments nil) (inherits-from nil) (constraints nil) (subtasks nil) (built-in nil) (code nil) (id nil)
				  (returns nil) (completion-criteria nil))
  ;;(trace set-result-vars find-argument-var find-actual-argument)
  ;;(trace check-patterns)
  (if (not (eq task-type 'abstract) )
      (progn
	;; usual case, we're setting up the structures
	(im::init-var-table)
	(push (list name (set-up-internal-structures (make-task :name name
								:id id
								:status status
								:task-type task-type
								:patterns (if (check-patterns patterns name)
									      (im::read-value patterns nil))
								:arguments (mapcar #'read-argument
										   (if inherits-from
										       (append arguments
											       (gather-inherited-arguments inherits-from))
										       arguments))
								:inherits-from inherits-from
								:constraints (im::read-value constraints nil)
								:subtasks (mapcar #'read-subtask subtasks)
								:built-in (eval built-in)
								:returns returns
								:completion-criteria (if completion-criteria (im::read-value completion-criteria nil)
											 (format t "~%WARNING: No completion criteria defined for task ~S" name))) 
								
						     ))
	      *all-tasks*))
      ;; otherwise, with abstract tasks we simply store them to be copied later
      (push (list name 
		  (make-task :name name
			     :id id
			     :description description
			     :status status
			     :task-type task-type
			     :patterns patterns
			     :arguments arguments
			     :inherits-from inherits-from
			     :constraints constraints
			     :subtasks subtasks
			     :built-in built-in
			     :returns returns
			     :completion-criteria completion-criteria)
		    )
	     *all-tasks*))
     
  (cond ((eq task-type 'top)
	 (push name *top-level-tasks*))
	))

(defun check-patterns (patterns taskname)
  "checks that each pattern has the correct structure and the required :instance-of declaration"
  (let ((*fail* nil))
    (if (consp patterns)
	(every #'(lambda (x)
		   (and (consp x)
			(if (every #'consp 
				   x)
			    (if (every #'(lambda (z) 
					   (member :instance-of z)) x)
				T
				(progn
				  (format t "~%ERROR: pattern in task ~S does not contain an :instance-of slot: ~S" taskname x)
				  (setq *fail* t)))
			    (progn
			      (format t "~%ERROR: patterns in task ~S is not a list of patterns: ~S" taskname x)
			      (setq *fail* t))
			    )))
	       patterns)
	(if patterns
	    (progn (setq *fail* t)
		   (format t "~%ERROR: patterns in task ~S is not a list: ~S" taskname patterns)))
	)
    (not *fail*)
    ))


(defun set-up-internal-structures (task)
  "adds bookkeepping info to new task"
  (mapcar #'(lambda (a) (set-result-vars a task))
	  (task-subtasks task))
 ;; (mapcar #'(lambda (a) (add-basic-arg-pattern a task))
;;	  (task-arguments task))
  task)

(defun set-result-vars (sub task)
  (let ((vars (remove-if #'null
			 (mapcar #'(lambda (x) (find-argument-var x task))
				 (subtask-output sub)))))
    (if vars
	(setf (subtask-vars-that-need-binding sub)
	      vars)
	(format t "~%WARNING: output var in subtask ~S not found in task ~S" sub task))))

(defun find-argument-var (map task)
  (let ((arg (find-actual-argument (cadr map) task)))
    (when arg (argument-value arg))
    ))
#|
(defun add-basic-arg-pattern (arg task)
  (let ((v (argument-value arg))
	(type (argument-type arg)))
    (when (and (im::var-p v) type)
      (push (list (list (im::read-value '?spec nil)
		      v
		      :instance-of
		      type))
	    (argument-patterns arg)))))|#

(defun read-argument (args)
  (apply #'read-argument1 args))

(defun read-argument1 (a &key (name nil) (type nil) (value nil) (requirements nil) (patterns nil) (produced-by nil) (kr-value nil)
			   (description nil) (when-filled nil))
  (make-argument :name name
		 :description description
		 :type type
		 :value (or (im::read-value value nil) (im::make-var :name 'val))
		 :kr-value (or (im::read-value kr-value nil) (im::make-var :name 'krval))
		 :requirements requirements
		 :patterns (if (check-patterns patterns name)
			       (im::read-value patterns nil))
		 :produced-by (interpret-subtask-function-spec produced-by)
		 :when-filled  (interpret-function-spec when-filled)
		 ))

(defun interpret-subtask-function-spec (x)
  (when (consp x)
    (case (car x)
      (subtask x)
      (funct (list 'funct (if (symbolp (cadr x))
			      (format t "~%ERROR: bad function, must be a lambda expression: ~S" x)
			      (eval (cadr x)))))
      (otherwise
       (format t "Warning: bad produced-by value found: ~S" x)))))

(defun interpret-function-spec (x)
  (when (consp x)
    (case (car x)
      (funct (list 'funct (if (symbolp (cadr x))
			      (format t "~%ERROR: bad function, must be a lambda expression: ~S" x)
			      (eval (cadr x)))))
      (otherwise
       (format t "Warning: bad when-filled value found: ~S" x)))))

(defun read-subtask (args)
  (apply #'read-subtask1 (im::read-value args nil)))

(defun read-subtask1 (a &key (name nil) (input nil) (output nil))
  (let ((output (mapcar #'expand-argument-if-necessary output)))
  (make-subtask :name name
		:input (mapcar #'expand-argument-if-necessary input)
		:output output
		)))

(defun expand-argument-if-necessary (x)
  (if (consp x)
      x
      (if (symbolp x)
	  (list x x)
	  (format t "~%ERROR: bad argument spec: ~S" x))))
	

(defun gather-inherited-arguments (task-id)
  (let ((task-instance (get-task-by-name task-id)))
    (if (task-p task-instance)
	(if (task-inherits-from task-instance)
	    (append (task-arguments task-instance)
		    (gather-arguments (get-task-by-name (task-inherits-from task-instance))))
	    (task-arguments task-instance))
	(progn
	  (format t "~%~% WARNING: task ~S, needed for inheritance, is not defined" task-id)
	  nil))))
	   
;; Managing the task stack

(defvar *task-stack* nil)

(defun push-task (task)
  (push task *task-stack*)
  task)

(defun pop-stack nil
  (pop *task-stack*))

(defun top-of-task-stack nil
  (car *task-stack*))

(defun instantiate-subtask (parent subspec newid)
  (let* ((subtasktemplate (get-task-by-name (subtask-name subspec)))
	 (subtask (if subtasktemplate (copy-task subtasktemplate)
		      (format t "Warning: Unknown subtask: ~S" (subtask-name subspec)))))
    (if subtask
	(let* ((bndgs (get-bindings-to-instantiate-args subtask parent (subtask-input subspec)))
	      (newtask (subst-in-task subtask bndgs)))
	  (setf (task-id newtask) newid)
	  (setf (task-returns newtask) (subtask-output subspec))
	  newtask
	  ))))

(defun replace-top-task (newtask)
  "This updates the top task with a new copy"
  (if (eq (task-name newtask) (task-name (top-of-task-stack)))
      (progn
	(pop-stack)
	(push-task newtask)
	newtask)
      (format t "~%Warning: attempt to replace top task with incompatible replacement: ~S" newtask)
      ))

(defun return-from-top-task (bndgs)
  "This pops the top task off the stack after instantiating the output arguments down into the lower task"
  (let* ((task-to-pop (top-of-task-stack))
	 (subtask (subst-in-task task-to-pop bndgs))
	 (parenttask (second *task-stack*))
	 (returns (task-returns subtask))
	 (bndgs (get-bindings-to-instantiate-args parenttask subtask returns)))
    (pop-stack)
    (bind-top-task bndgs)))

(defun bind-top-task (bndgs)
  "binds each argument in the return list to the value"
  (when bndgs
    (let* ((task (top-of-task-stack))
	   (newtask (subst-in-task task bndgs)))
      (if (not (eq task newtask))
	  (replace-top-task newtask)
	       
      ))))

(defun get-bindings-to-instantiate-args (newtask sourcetask input-spec)
  "this returns the bindings to instantiates the arguments of a new task that are specified in a source task according to the argument specification"
  (when input-spec
    (let* ((newtaskarg (find-actual-argument (caar input-spec) newtask))
	   (sourcearg (find-actual-argument (cadar input-spec) sourcetask)))
      (if (and (argument-p newtaskarg) (argument-p sourcearg))
	  (let (
		(bndgs (append (im::match-with-subtyping (argument-value newtaskarg) (argument-value sourcearg))
			       (im::match-with-subtyping (argument-kr-value newtaskarg) (argument-kr-value sourcearg))))
		)
	    (if bndgs
		(append bndgs (get-bindings-to-instantiate-args newtask sourcetask (cdr input-spec)))
		(progn
		  (format t "Warning: input ~S fails to bind argument in task ~S. It it ignored" input-spec sourcetask)
		  (get-bindings-to-instantiate-args newtask sourcetask (cdr input-spec))))
	    )
	  (get-bindings-to-instantiate-args newtask sourcetask (cdr input-spec)
					    )))))

(defun find-actual-argument (name task)
  (find-if #'(lambda (x) (eq (argument-name x) name))
	   (task-arguments task)))

(defun find-actual-subtask (name task)
  (find-if #'(lambda (x) (eq (subtask-name x) name))
	   (task-subtasks task)))


;;   Matching an input into the task hierarchy
;;   Given the LF of the user utterance, we try to interpret it with respect to the current context
;;  e.g., matching the task patterns to identify a new task or subtask,
;;        or matching an argument pattern to instantiate an argument

(defun match-into-task (lfs task)
  (let ((results (remove-if #'null (mapcar #'(lambda (x) (match-lfs-to-pattern lfs x))
				       (task-patterns task)))))
    (if results
	(append (car results)
		(flatten-once (mapcar #'car
				      (bndgs-from-match-lfs-against-arguments LFS task))))
	))
  )

(defun flatten-once (ll)
  (if (and (consp ll) (consp (car ll)))
      (append (car ll) (flatten-once (cdr ll)))
      ll))

(defun match-lfs-to-pattern (lfs pattern)
  ;; currently we are not binding the variables before going on -so this needs to be fixed later
  (when pattern
    (trace-msg 3 "Attempting to match LF ~S against pattern ~S" lfs pattern)
    (let ((bndgs (some #'(lambda (x) (im::lf-match (car pattern) x))
			  lfs)))
      (trace-msg 2 "~%Bindings are ~S" bndgs)
      (if bndgs
	  (if (cdr pattern)
	      (let ((newbndgs (match-lfs-to-pattern lfs (cdr pattern))))
		(if newbndgs (append bndgs newbndgs)
		    (trace-msg 2 "Matching failed"))
		)
	      bndgs)
	  ))))

;;  match the LF into the frontier on the task tree and return all the possibilities
(defun match-lfs-into-possible-tasks (LFS tasks)
  (let ((Possible-tasks (remove-if #'null
				   (mapcar #'(Lambda (x)
					       (let ((res (match-into-task lfs X)))
						 (if res (list x res))))
					   tasks))))
    (trace-msg 2 "~%Matching utterance into task: ~S" tasks)
    (trace-msg 3 "~%LFS are ~S" LFS)
    (trace-msg 2 "~%Possible matches are ~S:" (mapcar #'car possible-tasks))
    (trace-msg 3 "~%Bindings are ~S" possible-tasks)
    ;;(setq *possible-tasks* possible-tasks)
    (values
     ;; the result
     (cond ((null possible-tasks) 'unacceptable)
	   ((eq (list-length possible-tasks) 1) 'acceptable)
	   (t 'ambiguous))
     ;; the task info -- always a list of possible tasks, even when only one
     (if (eq (list-length possible-tasks) 1)
	possible-tasks ;;(car possible-tasks)
	 possible-tasks))
    ))

(defun bndgs-from-match-lfs-against-arguments (lfs task)
  (multiple-value-bind (result bndgs)
      (match-lfs-against-arguments lfs task)
    (if (eq result 'acceptable)
	(mapcar #'cadr bndgs))))

(defun match-lfs-against-arguments (lfs task)
  (let* ((arguments (task-arguments task))
	 (unbound-arguments (if arguments
			       (remove-if-not #'(lambda (x) (im::var-p (argument-value x)))
					      arguments)))
	 (matches (remove-if #'null
			     (mapcar #'(lambda (arg)
					 (remove-if #'null
						    (mapcar #'(lambda (pat)
								(match-lfs-to-pattern LFS pat))
							    (argument-patterns arg))))
				     unbound-arguments))))
    (format t "Argument Match results are ~S" matches)
    (values (if matches 'acceptable) (mapcar #'(lambda (x) (list task x)) matches))))

(defun instantiate-task (task bndgs id)
  (if (not (task-p task))
      (setq task (get-task-by-name task)))
  (if (task-p task)
      (let ((newtask (subst-in-task task bndgs)))
	(if id (setf (task-id newtask) id))
	newtask)
      (format t "~%ERROR in instantiate-task: no such task: ~S" task)))

(defun subst-in-task (task bndgs)
  "instantiates bound variables in the task - returning a new copy"
  (when (task-p task)
    (if bndgs 
	(make-task :name (task-name task)
		   :id (task-id task)
		   :description (task-description task)
		   :status (task-status task)
		   :task-type (task-task-type task)
		   :patterns (im::subst-in (task-patterns task) bndgs)
		   :arguments (mapcar #'(lambda (a) (subst-in-argument a bndgs))
				      (task-arguments task))
		   :inherits-from (task-inherits-from task)
		   :constraints (im::subst-in (task-constraints task) bndgs)
		   :subtasks (mapcar #'(lambda (tt) (subst-in-subtask tt bndgs))
				     (task-subtasks task))
		   :built-in (task-built-in task)
		   :returns (im::subst-in (task-returns task) bndgs)
		   :completion-criteria (im::subst-in (task-completion-criteria task) bndgs)
		   )
	
	task)))

(defun subst-in-argument (a bndgs)
  (when (argument-p a)
    (make-argument :name (argument-name a)
		   :description (argument-description a)
		   :type (im::subst-in (argument-type a) bndgs)
		   :value (im::subst-in (argument-value a) bndgs)
		   :kr-value (im::subst-in (argument-kr-value a) bndgs)
		   :requirements (argument-requirements a)
		   :patterns (im::subst-in (argument-patterns a) bndgs)
		   :produced-by (argument-produced-by a)
		   :when-filled (argument-when-filled a)
		   :value-description (im::subst-in (argument-value-description a) bndgs)
    )))

(defun subst-in-subtask (st bndgs)
  (when (subtask-p st)
    (make-subtask
     :name (subtask-name st)
     :input (im::subst-in (subtask-input st) bndgs)
     :output (im::subst-in (subtask-output st) bndgs)
     :vars-that-need-binding (im::subst-in (subtask-vars-that-need-binding st) bndgs))
    ))

;; TOP LEVEL CODE

(defun evaluate-adopt-goal (root lfs as)
  (if (null *task-stack*)
      (if (eq (car as) 'subgoal)
	  (values 'failed-TO-INTERPRET '((:TYPE :FAILED-TO-INTERPRET REASON :NO-CURRENT-GOAL)))
	  ;; otherwise, The frontier is set to the top level tasks
	  (match-lfs-into-possible-tasks LFS (mapcar #'get-task-by-name *top-level-tasks*)))
      ;;  otherwise, *task-stack* is set
      (let ((current-task (car *task-stack*)))
	(if (eq (car as) 'subgoal)
	    (if (task-subtasks current-task)
		(multiple-value-bind (status possible-subtasks)
		    (match-lfs-into-possible-tasks LFS (mapcar #'(lambda (x)
							       (instantiate-subtask current-task x (gen-symbol "I")))							  
							       (task-subtasks current-task)))
		  (values status possible-subtasks))
		;;  no subtasks, so try matching the arguments
		(match-lfs-against-arguments lfs current-task))
	    (match-lfs-against-arguments LFS current-task))
	))
  )

(defun evaluate-answer (content context)
  (let ((value (find-arg-in-act content :value)))
    (match-lfs-into-possible-tasks (remove-unused-context-with-root value context)
				   (list (top-of-task-stack))))
  )

      
(defun commit-goal (task-name bndgs as)
 ;; (case (car as)
     (push (instantiate-task task-name bndgs nil)
	   *task-stack*))

(defun evaluate-assertion (content context)
  (let ((current-task (top-of-task-stack)))
    (if current-task
	(match-lfs-against-arguments context current-task)
	(progn
	  (trace-msg 2 "Assertion found with empty task stack: ~S. Failing ..." content)
	  nil))))

;; WHAT NEXT

(defun what-next-in-task (task1 context)
  "explores ways to continue the current task"
  (if (task-p task1)
      (progn
	(when context (setf (task-description task1) (append (task-description task1) context)))
	(let ((task (Process-new-information-in-arguments task1)))
	  (trace-msg 2 "Decision on WHAT-NEXT on task ~S:~S" (task-id task) (task-name task))
	  (if (task-completed? task)
	      (let ((task-type (task-task-type task)))
		(trace-msg 2 "Task is completed - we are popping the stack")
		(return-from-top-task nil)
		(trace-msg 2 "New top task is ~S:~S" (task-id (top-of-task-stack )) (task-name (top-of-task-stack)))
		(if (not (eq task-type 'are-we-done))
		    (what-next-in-task (top-of-task-stack))
		    (ask-user-what-to-do))
		)
	      
	      (or (identify-essential-argument task)
		  (propose-argument-value 'what-next task)
		  (initiate-subtask task)
		  ;; last resort, try built in decision code for task
		  (and (task-built-in  task)
		       (invoke-built-in (task-built-in task) 'what-next task))
		  ;; otherwise attempt to initiate a uncompleted task
		  
		  (propose-ask-if-done task)
		  )))
      ;;  no task!!  fail
	))
  )

(defun ask-user-what-to-do nil
  '(PROPOSE :content (ONT::PROPOSE-GOAL :id xx :agent ONT::USER)))

(defun task-completed? (task)
  (and (task-completion-criteria task)
       (every #'(lambda (x) (if (Consp x)
				(every #'(lambda (y)
					   (format t "~%HERE")
					   (not (im::var-p y)))
				       x)
				(not (im::var-p x))))
	      (task-completion-criteria task))))

(defun propose-argument-value (op task)
  (trace-msg 2 "Attempting to find arguments to propose a value for ...")
  (let ((possible-argument-proposals
	 (remove-if-not #'(lambda (x)
			    (and (im::var-p (argument-value x))  ;; not yet bound
				 (argument-produced-by x)
				 (eq (car (argument-produced-by x))
				     'funct)
				 ));; and we have a way to compute it
			(task-arguments task))))
    (when possible-argument-proposals
      (let* ((arg (car possible-argument-proposals))
	     (fun (cadr (argument-produced-by arg))))
	(trace-msg 2 "Trying to process argmument ~S" (argument-name arg))
	(format t "~% FUN=~S  IS a function? ~S" fun (functionp fun))
	(if (functionp fun)
	    (multiple-value-bind (result-msg stack-op newtask)
		(apply-builtin fun (list op arg task))
	      (format t "~% RESULT-MSG=~S  STACK-OP=~S  B+NEWTASK =~S" result-msg stack-op newtask)
	      (case stack-op
		(PUSH  (push-task newtask)
		       )
		(update
		 (replace-top-task newtask)
		 )
		)
	      result-msg
	      )))
      ))
  )
	
(defun apply-builtin (fun args)
  "This gets arond the problem that an APPLY can't return multiple values -- we have the applied funciton return a list,
which is then broken into the three expected values: RESULT-MSG, stack OP, and new task"
  (let ((res (apply fun args)))
    (values (car res) (cadr res) (caddr res))))

  
(defun invoke-built-in (proc op task)
  (trace-msg 2 "Invoking decision code for ~S:~S" (task-name task) (task-built-in task))
  (if (functionp proc)
      (multiple-value-bind (result-msg stack-op newtask)
	  (applybuiltin proc (list op task))
	(trace-msg 2 "Result is ~S and we would ~S the stack" result-msg stack-op)
	(when result-msg
	  (case stack-op
	    (PUSH  (push-task newtask)
		   )
	    )
	  result-msg))
      ;; error - not a function
      (trace-msg 2 "ERROR: Attempt to apply an unknown function: ~S" proc)
      ))

(defun process-new-information-in-arguments (task)
  "Any arguments with bound value and unbound kr-value is likely new, and we should compute the KR value.
We return a copy of the task with these KR values bound"
  (trace-msg 3 "Attempting to compute KR values for newly found values ...")
  (let ((context (task-description task))
	(newly-filled-arguments
	 (remove-if-not #'(lambda (x)
			    (and (not (im::var-p (argument-value x)))
				 (im::var-p (argument-kr-value x))
				 (argument-when-filled x)))
			(task-arguments task))))
    
    (if newly-filled-arguments
	(progn
	  (trace-msg 2 "Computing KR value for arguments with newly bound arguments: ~S" newly-filled-arguments)
	  (let ((bndgs (flatten-once (remove-if #'null
						(mapcar #'(lambda (x) (apply (cadr (argument-when-filled x)) (list x context))) newly-filled-arguments)))))
	    
	    (if bndgs
		(replace-top-task (instantiate-task task bndgs nil))
		task)))
	(progn
	  (trace-msg 2 "No arguments to compute KR value")
	  task)
	)))
  

;; WHAT-NEXT Strategy 1: identify a required argument

(defun identify-essential-argument (task)
  "tries to identify an argument via a question"
  (let ((missing-arguments
	 (remove-if-not #'(lambda (x)
			    (and (eq (argument-requirements x) :required)  ;; required arg
				 (im::var-p (argument-value x))            ;; not yet bound
				 (not (argument-produced-by x))))          ;; and we don't know a way to compute it
			(task-arguments task))))
    (trace-msg 2 "Attempting to identify unbound arguments ...")
    (if missing-arguments
	(progn
	  (trace-msg 2 "Arguments found are ~S" missing-arguments)
	  (ask-clarification-question 'IDENTIFY-ARGUMENT (car missing-arguments) task))
	(trace-msg 2 "No arguments need further specification")
	)))

;; WHAT-NEXT strategy 2: Initiate a subtask
(defun initiate-subtask (task)
  (trace-msg 2 "Attempting to find a subtasks to execute ...")
  (let ((pending-subtasks (remove-if-not #'(lambda (x)
					     (some #'im::var-p (subtask-vars-that-need-binding x)))
					 (task-subtasks task))))
    (trace-msg 3 "Pending subtasks are ~S" pending-subtasks)
    (if pending-subtasks
	(propose-subtask (car pending-subtasks) task)
	(trace-msg 2 "No subtasks left to accomplish"))))

(defun propose-subtask (subtask-spec task)
  (let* ((t-id (gen-symbol "T"))
	 (subtask (push-task (instantiate-subtask task subtask-spec t-id))))
    (trace-msg 3 "Proposing subtask ~S" subtask)
    (if subtask
	(let* ((wh-id (gen-symbol "R"))
	       (P-id (gen-symbol "P")))
	  (list 'PROPOSE :content (list 'ONT::PROPOSE :id T-id   
					:what wh-id 
					:as (list 'SUBGOAL :OF (task-id task)))
		:context (if (task-patterns subtask)
			     (instantiate-pattern-as-proposal
			      (car (task-patterns subtask)) wh-id)
			     ;; backoff, just use the type
			     (progn (format t "~%ERROR: No pattern for subtask ~S" (task-name subtask))
				    `((ONT::F ,wh-id :instance-of ONT::SUBTASK))
			     ))
		))
	(format t "~%ERROR: No task named ~S" subtask)
	)))



(defun instantiate-pattern-as-proposal (pattern wh-id)
  "Here we build a LF of a query for generation, using the pattern associated with the arg"
  (let* ((root-var (find-unbound-lf-var-in pattern))   ;; assume sits the first one in the list that is unbound!
	 )
    (if root-var
	(bind-the-rest (im::subst-in pattern (list (list (second root-var) wh-id))) nil)
	(bind-the-rest pattern nil)
	)))

(defun propose-ask-if-done (task)
  (trace-msg 2 "Attempting find argument to check if done")
  (let ((arewedone-args
	 (remove-if-not #'(lambda (x)
			    (and (im::var-p (argument-value x))  ;; not yet bound
				 (eq (argument-requirements x)
				     :ask-if-done)
				 ));; and we have a way to compute it
			(task-arguments task))))
    (when arewedone-args
      (push-are-we-done-task (car arewedone-args))
      (let* ((P-id (gen-symbol "P"))
	     (N1 (gen-symbol "N"))
	     (wh-id (gen-symbol "WH"))
	     )
	(list 'PROPOSE :content (list 'ONT::ASK-IF :id P-id  
				      :what wh-id 
				      :as `(query-in-context :OF ,(task-id task)))
	      :context (list `(ont::F ,wh-id :instance-of ONT::HAVE-PROPERTY
				      :formal ,n1)
			     `(ont::F ,n1 :instance-of ONT::COMPLETED :figure ,(task-id task))
			     )
	      )
	))
    ))

(defun push-are-we-done-task (arg)
  ;; Push the task
  (let ((argvar (argument-value arg)))
    (push-task (make-task :name 'are-we-done
			  :task-type 'are-we-done
			  :patterns (append (argument-patterns arg)
					    (list `(ACCEPTED :what ,argvar))
					    (list '(REJECTED)))
			  :arguments (list arg)
			  :completion-criteria '(ALWAYS-DONE)
			  :returns (list (list (argument-name arg) (argument-name arg))))
	       )))

(defun build-propose-arg-binding (arg valueLFS newid task)
  (let* ((P-id (gen-symbol "P"))
	 (N1 (gen-symbol "N"))
	 (wh-id (gen-symbol "WH"))
	 (v-id (second (car valueLFS)))  ; the variable of the first LF term
	 )
	 
    (list 'PROPOSE :content (list 'ONT::PROPOSE :id newid   
				  :what wh-id 
				  :as (list 'SUBGOAL :OF (task-id task)))
	  :context (list* `(ont::F ,wh-id :instance-of ONT::BE
				  :neutral ,n1 :neutral1 ,v-id)
			  `(ont::DEFINITE ,n1 :instance-of ,(argument-type arg))
			  valuelfs)
		     )
	  ))

(defun build-agree-on-value-task (arg value id)
  (if (symbolp value)
      (setq value (list value value)))
  (make-task :name 'identify-parameter
	     :status 'proposed
	     :id id
	     :patterns (argument-patterns arg)
	     :arguments (list arg
			      (make-argument :name 'proposed-value
					     :value (car value)
					     :kr-value (cadr value)))
	     :returns `((,(argument-name arg) proposed-value))
	     :completion-criteria `((KNOWN ,(argument-value arg)))
	     )
  )


;;   INITIATING CLARIFICATION QUESTIONS
(defun ask-clarification-question (clarification-goal arg task)
  (let* ((type (argument-type arg))
	 (value-var (argument-value arg))
	 (reln-id (gen-symbol "R"))
	 (wh-id (gen-symbol "WH"))
	 (q-id (gen-symbol "G")))
    (trace-msg 2 "   Found ~S" arg)
    (push-clarification-task clarification-goal arg)
					; and ask the question
    (list 'PROPOSE :content (list 'ONT::ASK-WH :id q-id
				  :query wh-id
				  :what reln-id 
				  :as (list 'query-in-context :goal (task-id task)))
	  :context (if (argument-patterns arg)
		       (instantiate-pattern-as-query (car (argument-patterns arg)) value-var wh-id reln-id)
		       ;; backoff, just use the type
		       (list (list 'ont::term wh-id :spec 'wh-term
				   :instance-of type)
			     ))
	  )))



(defun push-clarification-task (clarification-goal arg)
  ;; Push the task
  (push-task (make-task :name clarification-goal
			:patterns (argument-patterns arg)
			:arguments (list arg)
			:completion-criteria (list (list 'KNOWN (argument-value arg)))
			:returns (list (list (argument-name arg) (argument-name arg))))
	     ))

(defun instantiate-pattern-as-query (pattern var wh-id reln-id)
  "Here we build a LF of a query for generation, using the pattern associated with the arg"
  (let* ((var-instantiated-lfs (im::subst-in pattern (list (list var wh-id))))
	 (reln-var (find-unbound-lf-var-in var-instantiated-lfs))
	 )
    (if reln-var
	(bind-the-rest (im::subst-in var-instantiated-lfs (list (list (second reln-var) reln-id))) wh-id)
	(bind-the-rest var-instantiated-lfs wh-id)
	)))

(defun find-unbound-lf-var-in (lfs)
  "returns the first LF that has an unbound var -- note this will ecventually need to be generalized as it doesn't guarantee that this is the root term of the LFs - but generally will work as we already bound the LF that is the WH-TERM"
  (find-if #'(lambda (lf)
	       (im::var-p (second lf)))
	   lfs))

(defun bind-the-rest (lfs wh-id &optional vars-seen)
  " this creates a binding for every remaining variable"
  (when lfs
    (multiple-value-bind (newlf bndgs)
	(bind-vars-in-lf (car lfs) wh-id vars-seen)
      (cons newlf
	    (bind-the-rest (cdr lfs) wh-id bndgs)))
    ))

(defun bind-vars-in-lf (x wh-id bndgs)
  (when x
    (multiple-value-bind (rest newbndgs)
	(bind-the-rest-of-lf (cdr x) wh-id bndgs)
      (values (cons (choose-specifier x wh-id) rest)
	      newbndgs))))

(defun bind-the-rest-of-lf (x wh-id bndgs)
  (cond ((consp x)
	 (multiple-value-bind (expr newbndgs)
	     (bind-the-rest-of-lf (car x) wh-id bndgs)
	   (multiple-value-bind (rest-expr newestbndgs)
	       (bind-the-rest-of-lf (cdr x) wh-id newbndgs)
	     (values (cons expr rest-expr) newestbndgs))))
	   
	((im::var-p x)
	 (if (im::var-values x)
	     ;;  if it has a type restriction, then we use the first type as the value
	     (if (consp (im::var-values x))
		 (car (im::var-values x))
		 (im::var-values x))
	     (if (assoc x bndgs)
		 (cadr (assoc x bndgs))
		 (let ((New (gen-symbol "X")))
		   (values New (cons (list x New) bndgs))))))
	  (t (values x bndgs))))

(defun choose-specifier (lf wh-id)
  (cond ((not (im::var-p (car lf)))
	 (car lf))
	((eq (second lf) wh-id)
	 'ont::wh-term)
	(t
	 (let ((type (find-arg lf :instance-of)))
	   (cond ((or (om::subtype type 'ont::situation-ROOT)
		      (om::subtype type 'ont::relation)
		      )
		  'ont::f)
	      (t 'ont::definite))))
	))
  
;;   Map display manipulations

(defun display-sorghum nil
  #|(im::send-msg '(request :receiver imagedisplay :content
		  (display
		   :file (file :name "/Users/james/Desktop/Work/Code/cwms/src/Data/unity_spam_harvest_sorghum.tif"
			  :format (raster "GTIFF" 27 40))
		   :scale "1000%")))|#

  (im::send-msg '(request :receiver imagedisplay :content
		  (display
		   :file (file :name "/Users/james/Desktop/Work/Code/cwms/src/Data/ssd_spam_harvest_sorghum.tif"
			  :format (raster "GTIFF" 135 106))
		   :scale "500%"))
		)
  )
