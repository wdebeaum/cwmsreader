;;  All the data structures for the CWMS behavioral agent

(in-package :cwmsAgent)

(defvar *current-plan* nil)
(defvar *parameters* nil)
(defvar *parameter-library* nil)
(defvar *service-library* nil)
(defvar *service-input-table* (make-hash-table :test #'equal ))
(defvar *service-output-table*  (make-hash-table :test #'equal ))
(defvar *top-level-cps-goals* nil)   ;; a list of active top-level goals
(defvar *active-goal* nil)
(defvar *goal-symbol-table* nil)

(defvar *parameter-graph* nil)
(defvar *parameter-graph-included-modules* '(CROPMODELLER ABN TWIST FOODSEC WELFARE))
(defvar *parameter-graph-excluded-params-tmp* '(:SC_YEAR :SE_YEAR :NUM_MALNOUR_CHILD :SSP :WATER_ACCESS_DEG))
(defvar *parameter-graph-excluded-params* (append '(:year :time :location :geo_description) *parameter-graph-excluded-params-tmp*) )

(defstruct planner-state
  type  ;; achieve-domain-goal, clarify-goal, alt-goal, ...
  status  ;; active, deferred, completed, abandonned
  content  ;; the goal, the issue to be clarified, ...
  context  ;; as usual
  remaining-issues ;; what we may need to deal with next once this state is completed
  solution  ;; the plan to achieve the goal
  starting-assumptions  ;; what needs to be true for the solution to work
  plan
  parent
  )

(defstruct goal
  description ;; goal description
  type
  id
  parameters
  status ;; active, deferred, abandonned, completed, created (but not active)
  context
  plan
  parent)


(defstruct partial-plan
  name              ;; unique id for this plan that persists over time
  version           ;; version number for the particular instance of the plan
  goal-state
  initial-state
  intermediate-states
  unknown-inputs    ;; list of variables
  known-inputs       ;; list of variables that have values specified externally
  outputs            ;; list of variables produced
  causal-gaps        ;; list of causal-links that are currently unlabelled (no service)
  graph              ;; list of all causal-links in the plan
  symbol-table       ;; actual definitions of all parameter and link structures
  )

(defun get-initial-state (plan)
  (get-from-symbol-table (partial-plan-initial-state plan)
			 plan))

(defun get-goal-state (plan)
  (get-from-symbol-table (partial-plan-goal-state plan)
			 plan))

(defun make-new-version-of-plan (p)
  (let ((new (copy-partial-plan p)))
    (setf (partial-plan-version new) (+ (partial-plan-version new) 1))
    new))
    
(defun add-to-plan-symbol-table (object-list plan)
  "maintains the symbol table as a ASSOC list -- no need to remove earlier values of symbols"
  (let* ((new-st-entries 
	  (if (consp object-list)
	      (mapcar #'(lambda (x)
			   (gen-new-ST-entry x))
		       object-list)
	      ;; for robustness we allow a single value to be added rather than a list
	      (list (gen-new-ST-entry object-list)))))
    (setf (partial-plan-symbol-table plan)
	  (append new-st-entries (partial-plan-symbol-table plan)))))

(defun gen-new-ST-entry (object)
  (cond ((causal-link-p object)
	 (list (causal-link-id object) object))
	((parameter-in-plan-p object)
	 (list (parameter-in-plan-id object) object))
	((state-p object)
	 (list (state-id object) object))
	(t 
	 (Format t "~%Warning: trying to add bad object to symbol table: ~S" object))
	)
  )

(defun get-from-symbol-table (name ST)
  "ST may be a partial-plan or a list"
  (if (partial-plan-p ST)
      (cadr (assoc name (partial-plan-symbol-table ST)))
      (cadr (assoc name ST))))

(defun get-graph-from-plan (plan)
  (mapcar #'(lambda (x)
	      (get-from-symbol-table x (partial-plan-symbol-table plan)))
	  (partial-plan-graph plan)))

(defstruct parameter-in-plan
  id           ;; unique name for this parameter instance in the plan
  id-code      ;; name in parameter ontology
  origin       ;; pointer back to service parameter that was used to create it
  arguments    ;; pointing to 
  links-in     ;; links producing this variable
  links-out    ;; links that this variable feeds
  value        ;; the value specified for the parameter
  name         ;; record the name for named entities
  ont-type    ;; the ont type if known
  )

(defstruct parameter
  name         ;; unique ID for this parameter in its operator
  gloss         ;; description if desired
  id-code      ;; id in parameter ontology
  id-code-constraint ;; values id-code can take
  unit         ;; the units that the parameter is measured in
  format       ;; specification of data format 
;  source       ;; the database from which the variable value(s) can be taken (or marked as a control input)
  arguments     ;;  ordered list of other parameters that it depends on
  requirements  ;; :optional or :required
  type          ;; INPUT or OUTPUT
  constant-value   ;; the parameter is introduced as a constant in the definition
  )

(defstruct causal-link
  id                ;; unique ID
  from-map          ;; mapping of service input parameters to the initial state 
  to-map            ;;mapping of service output parameters to the goal state  
  initial-state   ;; initial state
  goal       ;; the goal state
  service   ;; pointer to the reasoning engine that performs the tranformation (same format as from)
  decomposition    ;; a link of causal links connecting FROM to TO
  )

(defstruct state
  id
  unknowns
  knowns
  achieved
  status  ;; :initial, :goal, :active, :solved
  )

(defun build-state-from-parms (status parms)
  "this sorts out the knowns and unknowns and returns a new state"
  (multiple-value-bind (knowns unknowns)
      (split-list #'parameter-in-plan-value (remove-if #'null parms))
    (trace-msg 3 "Defining new state with knowns ~S and unknowns ~S" knowns unknowns)
    (make-state :id (gen-symbol "S")
		:status status
		:knowns (mapcar #'parameter-in-plan-id knowns)
		:unknowns (mapcar #'parameter-in-plan-id unknowns)))
  )

(defstruct service
  name             ;; unique ID for the service
  input            ;; a list (conjunction) of names of parameters required for causal link
  output            ;; a list of prameters whose values are produced
  component         ;; name of TRIPS module that performs the tranformation
  )

(defstruct candidate-qse
  service
  output  ;;  the output parameter produced by the service
  essential-info-needed  ;; arguments of the output MUST be defined
  unknown-info-needed  ;; the other knowledge that we'd need to invoke this service)
  input-map   ;; map of known input args to knowns
  score  ;; the number of flaws (so lower the better)
  )
