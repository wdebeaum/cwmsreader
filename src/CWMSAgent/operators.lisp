;;  Knowledge about all the quantitative modeling engines
;; This contains some hardwired models. In the long run, most models
;;  should be added by having the modules declare their capabilities when
;;  the system starts up


(in-package :cwmsAgent)

(setq *implementation-library* nil)
(setq *input-list* nil)
(setq *output-list* nil)
;(setq *parameter-library* nil)



;;  PARAMETER LIBRARY MAINTENANCE

;(defvar *parameter-library* nil)

(defun add-to-parameter-library (prm)
  (setq *parameter-library* (cons (list (parameter-id-code prm) prm) *parameter-library*))
  prm  ;; returns the new parameter structure
  )

(defmacro define-parameter (&rest args)
  `(apply #'define-parameter+ ',args))

(defun define-parameter+ (&key name gloss id-code id-code-constraint unit format requirements type arguments constant-value)
  (add-to-parameter-library (make-parameter :name (keywordify name)
					      :gloss gloss 
					      :id-code (keywordify id-code)
					      :id-code-constraint (mapcar #'keywordify id-code-constraint)
					      :unit unit 
					      :format format
					      :arguments (mapcar #'keywordify arguments)
					      :requirements requirements
					      :type type
					      :constant-value constant-value)))

(defun get-parameter-by-name (x)
  (assoc x *parameter-library*))

(defun validate-parameters (plist)
  (if (every #'parameter-in-plan-p plist)
      plist
      (format t "~%WARNING: Undefined parameters:  ~S" (remove-if #'parameter-in-plan-p plist))))


;;  SERVICE LIBRARY OPERATIONS

(defun add-to-service-library (imp)
  (if (service-p imp)
      (progn
	(mapcar #'(lambda (x)
		    (push imp (gethash (parameter-id-code x) *service-input-table*)))
		(service-input imp))
	(mapcar #'(lambda (x)
		    (push imp (gethash (parameter-id-code x) *service-output-table*)))
		(service-output imp))
	(setq *service-library* (cons (list (service-name imp) imp) *service-library*))
	)
      (format t "~%WARNING: Bad SERVICE structure: ~S" imp)))

(defun find-service-by-name (name)
  (cadr (assoc name *service-library*)))

(defmacro define-service (&rest args)
  `(apply #'define-service+ ',args))

(defun define-service+ (&key input output name component )
  
    (let ((uniquename (gen-unique-name name))
	  (input-name-list (mapcar #'process-input-declaration input))
	  (output-name-list (mapcar #'process-output-declaration output)))

      (if (validate-component component)
	  (progn
	    (add-to-service-library (make-service :input input-name-list 
						:output output-name-list 
						:name uniquename 
						:component component))
	  (if (member component *parameter-graph-included-modules*)
	      (let ( ;(input-id-list (mapcar #'(lambda (x) (parameter-id-code x)) input-name-list))
		     ;(output-id-list (mapcar #'(lambda (x) (parameter-id-code x)) output-name-list))
		    (input-id-list (remove-duplicates (remove-if #'null (mapcar #'get-graph-nodes input-name-list)) :test #'equal))
		    (output-id-list (remove-duplicates (remove-if #'null (mapcar #'get-graph-nodes output-name-list)) :test #'equal))
		    )
		(setq *parameter-graph* (append *parameter-graph* (list (list 'influence :input input-id-list :output output-id-list :name component))))
		)
	    )
	  )
      (warn "Invalid component name in declaration: ~S" component))))

(defun get-graph-nodes (parameter)
  (let* ((id-code (parameter-id-code parameter))
	 (id-code-no-delta (if (and (consp id-code) (eq (car id-code) ':DELTA))
			       (second id-code)
			     id-code
			     ))
	 (id-code-no-time-loc (if (member id-code-no-delta *parameter-graph-excluded-params*) nil id-code-no-delta))
	 )
    id-code-no-time-loc
    )
  )

; e.g., (get-path-in-parameter-graph ':PFDAY ':PCT_MALNOUR_CHILD)
(defun get-path-in-parameter-graph (startnode endnode)
  (get-parameter-subgraph-forward (list startnode) (get-parameter-subgraph (list endnode)) )
  )

; e.g., (get-parameter-subgraph-forward '(:PFDAY) (get-parameter-subgraph '(:PCT_MALNOUR_CHILD)) )
(defun get-parameter-subgraph-forward (startnodes graph) ; startnodes is a list of nodes
  (let ((relevant-parameters (get-relevant-parameters-forward nil startnodes graph)))
    (remove-duplicates (remove-if #'null
	(mapcar #'(lambda (x)
		    (let ((inputs (intersection relevant-parameters (find-arg (cdr x) :input))) ; probably this is always all the input parameters of the edge, but just in case
			  (outputs (intersection relevant-parameters (find-arg (cdr x) :output))))
		      (if (and inputs outputs) (list 'influence :input inputs :output outputs :name (find-arg (cdr x) :name)))
		      ))
		graph)) :test #'equal)
    )
  )

(defun get-relevant-parameters-forward (visited pending graph)
  (let* ((node (car pending))
	 (inputs (remove-duplicates (flatten (mapcar #'(lambda (x) (if (member node (find-arg (cdr x) :input))
						       (find-arg (cdr x) :output)
						     ))
						     graph)) :test #'equal))
	 (current (remove-if #'(lambda (y) (member y (append visited pending))) inputs))
	 (pending-new (append (cdr pending) current))
    	)

    ;(format t "~%~% node: ~S ~% visited: ~S ~% pending: ~S ~% inputs: ~S ~% current: ~S ~% pending-new: ~S ~%" node visited pending inputs current pending-new)
    (if pending-new
	(get-relevant-parameters-forward (append visited (list node)) pending-new graph)
      (append visited (list node))
      )
    )
  )

; e.g., (get-parameter-subgraph '(:PCT_MALNOUR_CHILD))
(defun get-parameter-subgraph (startnodes) ; startnodes is a list of nodes
  (let ((relevant-parameters (get-relevant-parameters nil startnodes)))
    (remove-duplicates (remove-if #'null
	(mapcar #'(lambda (x)
		    (let ((inputs (intersection relevant-parameters (find-arg (cdr x) :input))) ; probably this is always all the input parameters of the edge, but just in case
			  (outputs (intersection relevant-parameters (find-arg (cdr x) :output))))
		      (if (and inputs outputs) (list 'influence :input inputs :output outputs :name (find-arg (cdr x) :name)))
		      ))
	    *parameter-graph*)) :test #'equal)
    )
  )

(defun get-relevant-parameters (visited pending)
  (let* ((node (car pending))
	 (inputs (remove-duplicates (flatten (mapcar #'(lambda (x) (if (member node (find-arg (cdr x) :output))
						       (find-arg (cdr x) :input)
						     ))
				     *parameter-graph*)) :test #'equal))
	 (current (remove-if #'(lambda (y) (member y (append visited pending))) inputs))
	 (pending-new (append (cdr pending) current))
    	)

    ;(format t "~%~% node: ~S ~% visited: ~S ~% pending: ~S ~% inputs: ~S ~% current: ~S ~% pending-new: ~S ~%" node visited pending inputs current pending-new)
    (if pending-new
	(get-relevant-parameters (append visited (list node)) pending-new)
      (append visited (list node))
      )
    )
  )

(defun gen-unique-name (name)
  (if (and name (null (find-service-by-name name)))
      ;;  name is unique and OK for new service
      name
      (gen-symbol "SERVICE")))

(defun validate-component (name)
  "checks if the name is associated with a TRIPS module (but not sure if we care for now)"
  T)

(defun validate-id-code (code)
  "checks if code is known in the parameter ontology"
  code)

(defun process-input-declaration (input)
  (if (and (consp input) (eq (car input) 'input))
      (let ((name (or (find-arg (cdr input) :name)
		      (gen-symbol "INPUT")))
	    (gloss (find-arg (cdr input) :gloss))
	    (id-code (validate-id-code (find-arg (cdr input) :id-code)))
	    (id-code-constraint (find-arg (cdr input) :id-code-constraint))
	    (unit (find-arg (cdr input) :unit))
	    (args (find-arg (cdr input) :arguments))
	    (format (find-arg (cdr input) :format))
	    (requirements (or (find-arg (cdr input) :requirements)
			      :required))
	    )
	(if (and id-code format)
	    (define-parameter+ :name name
			       :gloss gloss
	                       :id-code id-code 
	                       :id-code-constraint id-code-constraint
			       :unit unit
			       :arguments args
			       :format format
			       :requirements requirements
			       :type 'input
			       :constant-value (find-arg (cdr input) :constant-value))
	    (warn "Ill-specified input parameter: must have ID-CODE and FORMAT defined: ~S:" input)))
      (warn "Ill-specified input parameter: does not start with INPUT: ~S" input)))

(defun process-output-declaration (output)
  (if (and (consp output) (eq (car output) 'output))
      (let ((name (or (find-arg (cdr output) :name)
		      (gen-symbol "OUTPUT")))
	    (id-code (validate-id-code (find-arg (cdr output) :id-code)))
	    (unit (find-arg (cdr output) :unit))
	    (format (find-arg (cdr output) :format))
	    (args (find-arg (cdr output) :arguments))
	    (gloss (find-arg (cdr output) :gloss))
	    )
	(if (and id-code format)
	    (define-parameter+ :name name
			       :gloss gloss
			       :arguments args
	                       :id-code id-code 
			       :unit unit
			       :format format
			       :type 'output
			       )
	    (warn "Ill-specified output parameter: must have ID-CODE and FORMAT defined: ~S:" output)))
      (warn "Ill-specified output parameter: does not start with OUTPUT: ~S" output)))
	      
	    
  
;;  CORE PLANNING OPERATIONS

(defun find-possible-services-for-gap (gp plan)
  (let ((gap (if (causal-link-p gp) 
		 gp
		 (cadr (assoc gp (partial-plan-symboltables plan))))))
  (find-possible-services 
   (state-knowns (causal-link-initial-state gap)) (state-unknowns (causal-link-goal causal-link-to gap)))))
   
(defun find-possible-services (inputs outputs)
  (let ((possible-services-from-inputs (find-services-from-inputs inputs))
	(possible-services-from-outputs (find-services-from-outputs outputs)))
    (intersection possible-services-from-inputs possible-services-from-outputs))
  )

(defun find-services-from-inputs (inputs)
  (find-intersecting-lists (mapcar #'(lambda (x)
				       (get-hash x *service-input-table*))
				   inputs)))

(defun  find-services-from-outputs (outputs)
  (find-intersecting-lists (mapcar #'(lambda (x) 
				       (gethash x *service-output-table*))
				   outputs)))

(defun find-intersecting-lists (possible-services)
  "find intersection of a list of lists (one list for each parameter)"
  (if (cdr possible-services)
      (intersection (car possible-services)
		    (find-intersecting-lists (cdr possible-services)))
      (car possible-services)))

;;  SOME BUILT-IN SERVICES

(define-service+ :name 'weather-approx
  :component 'builtin
  :output
  '((OUTPUT :NAME D_PROD :GLOSS
     "global sum of harvest weight at maturity" :ID-CODE (:DELTA :PROD)
     :ARGUMENTS (:LOCATION-FILE :CRID :PLYR1) :UNIT ONT::MEGATONNE :FORMAT
     (TABLE :PROD :LOCATION-FILE :CRID :PLYR)))
  :input
  '((INPUT :NAME PROD :GLOSS
     "global sum of harvest weight at maturity" :ID-CODE PROD
     :ARGUMENTS (:LOCATION-FILE :CRID :PLYR1) :UNIT ONT::MEGATONNE :FORMAT
     (TABLE :PROD :LOCATION-FILE :CRID :PLYR))
    (INPUT :NAME CRID :GLOSS "identifier of crop to simulate"
     :ID-CODE CRID :ID-CODE-CONSTRAINT
     (MAZ PML RIC SGG SBN WHB WHD WHT) :FORMAT ONT::CODE
     :REQUIREMENTS :REQUIRED)
    (INPUT :NAME PLYR :GLOSS "planting year(s) to simulate"
     :ID-CODE YEAR :FORMAT (OR ONT::NUMBER ONT::LIST)
     :ID-CODE-CONSTRAINT (2018 2019 2020)
     :REQUIREMENTS :REQUIRED)
    (INPUT :NAME PLYR1 :GLOSS "planting year(s) to simulate"
     :ID-CODE YEAR :FORMAT (OR ONT::NUMBER ONT::LIST)
     :constant-value (LIST 1980 2010)
     :REQUIREMENTS :REQUIRED)
    (INPUT :NAME LOCATION-FILE :GLOSS
     "image file of geographic region to simulate" :ID-CODE
     LOCATION :FORMAT ONT::COUNTRY-CODE :REQUIREMENTS
     :REQUIRED)))



;;  SOME DEFALT STUFF FOR DEBUGGING


#||(define-parameter :name FERTLEVEL :id-code FERTLEVEL
		  :gloss "fertilizer level" :unit ont::level)
(define-parameter :name WHEAT-YIELD :id-code wheat-yield
		  :gloss "Wheat yield" :unit ont::kilogram)
(define-parameter :name WHEAT-PRICE :id-code wheat-price
		  :gloss "Wheat price" :unit ont::dollar)

(define-service
    :name compute-yield
    :input (FERTLEVEL CULTIVAR WEATHER LOCATION)
    :output (WHEAT-YIELD)
    :component PSIMS)

(define-service
    :name compute-prices
    :input (WHEAT-YIELD)
    :output (WHEAT-PRICE)
    :component PUMA)||#
