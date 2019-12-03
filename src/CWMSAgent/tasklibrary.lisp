;;  CWMS Task Library


(in-package :cwmsagent)

;;  With inherited roles, make sure there aren't unintentional variable name overlaps with
;;   variables in the subtype
(define-task situated-task
    :description "situated tasks all require a time and location"
    :task-type  abstract
    :arguments
    ((argument :name LOCATION
	       :type ONT::GEOGRAPHIC-REGION
	       :value ?loc
	       :kr-value ?kr-value-loc
	       :requirements :required
	       :patterns (;; e.g.  Sudan
			  ((?inheritvar3 ?loc :instance-of ONT::GEOGRAPHIC-REGION
					 :cwms ?kr-value-loc))
			  ;; e.g., In Sudan
			  ((ONT::RELN ?inheritvar2 :instance-of ONT::IN-LOC :ground ?loc)
			   (?inheritvar3 ?loc :instance-of ONT::GEOGRAPHIC-REGION
					 :cwms ?kr-value))
			  )
	       )
     (argument :name TIME
	       :type ONT::TIME-LOC
	       :value ?time
	       :kr-value ?kr-value
	       :requirements :required
	       :patterns (;; e.g., June 1998
			  ((?inheritvar3 ?time :instance-of ONT::TIME-LOC
					))
			  ;; at/in/before 1998
			  ((?inheritvar4 ?inheritvar5 :instance-of (? ?inheritvar6 ONT::TEMPORAL-LOCATION) :ground ?time)
			   (?inheritvar7 ?time :instance-of ONT::TIME-LOC :year ?kr-value-time))
			  )
	       :when-filled (funct #'(lambda (x context) (canonicalize-temporal-expression x context)))
	       )
     (argument :name EXTENT
	       :type ONT::EXTENT
	       :value ?extent
	       :requirements :optional
	       )
     ))


(define-task increase-production
    :description "explore ways to increase some indicator, e.g., increase crop yield"
    :task-type top
    :inherits-from situated-task
    :patterns (
	       ;; e.g., increase crop yield
	       ((?spec1 ?i11 :instance-of ONT::increase :affected ?!param-id)
		(?spec2 ?!ev :instance-of (? situation-aspect ONT::cause-produce-reproduce)
			))
	       ((?spec1 ?i11 :instance-of ONT::increase :affected ?!param-id)
		(?spec2 ?!ev :instance-of ONT::SET :element-type (? situation-aspect ONT::cause-produce-reproduce)
			))
	       )
    :arguments ((argument :name commodity
			  :type ONT::FOOD
			  :value ?commodity
			  :kr-value ?param-code
			  :requirements :required
			  :patterns (;;  teff yield, production of Teff
				     ((?spec3 ?!ev :instance-of ?anything :assoc-with ?commodity)
				      (?spc2 ?commodity :instance-of (? var ONT::FOOD ONT::PLANT ONT::COMMODITY) )); :cwms ?param-code))
				    ((?spec4 ?x3 :instance-of ONT::cause-produce-reproduce :affected-result ?indicator)
				     (?spc2 ?commodity :instance-of (? var ONT::FOOD ONT::PLANT ONT::COMMODITY) :cwms ?param-code))
				     
				    )
			  )
		(argument :name INDICATOR
			  :type ONT::MEASURE-METRIC
			  :value ?indicator
			  :kr-value ?param-id
			  :requirements :required
			  #|:patterns (;; using a malnutrition measure
				     ((ONT::F ?x3 :instance-of ONT::USE :affected ?indicator)
				      (?spc2 ?indicator :instance-of (? var ONT::PARAMETER ONT::MEASURE-METRIC)))
				     )|#
			  :produced-by (funct #'(lambda (x y z)
					 (propose-qre-and-indicator x y z)))
			  )

		(argument :name QRE
			  :value ?qre-description
			  :type ONT::ALGORITHM
			  :kr-value ?qre
			  :requirements :optional
			  :patterns (;; e.g., using DSSAT
				     ((?spec2 ?u :instance-of ONT::USE :affected ?engine)
				      (?spec4 ?qre :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM)))
				     ;; e.g., with DSSAT
				     ((?spec2 ?u :instance-of ONT::WITH-INSTRUMENT :ground ?engine)
				      (?spec4 ?qre :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM))))
			   :produced-by (funct #'(lambda (x y z)
						   (propose-qre-and-indicator x y z)))
			   )
		
		 (argument :name QRE-ARGUMENTS
			    :value ?qre-spec
			    :requirements :required
			    :produced-by (subtask IDENTIFY-QRE-ARGUMENTS)
			    )
		
		 (argument :name BASELINE-ANALYSIS
			    :value ?baseline-analysis
			    :requirements :optional
			    :produced-by (subtask RUN-BASELINE)
			    )
		)
    
    :constraints ((INDICATOR-FOR-SITATION ?indicator commodity)
		  (QRE-PRODUCES ?QRE ?indicator)
		  (RESULTS-FROM-EXECUTION ?plan ?baseline-analysis))
		 
    :subtasks ((subtask :name IDENTIFY-QRE-ARGUMENTS
			:input (QRE INDICATOR TIME LOCATION)
			:output (QRE-ARGUMENTS GRIDDED-LOC CRID PLANTINGYR))
	       
	       (subtask :name RUN-BASELINE
			:input ((PLAN TIME LOCATION))
			:output (BASELINE-ANALYSIS))
	       )
    :completion-criteria ((KNOWN ?baseline-analysis))
    :built-in #'(lambda (x y) (do-analyze-situation x y))
    )

(define-task IDENTIFY-QRE-ARGUMENTS
    :description "identify the argument required for this specific QRE"
    :task-type dependent
    :inherits-from situated-task
    :patterns (
	       ;; e.g., identify the parameters
	       ((?spec1 ?x :instance-of ONT::classify :neutral ?!param-id)
		(?spec2 ?!param-id :instance-of (? situation-aspect ONT::parameter)
			))
	       )
    :arguments ((argument :name CRID
			  :patterns (
				     ((?spec9 ?crid-var :instance-of (? xx ONT::PRODUCE ONT::GRAINS) :cwms ?CRID))
				     )
			  :type ont::crop
			  :value ?crid-var
			  :kr-value ?crid
			  :requirements :required
			  )
		(argument :name gridded-location
			  :type ONT::LOCATION
			  :value ?gridded-loc
			  :kr-value ?gridded-loc
			  :requirements :required
			  :produced-by (subtask identify-exact-location)
			  )
		(argment :name QRE)
		
		;;  need to identify the CRID
		;;  need to identify planting year
		)
    :subtasks (
	       (subtask :name IDENTIFY-EXACT-LOCATION
			:input (TIME LOCATION)
			:output (GRIDDED-LOCATION))
	       )
    :completion-criteria ((KNOWN ?gridded-loc ?crid ?plantingyr))
    )

(define-task identify-exact-location
    :description "uses map display to identify exact location"
    :task-type dependent
    :inherits-from situated-task
    :arguments ((argument :name map-display
			  :value ?displayed
			  :kr-value ?displayed
			  :requirements :required
			  :produced-by (funct #'(lambda (x y z) (identify-gridded-locs x y z)))
			  )
		(argument :name scale
			  :patterns (
				     ((?spec7 ?id7 :instance-of ONT::MOVE)
				      (?spec8 ?id8 :instance-of (? direction ONT::DIRECTION-IN ONT::DIRECTION-OUT))
				      )
				     )
			  :value ?value
			  :kr-value "500%"  ;; the default or current scale 
			  :requirements :optional
			  :when-filled (funct #'(lambda (x y z) (change-scale x y z)))
			  )
		;; this task continues until we get explicit user confirmation
		(argument :name userconfirmation
			  :patterns (
				     ;; We're done
				     ((?spec2 ?confirmed :instance-of ONT::FINISHED)))
			  
			  :value ?confirmed
			  :requirements :ask-if-done
			  :produced-by (USER)
			  )
		)
    
    :completion-criteria (KNOWN ?confirmed))
		

(define-task plan-for-factor
    :inherits-from situated-task
    :task-type dependent
    :description "create a plan to estimate a FACTOR over a given TIME and PLACE"
    :patterns (
	       ;; e.g., create a plan
	       ((?spec1 ?x :instance-of ONT::create :affected-result ?!p :reason ?purpose)
		(?spec2 ?!p :instance-of (? s ONT::plan)))
	       )
    :arguments ((argument :name FACTOR 
			  :type ONT::DOMAIN
			  :value ?factor
			  :requirements :required
			  :patterns ( ;; to assess malnourishment
				     ((?spec2 ?purpose :instance-of ONT::PURPOSE :ground ?g)
				      (?spec3 ?g :instance-of ONT::SCRUTINY :neutral ?factor)
				      (?spec4 ?factor :instance-of ONT::DOMAIN))
				      ;; e.g., for malnourishment
				      ((?spec2 ?purpose :instance-of ONT::PURPOSE :ground ?factor)
				       (?spec4 ?factor :instance-of ONT::DOMAIN))
				     )
			  )
			    
				       
		 (argument :name REQUIRED-ENGINE
			    :value ?required-engine
			    :requirements :optional
			    :patterns (;; e.g., using DSSAT
				       ((?spec2 ?u :instance-of ONT::USE :affected ?engine)
					(?spec4 ?required-engine :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM)))
				       ;; e.g., with DSSAT
				       ((?spec2 ?u :instance-of ONT::WITH-INSTRUMENT :ground ?engine)
					(?spec4 ?required-engine :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM))))
			    )
				       
		 (argument :name TENTATIVE-PLAN
			    :type ONT::MENTAL-PLAN
			    :value ?initial-plan
			    :requirements :computed
			    :produced-by (funct #'(lambda (x y z)
						    (SYSTEM-PLAN x y z)))
			    )
		
		 (argument :name AGREED-PLAN
			    :type ONT::MENTAL-PLAN
			    :value ?finalplan
			    :requirements :required
			    :produced-by (subtask AGREE-ON-PLAN))
		 
		 )

    :subtasks (
	       (subtask :name SYSTEM-PLAN
			:input (FACTOR REQUIRED-ENGINE)
			:output ((PLAN TENTATIVE-PLAN) NEEDTOKNOW))
	       (subtask :name AGREE-ON-PLAN
			:input (FACTOR (PLAN TENTATIVE-PLAN))
			:output ((PLAN AGREED-PLAN)))
	       )
    :completion-criteria ((KNOWN ?finalplan))
    )



;;;==================================
;;  These are tasks used for the basic system test scripts

;;  ANALYZE SITUATION -- the start of the old demo

  (define-task analyze-situation
    :description "Analyze a situation"
    :task-type top
    :inherits-from situated-task
    :patterns (
	       ;; analyze food insecurity in Sudan
		   ((?spec1 ?x :instance-of ONT::scrutiny :neutral ?!param-id)
		    (?spec2 ?!param-id :instance-of (? situation-aspect ONT::domain)
			    :param-code ?param-code))
		   )
    :arguments ((argument :name SITUATION-ASPECT
			  :type ONT::DOMAIN
			  :value ?situation-aspect
			  :kr-value ?param-code
			  :requirements :required
			  )
		(argument :name INDICATOR
			  :type ONT::MEASURE-METRIC
			  :value ?indicator
			  :requirements :required
			  :patterns (;; using a malnutrition measure
				     ((ONT::F ?x3 :instance-of ONT::USE :affected ?indicator)
				      (?spc2 ?indicator :instance-of (? var ONT::PARAMETER ONT::MEASURE-METRIC)))
				     )
			  :produced-by (funct #'(lambda (x y z)
					 (propose-indicator x y z)))
			  )
		 (argument :name PLAN
			    :value ?plan
			    :requirements :required
			    :produced-by (subtask PLAN-FOR-FACTOR)
			    )
		 (argument :name BASELINE-ANALYSIS
			    :value ?baseline-analysis
			    :requirements :optional
			    :produced-by (subtask RUN-BASELINE)
		 ))
    
    :constraints ((INDICATOR-FOR-SITATION ?indicator ?situation-aspect)
		  (PLAN-RESULTS-IN-VALUE ?plan ?factor)
		  (RESULTS-FROM-EXECUTION ?plan ?baseline-analysis))
		 
    :subtasks ((subtask :name PLAN-FOR-FACTOR
			:input ((FACTOR INDICATOR) TIME LOCATION)
			:output (PLAN))
	       
	       (subtask :name RUN-BASELINE
			:input ((PLAN TIME LOCATION))
			:output (BASELINE-ANALYSIS))
	       )
    :completion-criteria ((KNOWN ?baseline-analysis))
    :built-in #'(lambda (x y) (do-analyze-situation x y))
    )

(define-task plan-for-factor
    :inherits-from situated-task
    :task-type dependent
    :description "create a plan to estimate a FACTOR over a given TIME and PLACE"
    :patterns (
	       ;; e.g., create a plan
	       ((?spec1 ?x :instance-of ONT::create :affected-result ?!p :reason ?purpose)
		(?spec2 ?!p :instance-of (? situation ONT::plan)))
	       )
    :arguments ((argument :name FACTOR 
			  :type ONT::DOMAIN
			  :value ?factor
			  :requirements :required
			  :patterns ( ;; to assess malnourishment
				     ((?spec2 ?purpose :instance-of ONT::PURPOSE :ground ?g)
				      (?spec3 ?g :instance-of ONT::SCRUTINY :neutral ?factor)
				      (?spec4 ?factor :instance-of ONT::DOMAIN))
				     ;; e.g., for malnourishment
				      ((?spec2 ?purpose :instance-of ONT::PURPOSE :ground ?factor)
				       (?spec4 ?factor :instance-of ONT::DOMAIN)))
			  )
		
				       
		 (argument :name REQUIRED-ENGINE
			    :value ?required-engine
			    :requirements :optional
			    :patterns (;; e.g., using DSSAT
				       ((?spec2 ?u :instance-of ONT::USE :affected ?engine)
					(?spec4 ?required-engine :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM)))
				       ;; e.g., with DSSAT
				       ((?spec2 ?u :instance-of ONT::WITH-INSTRUMENT :ground ?engine)
					(?spec4 ?required-engine :instance-of (? x  ONT::ALGORITHM ONT::SYSTEM))))
			    )
				       
		 (argument :name TENTATIVE-PLAN
			    :type ONT::MENTAL-PLAN
			    :value ?initial-plan
			    :requirements :computed
			    :produced-by (funct #'(lambda (x y z)
						    (SYSTEM-PLAN x y z)))
			    )
		
		 (argument :name AGREED-PLAN
			    :type ONT::MENTAL-PLAN
			    :value ?finalplan
			    :requirements :required
			    :produced-by (subtask AGREE-ON-PLAN))
		 
		 )

    :subtasks (
	       (subtask :name SYSTEM-PLAN
			:input (FACTOR REQUIRED-ENGINE)
			:output ((PLAN TENTATIVE-PLAN) NEEDTOKNOW))
	       (subtask :name AGREE-ON-PLAN
			:input (FACTOR (PLAN TENTATIVE-PLAN))
			:output ((PLAN AGREED-PLAN)))
	       )
    :completion-criteria ((KNOWN ?finalplan))
    )

				       
		 
			    
    
    
    
		 
		 
			    
    
