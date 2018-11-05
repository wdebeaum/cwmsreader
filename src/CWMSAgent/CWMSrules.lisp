;;  CWMS Agent State Management
;;   This resuses the BA agent FST code 
;;   input from the messages is flattened to facilitate matching
;;  e.g.,  (WHAT-NEXT :active-goal XXX :context ((ONT::RELN ...) (ONT::THE ...) ...))
;;  becomes
;;         (  (WHAT-NEXT :active-goal XXX :context ((ONT::RELN xxx ...) (ONT::THE ...) ...))
;;            (ONT::RELN xxx ...)
;;            (ONT::THE ...)
;;            ...)


(in-package :cwmsagent)

(dagent::add-state 'evaluate-handling
    (dagent::state :action nil 
		   :transitions
		       (list
			(dagent::transition
			 :description "top level CONDITIONAL ASK-WH -- What happens to the price of wheat if we cut th amount of fertilzer in half"
			 :pattern '((REQUEST XX EVALUATE :ps-id ASK-WH :id ?!id :what ?!what :query ?!query :as ?!as)
				    ((? x1 ont::reln ont::event) ?!query ONT::COND :factor ?!factor :outcome ?!outcome)
				    ((? x2 ont::reln ont::event) ?!factor (? op ONT::DECREASE ONT::INCREASE ont::CARVE-CUT ont::break) :affected ?parm :extent ?extent)
				    ((? x3 ont::reln ont::event) ?!outcome ont::occurring :affected ?!outcome-parm
				     :time ?time :location ?loc)
				    -evaluate-ask-wh-happens-to-compute-delta-effect>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (modify-goal (ONT::RELN ?newid :instance-of  COMPUTE-DELTA-EFFECT :affected-parameter ?!outcome-parm
							     :delta-event ?!factor
							     :delta-parameter ?parm
							     :delta-extent ?extent
							     :time ?time
							     :loc ?loc)
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "top level CONDITIONAL ASK-WH -- What can we do to mitigate the problem"
			 :pattern '((REQUEST XX EVALUATE :ps-id ASK-WH :id ?!id :what ?!what :query ?!query :as ?!as)
				    ((? x1 ont::reln ont::event) ?!query ONT::EXECUTE :neutral ?!what :reason ?!reason)
				    ((? x2 ont::reln ont::event) ?!reason ONT::PURPOSE :ground ?!goal
				     :time ?time :location ?loc)
				    -evaluate-ask-what-to-what-about-problem>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of SOLVE-PROBLEM
							     :problem ?!goal
							     :time ?time
							     :loc ?loc)
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "top level CONDITIONAL ASK-WH -- How does this relate to planting date"
			 :pattern '((REQUEST XX EVALUATE :ps-id ASK-WH :id ?!id :what ?!what :query ?!query :as ?!as)
				    ((? x1 ont::reln ont::event) ?!query ONT::EXTENDED-SAY :formal ?!prop)
				    (?spec ?!prop ONT::TIME-POINT)
				    
				    -evaluate-ask-about-planting-date>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of relate-planting-date
							     :problem ?!goal
							     :time ?time
							     :loc ?loc)
				      ))
				    )
			 :destination 'dagent::segmentend)

			;; what if
			(dagent::transition
			 :description "what if question --> perform comparison"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    ((? x1 ont::reln ont::event) ?!query ONT::EXISTS :neutral ?!factor :time ?!time-loc)
				    (?spec ?!factor (? op ONT::NATURAL-PHENOMENON))
				    ((? x3 ont::reln ont::event) ?!time-loc ont::time-span-rel :ground ?!time)
				    -evaluate-ask-wh-happens-to-compute-delta-effect1>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of WEATHER-EFFECT
							     :effect ?!factor
							     :time ?!time
							     :loc ?loc)
						  ))
				    )
			 :destination 'dagent::segmentend)

			
			(dagent::transition
			 :description "direct command to perform the weather effect"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    ((? x1 ont::reln ont::event) ?!what ONT::EXECUTE :neutral ?!prog :time ?time-loc)
				    (?spec ?!prog (? op ONT::IMITATE-SIMULATE) :location ?loc)
				    ;;((? x3 ont::reln ont::event) ?!time-loc ont::time-span-rel :ground ?!time)
				    -command-simulate>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of WEATHER-EFFECT
							     :effect ?!factor
							     :time ?!time
							     :loc ?loc)
						  ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "direct command to perform the weather effect"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    ((? x1 ont::reln ont::event) ?!what ONT::CHANGE :affected ?!parm :time ?time)
				    (?spec ?!parm (? op ONT::TIME-POINT) :location ?loc)
				    ;;((? x3 ont::reln ont::event) ?!time-loc ont::time-span-rel :ground ?!time)
				    -change-parameter>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of CHANGE-PARM
							     :param ?!parm
							     :time ?time
							     :loc ?loc)
						  ))
				    )
			 :destination 'dagent::segmentend)

			
			(dagent::transition
			 :description "generic exploration of some property"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    ((? x1 ont::reln ont::event ont::epi) ?!query ONT::SCRUTINY :neutral ?!param-id :extent ?extent1 :time ?time1 :location ?loc1)
				    (?spec ?!param-id (? param ONT::DOMAIN) :time ?time2 :location ?loc2 :extent ?extent2)
				    -evaluate-analyze-param>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (modify-goal (ONT::RELN ?newid :instance-of ANALYZE-ASPECT
								:parameter ?!param-id
								:extent1 ?extent1
								:extent2 ?extent2
								:time1 ?time1
								:time2 ?time2
								:loc1 ?loc1
								:loc2 ?loc2)
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "goal to explain/understand some situation, event of process"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    ((? x1 ont::reln ont::event ont::epi) ?!what
				             ONT::UNDERSTAND :formal ?!param-id :extent ?extent1 :time ?time1 :location ?loc1)
				    (?spec ?!param-id ONT::SITUATION-ROOT :time ?time2 :location ?loc2 :extent ?extent2)
				    -evaluate-analyze-situation>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (new-goal (ONT::RELN ?newid :instance-of ANALYZE-SITUATION
								:event ?!param-id
								:extent1 ?extent1
								:extent2 ?extent2
								:time1 ?time1
								:time2 ?time2
								:loc1 ?loc1
								:loc2 ?loc2)
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "e.g., here's a paper"
			 :pattern '((REQUEST XX EVALUATE :ps-id ASSERTION :id ?!id :what ?!what :as ?!as)
				    (ont::reln ?!what ONT::EVENTS-IN-MODEL :events (?!prop))
				    (ont::reln ?!prop
				             ONT::EXISTS :neutral ?!param-id :time ?time1 :location ?loc1)
				    (?spec ?!param-id ONT::ARTICLE :time ?time2 :location ?loc2)
				    -evaluate-analyze-situation-read-article>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (new-goal (ONT::RELN ?newid :instance-of READ-PAPER
								:event ?!param-id
								:time1 ?time1
								:time2 ?time2
								:loc1 ?loc1
								:loc2 ?loc2)
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "e.g., show me information related to Ebola"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    (ont::reln ?!what ONT::SHOW :neutral ?!prop)
				    (?spec2 ?!prop ONT::INFORMATION :mod ?!param-id)
				    (ont::EPI ?!param-id ONT::ASSOCIATE :neutral2 ?!topic)
				    ((? spec3 ONT::A ONT::INDEF-SET ONT::THE) ?!topic ?topic-type)
				    -show-information-related-to-concept
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (new-goal (ONT::RELN ?newid :instance-of GET-RELATED-INFO
							  :concept ?topic-type
							  )
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "e.g.,Add the fact that the attacks caused the lockdown"
			 :pattern '((REQUEST XX EVALUATE :ps-id ADOPT :id ?!id :what ?!what :as ?!as)
				    (?spec2 ?!what ONT::ADD-INCLUDE :affected ?!prop)
				    (ont::the ?!prop ONT::FACT :formal ?!param-id)
				    (?spec  ?!param-id ONT::CAUSE :factor ?!agent :outcome ?!effect)
				    ((? spec1 ont::THE ont::the-set ont::event) ?!agent ?agent-type)
				    ((? spec3 ont::THE ont::the-set ont::event) ?!effect ?effect-type)
				    -add-causal-link
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (new-goal (ONT::RELN ?newid :instance-of ADD-LINK
							  :cause ?agent-type
							  :effect ?effect-type
							  )
				      ))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "answer to WH question to identify a parameter"
			 :pattern '((REQUEST XX EVALUATE :ps-id ANSWER :to ?!to :what ?!what :query ?!query :value ?!value :content ?!content :context ?!context)
				    (ont::eval (validate-answer :act CLARIFY-PARAMETER :to ?!to :what ?!what :query ?!query :value ?!value :content ?!content :context ?!context :result ?!res :ptype ?!ptype))
				    -got-answer-for-parameter-value>
				    ;(dagent::set-result (set-parameter :id ?!to :value ?!value))
				    (dagent::set-result (clarify-parameter :id ?!to :value ?!value :ptype ?!ptype :result ?!res))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "answer to YN confirmation request"
			 :pattern '((REQUEST XX EVALUATE :ps-id ANSWER :to ?!to :what nil :query ?!query :value (? value ONT::TRUE ONT::FALSE):content ?!content :context ?!context)
				    (ont::eval (validate-answer :act CONFIRM :to ?!to :what nil :query ?!query :value ?value :content ?!content :context ?!context :result ?!res))
				    -got-confirmation-for-parameter-value>
				    (dagent::set-result (confirm-parameter :id ?!to :value (? value ONT::TRUE ONT::FALSE) :result ?!res))
				    )
			 :destination 'dagent::segmentend)

			(dagent::transition
			 :description "default rule for questions asking about a property of some country"
			 :pattern '((REQUEST XX EVALUATE :ps-id ASK-WH :id ?!id :what ?!what :query ?!query :as ?!as)
				    ((? x1 ont::reln ont::event) ?!query ONT::BE :neutral ?!n :neutral1 ?!n1)
				    (ont::TERM ?!n ONT::REFERENTIAL-SEM :spec ont::WH-TeRM)
				    (?ss ?!n1 (? xx ont::INFLUENCE ont::MODULATE) :factor ?!factor
				     :time ?time :location ?loc)
				    -evaluate-ask-question-default>
				    (gen-symbol G ?newid)
				    (dagent::set-result
				     (add-subgoal (ONT::RELN ?newid :instance-of GENERAL-QUESTION
							     :factor ?!factor
							     :time ?time
							     :loc ?loc)
				      ))
				    )
			 :destination 'dagent::segmentend)

			
			)
		       ))

;;  And here's the code of all functions that are not defined by DAGENT functions

(dagent::define-user "CWMS" (dagent::make-user :name "CWMS" :channel-id 'desktop))

(defun process-message (msg args)
  (dagent::invoke-state 'what-next-handling nil 'desktop args 
			(process-input (find-arg-in-act msg :content))))
			
;;  DOMAIN Specific actions for execute-action

(defun dagent::check-domain-specific-actions (act)
  (case (car act)
    (do-planning
	(apply 'do-planning (cdr act)))
    (gen-symbol
     (im::match-vals nil (gen-symbol (second act)) (third act)))
    (validate-answer
     (apply #'validate-answer (cdr act)))
	  
      )
    )

(defun do-planning (&key factor outcome result)
  (format t "Got to DO-PLANNING with factor ~S and outcome ~S" factor outcome)
  (let ((plan (cwms-planner (list factor) (list outcome))))
    (if plan
	(progn
	  (format t "~%Found plan!")
	  (show-plan plan)
	  (im::match-vals nil result (execute-plan plan))
	  )
	(progn
	  (format t "~%Failed to find plan!")
	  (im::match-vals nil result '(report :content (failure :type cannot-perform :reason (failed-action :what planning))))
	  )
	))
  )
       

(defun validate-answer (&key act to what value query content context result ptype)
  "check that answer satisfies the parameter constraints"
  ;; actually for now just check that the ID is valid!
  (let* ((p (get-parameter-being-clarified to))
	 (lf (get-from-context value context))
	 (actualvalue (extract-value value context))
	 )
    (when (and p
	       (eq (car p) act)
	       actualvalue)
      (case act
	(clarify-parameter
	 ;;  now check if answer is of the right type
	 (let* ((expected-id-code (if (parameter-in-plan-p (cadr p))
				     (parameter-in-plan-id-code (cadr p))))
	       (expected-type (case expected-id-code
			  (:loc 'ONT::LOCATION)
			  (:time 'ONT::TIME-LOC)))
	       )
	   #|
	   (when (case expected-id-code
		   (:loc
		    (om::subtype (find-arg lf :instance-of) 'ONT::LOCATION))
		   (:time
		    (om::subtype (find-arg lf :instance-of) 'ONT::TIME-LOC)))
	     im::*success*))
	   |#
	   (if (om::subtype (find-arg lf :instance-of) expected-type)
	       (append (im::match-vals nil ptype expected-type) (im::match-vals nil result 'acceptable))
	     (append (im::match-vals nil ptype expected-type) (im::match-vals nil result 'unacceptable)) 
	   )
	 ))
	((confirm confirm-parameter)
	 (if (member value '(ONT::TRUE ONT::FALSE))
	     (im::match-vals nil result 'acceptable)
	   (im::match-vals nil result 'unacceptable)))
       
	(otherwise
	 ;im::*success*)))))
	 (im::match-vals nil result 'acceptable))))))
