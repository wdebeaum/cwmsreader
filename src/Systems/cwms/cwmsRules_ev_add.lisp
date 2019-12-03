(in-package "IM")

(reset-im-rules 'cwms_ev_add)

(mapcar #'(lambda (x) (add-im-rule x 'cwms_ev_add))  
	'(

	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; LOC rules
	  ;;;;;;;;;;;;;;;;;;;

	  #|
	  ; :FIGURE of ?!loc points to ?ev
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
	    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal :FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc1>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc1
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )
	  |#

	  ; no FIGURE; :LOCATION points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :LOCATION ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED)) ; should have been symbol-mapped already from previous levels.  If done here, ONT::F types would be changed from (:* ont::type w::word) to just ont::type even if it doesn't match anything in symbolmapping
           -loc2>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc2
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCATION -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )

	  ; I ate the pizza in the kitchen ("in the kitchen" is attached to "pizza")
	  ; no FIGURE; :LOCATION of :AFFECTED points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
		    ;:AGENT ?!ag
		    :AFFECTED ?!obj ;:DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :passive -
		    )
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART) :LOCATION ?!loc)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal
		   :FIGURE ?!obj)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc2-AFFECTED>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc2-AFFECTED
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )

	  
	  ; no FIGURE; :SOURCE points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :SOURCE ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc3>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc3
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :SOURCE -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )

	  #| this is now split into two rules so we can zero out the :RESULT and/or :RESULT1
	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc :RESULT1 ?!loc1)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?!loc1
		   (? tmp1 ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal1)
		   ;:FIGURE ?ev)
           (?relnLoc1 ?!locVal1 (? locType1 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :LOCMOD1 ?tmp1
	    :LOC1 ?!locVal1
	    :RESULT -
	    :RESULT1 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )
	  |#

	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4a>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4a
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD ?tmp
            :LOC ?!locVal
	    :RESULT -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )

	  ; no FIGURE; :RESULT points to ?!loc
          ;; modified from rule40_2_AGENT_AFFECTED-CELL_LOC1-gd-others rule with AGENT and AFFECTED
          ((?!reln0 ?ev ?type
	    ;:AGENT ?!ag :AFFECTED ?!obj :DRUM ?code
		    :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -
		    :RESULT1 ?!loc)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?!loc
		   (? tmp ONT::POSITION-RELN ONT::PATH)
		   ;(? tmp ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION)
		   :GROUND ?!locVal)
		   ;:FIGURE ?ev)
           (?relnLoc ?!locVal (? locType ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -loc4b>
           40
           (?!reln0 ?ev ?type ;?!eventName
            :rule -loc4b
            ;:AGENT ?!ag
            ;:AFFECTED ?!obj
            :LOCMOD1 ?tmp
            :LOC1 ?!locVal
	    :RESULT1 -
            ;:MODALITY ?modVal   ; these are automatically passed on in cwms (but not in DRUM!)
            ;:FORCE ?fVal
            ;:DEGREE ?type_degree
            ;:FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
            )
           )
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;; (un)able to => can(not)
	  ;;; need rules for other roles (NEUTRAL, etc)
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ; I am able to eat (AGENT)
          ((?!reln0 ?ev ONT::ABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive -)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able
	    :AGENT ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::POSSIBLE
            )
           )

	  ; I am unable to eat the pizza (AGENT)
          ((?!reln0 ?ev ONT::UNABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive -)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able2>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able2
	    :AGENT ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::IMPOSSIBLE
	    :NEGATION +
            )
           )

	  ; The pizza is able to be eaten (AFFECTED)
          ((?!reln0 ?ev ONT::ABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive +)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able3>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able3
	    :AFFECTED ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::POSSIBLE
            )
           )

	  ; The pizza is unable to be eaten (AFFECTED)
          ((?!reln0 ?ev ONT::UNABLE :FIGURE ?!fig :GROUND ?!gd)
           ;(?reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           ;(?reln2 ?!obj (? t2 ONT::PHYS-OBJECT ONT::ABSTRACT-OBJECT ONT::PART))
           (?!reln1 ?!fig ?!fig-type)
           (?!reln2 ?!gd ?!gd-type :passive +)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -able4>
           40
           (?!reln2 ?!gd ?!gd-type
            :rule -able4
	    :AFFECTED ?!fig
	    :MODALITY (:* ONT::ABILITY W::CAN)
	    :FORCE ONT::IMPOSSIBLE
	    :NEGATION +
            )
           )

	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; provenance      ; move to rules_mod
	  ;;;;;;;;;;;;;;;;;;;

	  ; He said he ate the pizza.
	  ((?!reln0 ?ev ONT::COMMUNICATION :AGENT ?!ag :FORMAL ?!f)
           (?!reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (?!reln2 ?!f ?t2)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -attributedTo>
           40
           (?!reln2 ?!f ?t2
            :rule -attributedTo
	    :ATTRIBUTED-TO ?!ag
            )
           )

	  ; "I ate the pizza," he said.             ; need the speechact too for questions etc: maybe pass on the speechact?
          ((?!reln0 ?ev ONT::COMMUNICATION :AGENT ?!ag :FORMAL ?!f)
           (?!reln1 ?!ag  (? t1 ONT::REFERENTIAL-SEM))
           (ONT::SPEECHACT ?!f ?t2 :CONTENT ?!c)
	   (?!reln3 ?!c ?t3)
           ;(ONT::EVAL (symbolmap ?type ?!eventName -rule40_2_AGENT_AFFECTED))
           -attributedTo2>
           40
           (?!reln3 ?!c ?t3
            :rule -attributedTo2
	    :ATTRIBUTED-TO ?!ag
            )
           )
	  
	  
	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; TIME rules
	  ;;;;;;;;;;;;;;;;;;;

	  ; I ate Saturday.
	  ; I ate in January.
	  ; I ate beginning Saturday.
	  ; I ate yesterday.
	  ; I ate in recent weeks.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
           -time>
           40 ;30
           (?!reln0 ?ev ?type 
            :rule -time
	    :TIMEMOD ?time-type
	    :TIME ?!gd
            )
           )
	  
	  #|
	  ; I ate Saturday.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
	   (?!reln2 ?!gd (? gd-type ONT::TIME-LOC) :DAY-OF-WEEK ?!day)
           -time1>
           40
           (?!reln0 ?ev ?type 
            :rule -time1
	    :TIMEMOD ?time-type
	    :TIME ?!day
            )
           )

	  ; I ate in January.
          ((?!reln0 ?ev ?type :TIME ?!time)
	   (?!reln1 ?!time (? time-type ONT::TEMPORAL-PREDICATE) :GROUND ?!gd)
	   (?!reln2 ?!gd (? gd-type ONT::TIME-LOC) :MONTH ?!month)
           -time2>
           40
           (?!reln0 ?ev ?type 
            :rule -time2
	    :TIMEMOD ?time-type
	    :TIME ?!month
            )
           )
	  |#



	  
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
