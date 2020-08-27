(in-package "IM")

;(setq *roles-to-suppress* '(:location))  ; temporarily here
      
(reset-im-rules 'cwmsRules)  ;; this allows you to edit this file and reload it without having to reload the entire system

(mapcar #'(lambda (x) (add-im-rule x 'cwmsRules))  ;; sets of rules are tagged so they can be managed independently 
	'(

;;;;;;;;;;;;;;;

#|	  
          ((ONT::F ?ev ?!t)   
           -rule-generic>
           60
           (ONT::EVENT ?ev ?!t
            :rule -rule-generic
            )
           )

          (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev ?!t)   
           -rule-generic2>
           60
           (ONT::TERM ?ev ?!t
            :rule -rule-generic2
            )
           )
|#
	  
;;;;;;;;;;;;;;;

	  ;; robust rule for pronouns, e.g., it, itself, we
	  ; note: the type might be replaced by coref type
	  ; impro is for "you" but there are others
	  (((? reln ONT::PRO ONT::PRO-SET ONT::IMPRO ONT::WH-TERM) ?!obj
	    ;(:* ?!type ?!w) :PROFORM ?!pro
	    ?!type :PROFORM ?!pro ; took out the ?!w because coref substitution would remove the ?!w
	    )
	   -robustPro>
	   100
	   (ONT::TERM ?!obj ?!type
	    :pro ?!pro
;	    :drum ?code
	    :rule -robustPro
	    )
	   )
	  
	  ((?reln ?!obj (:* (? t1 ONT::GRAINS ONT::CHEMICAL ONT::GEOGRAPHIC-REGION) ?!w) :SEQUENCE -)
	   -simple-ref>
	   90
	   (ONT::TERM ?!obj ?t1
	    :name ?!w
	    ;:drum ?code  ; we can call the parameter finder here
	    :rule -simple-ref
	    ))

	  ((?reln ?!obj ONT::QUANTITY :AMOUNT ?!amt :unit ?u :scale ?sc :SEQUENCE -)
	   (?reln2 ?!amt ONT::NUMBER :VALUE ?!val)
	   -simple-ref3>
	   90
	   (ONT::TERM ?!obj ONT::QUANTITY
	    ;:name ?!w
	    :amount ?!val
	    :unit ?u
	    :scale ?sc
	    ;:drum ?code  ; we can call the parameter finder here
	    :rule -simple-ref3
	    ))

	  #|
	  ; the amount/quantity/number (of Ras)
	  ((?spec ?!amt ONT::QUANTITY-ABSTR :FIGURE ?obj) ; :FIGURE is optional
	   (?spec2 ?obj ?t) 
	   -amt>
	   100
	   (ONT::TERM ?!amt ONT::QTY
	    :rule -amt
	    :entity ?obj
	    :figure -
;	    :drum ?code    
	    )
	   )
	  |#

	  #|
	  ((?reln ?!obj (:* (? t1 ONT::PRICE ONT::QUANTITY-ABSTR) ?!w) :FIGURE ?!f :location ?loc :time ?time1 :SEQUENCE -)
	   (?reln2 ?!f (:* (? t2 ONT::GRAINS ONT::CHEMICAL) ?!w2) :location ?loc2 :time ?time2 :SEQUENCE -)

           (ONT::F ?loc (? tmp ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION) :GROUND ?locVal)
           (?relnLoc ?locVal (? locType ONT::GEOGRAPHIC-REGION))

           ;(ONT::F ?loc2 (? tmp2 ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION) :GROUND ?locVal)
           (ONT::F ?loc2 (? tmp2 ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION) :GROUND ?locVal2)
           (?relnLoc2 ?locVal2 (? locType2 ONT::GEOGRAPHIC-REGION))

           (ONT::F ?time1 (? tmp3 ONT::TEMPORAL-LOCATION) :GROUND ?timeVal)
           (?relnTime ?timeVal (? timeType ONT::TIME-OBJECT) :year ?yr)

           ;(ONT::F ?time2 (? tmp4 ONT::TEMPORAL-LOCATION) :GROUND ?timeVal)
           (ONT::F ?time2 (? tmp4 ONT::TEMPORAL-LOCATION) :GROUND ?timeVal2)
           (?relnTime2 ?timeVal2 (? timeType2 ONT::TIME-OBJECT) :year ?yr2)

           (ONT::EVAL (symbolmap ?t1 ?!t1_new))
	   -simple-ref2>
	   90
	   (ONT::TERM ?!obj ?!t1_new
	    ;:name ?!w
	    :entity ?!f
	    :figure -
	    :locmod ?tmp
	    :loc ?locVal
	    :locmod ?tmp2
	    :loc ?locVal2
	    :timemod ?tmp3
	    :time ?timeVal ;?yr
	    :timemod ?tmp4
	    :time ?timeVal2 ;?yr2
	    ;:drum ?code
	    :rule -simple-ref2
	    :location - ; suppress this as we have the info in :loc
	    )
	   (?reln2 ?!f ?t2
	    :location - ; suppress this as we have the info in :loc
	    )

	   )
	  |#

	  #|
	  ; map develop from ONT::GROW to ONT::DEVELOP
	  ((?reln ?!obj (:* (? t1 ONT::GROW) (? w w::development w::develop)) :SEQUENCE -) ; need the other forms too, e.g., develops
	   -simple-ref0>
	   90
	   (;ONT::TERM ?!obj ONT::DEVELOP
	    ?reln ?!obj ONT::DEVELOP
	    ;:name ?w
	    ;:drum ?code  ; we can call the parameter finder here
	    :rule -simple-ref0
	    ))
	  |#
	  

	  #|
          ;; rule20_1_AGENT_AFFECTED rule with AGENT and AFFECTED
	  ;; cwms: made ?ag and ?obj optional and with no type constraint (not even REFERENTIAL-SEM) because the type could be an extracted type that does not exist in the TRIPS ontology
          ((?!reln0 ?ev
            (? type ONT::INCREASE ONT::ADD-INCLUDE ONT::GROW ONT::PROGRESS ONT::DECREASE ONT::DECREASE-COMPLETELY ONT::DISAPPEAR ) :AGENT ?ag :AFFECTED ?obj :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq 
	    :location ?loc :time ?time1 :extent ?ext
	    :OPERATOR - )
           (?reln1 ?ag  ?t1)
           (?reln2 ?obj ?t2 :location ?loc2 :time ?time2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::F ?loc (? tmp ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION) :GROUND ?locVal)
           (?relnLoc ?locVal (? locType ONT::GEOGRAPHIC-REGION))
           (ONT::F ?time1 (? tmp3 ONT::TEMPORAL-LOCATION) :GROUND ?timeVal)
           (?relnTime ?timeVal (? timeType ONT::TIME-OBJECT) :year ?yr)
           (ONT::F ?ext (? tmp4 ONT::EXTENT-PREDICATE) :GROUND ?extVal)

           (ONT::F ?loc2 (? tmp5 ONT::SOURCE-RELN ONT::GOAL-RELN ONT::IN-LOC ONT::AT-LOC ONT::ON ONT::OUTSIDE ONT::TRAJECTORY ONT::POS-AS-INTERSECTION) :GROUND ?locVal2)
           (?relnLoc2 ?locVal2 (? locType2 ONT::GEOGRAPHIC-REGION))
           (ONT::F ?time2 (? tmp6 ONT::TEMPORAL-LOCATION) :GROUND ?timeVal2)
           (?relnTime2 ?timeVal2 (? timeType2 ONT::TIME-OBJECT) :year ?yr2)
	   
           (ONT::EVAL (symbolmap ?type ?!eventName -rule20_1_AGENT_AFFECTED))
           -rule20_1_AGENT_AFFECTED>
           20
           (ONT::event ?ev ?!eventName
            :rule -rule20_1_AGENT_AFFECTED
            :AGENT ?ag
            :AFFECTED ?obj
	    :locmod ?tmp
	    :loc ?locVal
	    :locmod ?tmp5
	    :loc ?locVal2
	    :timemod ?tmp3
	    :time ?timeVal ;?yr
	    :timemod ?tmp6
	    :time ?timeVal2 ;?yr2
	    :extent ?extVal
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            ;:DRUM ?code
	    :location - ; suppress this as we have the info in :loc
            )
	   #|  ; when ?obj is absent, this produces (- - - :LOCATION -)
	   (?reln2 ?obj ?t2
	    :location - ; suppress this as we have the info in :loc
	    )
	   |#
           )
	  |#

	  #| ; 20180618: may be duplicates of the expanded rules
          ;; rule30_1_AGENT_AFFECTED rule with AGENT and AFFECTED
	  ;; cwms: made ?ag and ?obj optional and with no type constraint (not even REFERENTIAL-SEM) because the type could be an extracted type that does not exist in the TRIPS ontology
          ((?!reln0 ?ev
            (? type ONT::ENCOURAGE ONT::CAUSE-STIMULATE ONT::HELP ONT::IMPROVE ONT::INHIBIT-EFFECT ONT::CAUSE-COME-FROM ONT::REMOVE-FROM ONT::RENDER-INEFFECTIVE ONT::SUBDUING ) :AGENT ?ag :AFFECTED ?obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?ag  ?t1)
           (?reln2 ?obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule30_1_AGENT_AFFECTED))
           -rule30_1_AGENT_AFFECTED>
           30
           (ONT::event ?ev ?!eventName
            :rule -rule30_1_AGENT_AFFECTED
            :AGENT ?ag
            :AFFECTED ?obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
	  
          ;; rule40_4_AFFECTED_AFFECTED1 rule with AFFECTED and AFFECTED1
	  ;; cwms: made ?ag and ?obj optional and with no type constraint (not even REFERENTIAL-SEM) because the type could be an extracted type that does not exist in the TRIPS ontology
          ((?!reln0 ?ev
            (? type ONT::JOINING ONT::ATTRACT ONT::COALESCE ONT::BREAK-OBJECT ONT::SEPARATION ) :AGENT ?ag :AFFECTED ?obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR - )
           (?reln1 ?ag  ?t1)
           (?reln2 ?obj ?t2)
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule40_4_AGENT_AFFECTED))
           -rule40_4_AGENT_AFFECTED>
           40
           (ONT::event ?ev ?!eventName
            :rule -rule40_4_AGENT_AFFECTED
            :AGENT ?ag
            :AFFECTED ?obj
            :MODALITY ?modVal
            :FORCE ?fVal
            :DEGREE ?type_degree
            :FREQUENCY ?type_freq
            :TYPE ?type
            :DRUM ?code
            )
           )
	  |#

	  #|
          ;; copied from rule5_3_AGENT_AFFECTED rule with AGENT and AFFECTED
	  ;; cwms: made ?ag and ?obj optional
          ((?!reln0 ?ev
            ?type :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -)
           (?reln1 ?!ag ?t1)
           (?reln2 ?!obj ?t2 :OPERATOR ONT::AND-THUS :SEQUENCE (?!seq1 ?!seq2))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule5_3_AGENT_AFFECTED))
           -and-thus>
           5
           (?!reln0 ?ev ?type
            :rule -and-thus   
            :affected ?!seq1   ; :AGENT is passed on automatically
            )
           (?!reln0 *1 ?type
            :rule -and-thus-1
	    :agent ?!ag
            :affected ?!seq2
            )
	   (ONT::CC *2 ONT::MODULATE
	    :factor ?!seq1
	    :outcome ?!seq2
	    :rule -and-thus-2
	    )
	   )
	  |#

          ;; copied from rule5_3_AGENT_AFFECTED rule with AGENT and AFFECTED
	  ;; cwms: made ?ag and ?obj optional
          ((?!reln0 ?ev
            ?type :AGENT ?!ag :AFFECTED ?!obj :DRUM ?code :MODALITY ?modVal :FORCE ?fVal :DEGREE ?var_degree :FREQUENCY ?var_freq :OPERATOR -)
           (?reln1 ?!ag ?t1)
           (?reln2 ?!obj ?t2 :OPERATOR ONT::AND-THUS :SEQUENCE (?!seq1 ?!seq2))
           (ONT::F ?var_degree ?type_degree)  
           (ONT::F ?var_freq ?type_freq)
           (ONT::EVAL (symbolmap ?type ?!eventName -rule5_3_AGENT_AFFECTED))
           -and-thus>
           5
           (?!reln0 ?ev ?type
            :rule -and-thus   
            :affected ?!seq1   ; :AGENT is passed on automatically
            )
	   (ONT::CC *1 ONT::CAUSE
	    :factor ?!seq1
	    :outcome ?!seq2
	    :rule -and-thus-1
	    )
           )

	  
	  #|
	  ((?reln ?!obj ?t :OPERATOR ONT::AND-THUS :SEQUENCE (?!seq1 ?!seq2)) 
	   -and-thus>
	   90
	   (ONT::CC ?!obj ONT::CAUSE
	    :factor ?!seq1
	    :outcome ?!seq2
	    :sequence -
	    :operator -
	    :rule -and-thus
	    ))
	  |#

	  ;;;;;;;;;;;;;;;;;;;
	  ;;;; on + nominalization
	  ;;;;;;;;;;;;;;;;;;;
	  #|
          ((?!reln0 ?ev ONT::HAVE-PROPERTY :NEUTRAL ?n :FORMAL ?!f)
	   (?reln1 ?n ?type1) ; optional
	   (?!reln2 ?!f ONT::ASSOC-WITH :GROUND ?!gd)
	   (?!reln3 ?!gd (:* (? f-type ONT::CONFINE) ?w))
           -onNom>
           40
           (ONT::EVENT ?ev ?f-type  ; ?ev to keep other mods (not such a good solution) Note that this needs to be at a different extraction level from the ones that extract the mods
            :rule -onNom
	    :AFFECTED ?n  ; need other roles too
	    :NEUTRAL -
	    :FORMAL -
	    :passive + ; so that -loc2-affected won't apply
            )
           )
	  |#
	  
	  
	  )
	)
