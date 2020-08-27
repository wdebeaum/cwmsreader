(in-package "IM")

(reset-im-rules 'postprocessRules)

(mapcar #'(lambda (x) (add-im-rule x 'postprocessRules))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; normalize ASSOCs and QUALs
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  ((?reln ?!obj ?t :QUAL1 ?q1 :QUAL1b ?q1b :QUAL2 ?q2)
	   (?reln1 ?q1 ?t1) ; match these so that -normQUAL and -normASSOC can both fire for the same clause
	   (?reln1b ?q1b ?t1b)
	   (?reln2 ?q2 ?t2)
	   -normQUAL>
	   100
	   (?reln ?!obj ?t
	    :QUAL ?q1
	    :QUAL ?q1b
	    :QUAL ?q2
	    :QUAL1 -
	    :QUAL1b -
	    :QUAL2 -
	    :rule -normQUAL
	    ))

	  ; this doesn't seem to work if we try to match two slots of the same name (e.g., two :ASSOC slots)
	  ((?reln ?!obj ?t :ASSOC1 ?a1 :ASSOC1b ?a1b :ASSOC2 ?a2 :ASSOC3 ?a3 :ASSOC4 ?a4)
	   (?reln1 ?a1 ?t1)
	   (?reln1b ?a1b ?t1b)
	   (?reln2 ?a2 ?t2)
	   (?reln3 ?a3 ?t3)
	   (?reln4 ?a4 ?t4)
	   -normASSOC>
	   100
	   (?reln ?!obj ?t
	    :ASSOC ?a1
	    :ASSOC ?a1b
	    :ASSOC ?a2
	    :ASSOC ?a3
	    :ASSOC ?a4
	    :ASSOC1 -
	    :ASSOC1b -
	    :ASSOC2 -
	    :ASSOC3 -
	    :ASSOC4 -
	    :rule -normASSOC
	    ))
	  
))
