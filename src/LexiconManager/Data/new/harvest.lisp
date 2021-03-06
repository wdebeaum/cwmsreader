;;;;
;;;; W::harvest
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::harvest
   (SENSES
    ((meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     ;(LF-PARENT ONT::acquire)
     (LF-PARENT ONT::COLLECT)
     )
    ((meta-data :origin calo-ontology :entry-date 20060426 :change-date nil :comments nil)
     ;(LF-PARENT ONT::acquire)
     (LF-PARENT ONT::COLLECT)
     (TEMPL agent-templ)
     )    
    )
   )
))

(define-words :pos W::n 
  :words (
	  (w::harvest
	   (senses
	    (
	     (LF-PARENT ONT::crop)
	     (TEMPL COUNT-PRED-TEMPL)
	     (example "the season's harvest")
	     )
	    )
	   )
	  ))
