;;;;
;;;; W::deteriorate
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::deteriorate
   (wordfeats (W::morph (:forms (-vb) :nom w::deterioration)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090504 :comments nil :vn ("entity_specific_cos-45.5") :wn ("deteriorate%2:29:00" "deteriorate%2:30:00"))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (LF-PARENT ONT::deteriorate)
 ; like ferment
     )
    ((meta-data :origin step :entry-date 20080623 :change-date 20090504 :comments nil)
     (LF-PARENT ONT::deteriorate)
     (example "the structure deteriorated")
     (templ affected-templ)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

