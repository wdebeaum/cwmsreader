;;;;
;;;; W::unchain
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::unchain
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("disassemble-23.3") :wn ("unchain%2:35:00"))
     (LF-PARENT ONT::unattach)
     (TEMPL agent-affected-source-optional-templ (xp (% w::pp (w::ptype w::from)))) ; like disconnect
     )
    )
   )
))
