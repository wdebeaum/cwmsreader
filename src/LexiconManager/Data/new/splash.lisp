;;;;
;;;; W::splash
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::splash
   (wordfeats (W::morph (:forms (-vb) :nom w::splash)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("spray-9.7-1") :wn ("splash%2:35:00" "splash%2:35:04"))
     (LF-PARENT ONT::push-liquid)
 ; like spray
     )
    ((meta-data :origin caet :entry-date 20110114 :change-date nil :comments nil :vn ("spray-9.7-1"))
     ;(LF-PARENT ONT::cause-cover)
     (LF-PARENT ONT::push-liquid)
     (example "the paint splashed on the wall")
     (templ affected-templ)
     )
    )
   )
))

