(in-package "IM")

(reset-im-rules 'postprocessF)

(mapcar #'(lambda (x) (add-im-rule x 'postprocessF))  ;; sets of rules are tagged so they can be managed independently 
	'(
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ; extract all F's under SITUATION-ROOT as EVENTs
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  (((? !reln ONT::EVENT ONT::TERM ONT::EPI ONT::CC) ?!obj (? t ONT::SITUATION-ROOT ))
	   -eventExtra>
	   100
	   (ONT::EVENT ?!obj ?t
	    :rule -eventExtra
	    ))

))
