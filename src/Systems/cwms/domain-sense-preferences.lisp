;;  the domain specific sense preferences
;;  the domain specific sense preferences
;;   entries are of the form (<word stem> <preferred type> <optional weight>)
;;;   <optional weight> may be between 1 and 10 and indicates the strength of the preference
;;   the default is 1, a slight preference. The value 10 forces this sense to be used no matter what.
;;   To prefer a sense no matter the word, add an entry of form (w::any-word T1 T2 T3 ...) and
;;   the types listed will always be preferred.

;;
;; (setf lxm::*domain-sense-preferences* '((w::expression ont::gene-expression 4)
;;					   (W::TRANSCRIPT ONT::RNA)
;;					   (W::SITE ONT::MOLECULAR-SITE 3)
;;					   (W::POSITION ONT::MOLECULAR-SITE)
;;                                         (w::any-word ONT::PROTEIN-FAMILY ONT::MOLECULE ONT::BIND-INTERACT))
;;

(setf lxm::*domain-sense-preferences* '(
					(w::development ONT::GROW 1)
					(w::attack ONT::ATTACK 4)
					))

