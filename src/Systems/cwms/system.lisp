;;;;
;;;; File: system.lisp
;;;;
;;;; Defines and loads an instance of the TRIPS system for CWMS
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :cwms
  (:dfc-component	:lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :im                #!TRIPS"src;NewIM;")
  (:dfc-component       :dagent            #!TRIPS"src;BasicDialogueAgent;")
  (:dfc-component       :CWMSAgent     	   #!TRIPS"src;CWMSAgent;")
  (:dfc-component       :dummy     	   #!TRIPS"src;Dummy;")
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser;defsys.lisp")
  (nconc (assoc :cwms trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)

;; domain preferences
(load "domain-sense-preferences")
(load "domain-words.lisp")

;;;; extractor rules
(load "preprocessRules.lisp")
(load "cwmsRules.lisp")
(load "DRUMRules_ev.lisp")
(load "cwmsRules_ev_add.lisp")
(load "DRUMRules_mod.lisp")
(load "cwmsRules_CC.lisp")
(load "postprocessRules.lisp")
(load "symbolmapping.lisp")
(setq im::*symbol-map* (append im::*symbol-map* '((ONT::QUANTITY-ABSTR ONT::QTY)
						  (ONT::OBJECTIVE-INFLUENCE ONT::INFLUENCE im::-rule5_3_AGENT_AFFECTED)
						  )))


;;  loading the dummy message handling

;; the :dummy component is used to fake certain message interactions during
;; system development.
;; if you need to use either of the following Dummy features, uncomment them
;; LOCALLY, but please do not commit without comments!

(load #!TRIPS"src;Systems;cwms;dummymessages.lisp")

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))
