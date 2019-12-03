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
  ;;(:dfc-component       :dummy     	   #!TRIPS"src;Dummy;")
  (:dfc-component       :pdflearn          #!TRIPS"src;PDFLearn;")
  (:dfc-component	:deepsemlex	   #!TRIPS"src;DeepSemLex;code;lib;")
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser;defsys.lisp")
  (nconc (assoc :cwms trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)
;; this isn't part of a lisp component so we load it separately
(load #!TRIPS"src;TextTagger;cwms-dsl-resources.lisp")

;; domain preferences
(load #!TRIPS"src;Systems;cwms;domain-sense-preferences")
(load #!TRIPS"src;Systems;cwms;domain-words.lisp")

;;;; extractor rules: used in cwmsreader but not cwms
(load #!TRIPS"src;Systems;cwms;preprocessRules.lisp")
(load #!TRIPS"src;Systems;cwms;cwmsRules.lisp")
(load #!TRIPS"src;Systems;cwms;DRUMRules_ev.lisp")
(load #!TRIPS"src;Systems;cwms;cwmsRules_ev_add.lisp")
(load #!TRIPS"src;Systems;cwms;DRUMRules_mod.lisp")
(load #!TRIPS"src;Systems;cwms;cwmsRules_CC.lisp")
(load #!TRIPS"src;Systems;cwms;postprocessRules.lisp")
(load #!TRIPS"src;Systems;cwms;postprocessRules_F.lisp")
(load #!TRIPS"src;Systems;cwms;symbolmapping.lisp")

(setq im::*symbol-map* (append im::*symbol-map* '((ONT::QUANTITY-ABSTR ONT::QTY)
						  (ONT::OBJECTIVE-INFLUENCE ONT::INFLUENCE im::-rule5_3_AGENT_AFFECTED)
						  )))


;;  loading the dummy message handling

;; the :dummy component is used to fake certain message interactions during
;; system development.
;; if you need to use either of the following Dummy features, uncomment them
;; LOCALLY, but please do not commit without comments!

;;(load #!TRIPS"src;Systems;cwms;dummymessages.lisp")

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))
