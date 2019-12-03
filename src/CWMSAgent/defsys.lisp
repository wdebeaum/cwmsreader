;;;;
;;;; defsys.lisp for CWMS behavioral Agent
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)  
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :ont)
  (load #!TRIPS"src;OntologyManager;ont-pkg"))

(unless (find-package :dagent)
  (load #!TRIPS"src;BasicDialogueAgent;defsys"))

(mk:defsystem :cwms-code
    :source-pathname #!TRIPS"src;CWMSAgent;"
    :components ( "structures"
		  "CWMS"
		  "code"
		  "operators"
		  "planner"
		  "messages"
		  "warn"
		;;  "CWMSrules"
		  "task-manager"
		  "taskDecisionFunctions"
		  "tasklibrary"
		  "CWMSgenerator"
		  ))

(dfc:defcomponent :cwmsAgent
  :use (:common-lisp :util :dagent)
  :system (:depends-on (:util :parser :im :dagent :cwms-code)))


(dfc:defcomponent-method dfc:init-component :after ()
  (initialize)
  )

(defun initialize nil
  ;(send-msg '(REQUEST :content (RESTART)))
  ;(send-msg '(TELL :content (I-AM-HERE :who CWMSAGENT)))
  (setq *user* (dagent::lookup-user 'desktop))
  (restart-cwms)
  (setq dagent::*suppress-context-on-what-next* t)  ;;  this simplifies message traffic as we already have the context
  )
  
(defun run ()
  (dfc:run-component :cwmsAgent)
  )

(defvar *replyCounter* 0)

(defvar *cwms-package* (find-package :cwmsagent))

(defvar *user* nil)

