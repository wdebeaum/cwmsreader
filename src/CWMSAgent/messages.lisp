;;;;
;;;; messages.lisp for CWMS
;;;;
;;;;

(in-package :cwmsagent)

(in-component :cwmsagent)

(defcomponent-handler
  '(request &key :content (are-you-there . *))
     #'(lambda (msg args)
	 (process-reply msg args '(I-AM-HERE :WHO CWMSAGENT)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (restart . *))
  #'(lambda (msg args)
      (restart-cwms-if-not-from-myself msg)
      )
  :subscribe t)

(defcomponent-handler
  '(request &key :content (evaluate  . *))
     #'(lambda (msg args)
	 (process-reply msg args 
			(process-evaluate msg args)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (commit  . *))
     #'(lambda (msg args)
	 (process-commit msg args))
  :subscribe t)


(defcomponent-handler
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(process-reply msg args
		       (what-next msg args)))
			 
  :subscribe t)

(defcomponent-handler
    '(tell &key :content (define-parameter . *))
    #'(lambda (msg args)
	(apply #'define-parameter+ args))
			 
  :subscribe t)

;;  Format for defining services is
;;     (TELL :content (define-service :name <name of service provided>
;;                                          :component <TRIPS agent name providing service>
;;                                          :input ((input :name <unique name for the parameter - system will generate one if not specified>
;;                                                         :id-code <code in parameter ontology (e.g., ICASA)>
;;                                                         :unit <ontology type for unit>
;;                                                         :format <code in data format ontology, TBD>
;;                                                         :requirements <code: currently :optional or :required (default if not specified)>)*)
;;                                          :output ((output :name <parameter-name>
;;                                                           :id-code  <code in parameter ontology (e.g., ICASA)>
;;                                                           :unit <ontology type for unit>
;;                                                           :format <code in data format ontology>)*)
;;              ))
;;  EXAMPLE  (can be used for testing)
#||
(TELL :content (define-service :name compute-yield
		   :component PSIMS
		   :input ((input :name fert1
				  :id-code fertlevel
				  :unit (RATIO ont::KG ont::acre)
				  :format number
				  :requirements :required)
			   (input :name cultivar
				  :id-code cult
				  :format code
				  :requirements :required)
			   (input :name weather
				  :id-code weath
				  :format (csv location time)
				  :requirements :required)
			   (input :name location
				  :id-code location
				  :format geoloc
				  :requirements :required))
			   
		   :output ((output :name WHEAT-YIELD
				    :id-code WHYD
				    :unit ont::kg
				    :format number))
		   ))
||#


(defcomponent-handler
    '(tell &key :content (define-service . *))
    #'(lambda (msg args)
	(apply #'define-service+ args))
			 
  :subscribe t)

(defcomponent-handler
    '(request &key :content (plan-test . *))
    #'(lambda (msg args)
	(execute-plan *sample-plan* nil))
			 
  :subscribe t)



#||
(defcomponent-handler
  '(request &key :content (accepted . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))

  :subscribe t)

(defcomponent-handler
  '(request &key :content (notify-completed . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))
  :subscribe t)
||#

