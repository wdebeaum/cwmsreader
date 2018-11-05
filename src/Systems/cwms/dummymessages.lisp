;;;;
;;;; messages.lisp for DUMMY
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;  Here we pick up messages for all modules that don't exist yet, to
;;  allow developers to run the system as they add functionality
;;  As functionality is added, messages should be commented out

;;;  Here we have a chance to send any messages to simulate modules registering
(defun send-initial-messages nil
  (im::send-msg '(REQUEST :content (are-you-there :who CWMSAGENT))))

(defvar *service-msg-sent* nil)

(defcomponent-handler
  '(request &key :content (restart . *))
     #'(lambda (msg args)
	 (setq *service-msg-sent* nil)
	 (send-initial-messages)
	 )
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (i-am-here . *))
     #'(lambda (msg args)
	(when (not *service-msg-sent*)
	  (send-initial-dummy-messages))
  	(setq *service-msg-sent* t)
	)
  :subscribe t)


(defcomponent-handler
    '(request &key :content (identify-parameter . *))
     #'(lambda (msg args)
	 (process-reply msg args
			(identify-parameter args)))
  :subscribe t)

(defun identify-parameter (args)
  '(REPORT :content ?x 
    :context ((RELN ?x PARAMETER-ID :var ?v :id PROD))))


(defun send-initial-dummy-messages nil
  (im::send-msg '(TELL :content (define-service :name reduce
		   :component CWMS_AUX
		   :input (
			   ; required
			   (input :name input_val
				  :gloss "input values by year"
				  :id-code INPUT_VAL
				  :format complexD  ; complexD = netcdf or csv or...
				  :requirements :required)
			   (input :name operator
				  :gloss "what to iterate over"
				  :id-code OPERATOR
				  :format ont::function
				  :requirements :required)
			   #|
			   (input :name weight
				  :gloss "weights for aggregating results"
				  :id-code WEIGHT
				  :format varies ; <== should match some dimension of the input 
				  :requirements :optional)  ; default is equal weights
			   |#
			   )
			   
		   :output ((output :name res
				    :gloss "weighted value(s)"
				    :id-code RES
				    :format ont::number) ; one number for now (could also be complexD)
			    )
				 )))

  (im::send-msg '(TELL :content (define-service 
				 :name weather-estimator
				 :component builtin
				 :output
				 ((OUTPUT :NAME D_PROD :GLOSS
				    "global sum of harvest weight at maturity" :ID-CODE (:DELTA :PROD)
				    :ARGUMENTS (:LOCATION-FILE :CRID :PLYR) :UNIT ONT::MEGATONNE :FORMAT
				    (TABLE :PROD :LOCATION-FILE :CRID :PLYR)))
				 :input
				 ((INPUT :NAME PROD :GLOSS
				    "global sum of harvest weight at maturity" :ID-CODE PROD
				    :ARGUMENTS (:LOCATION-FILE :CRID :PLYR1) :UNIT ONT::MEGATONNE :FORMAT
				    (TABLE :PROD :LOCATION-FILE :CRID :PLYR))
				   (INPUT :NAME CRID :GLOSS "identifier of crop to simulate"
				    :ID-CODE CRID :ID-CODE-CONSTRAINT
				    (MAZ PML RIC SGG SBN WHB WHD WHT) :FORMAT ONT::CODE
				    :REQUIREMENTS :REQUIRED)
				   (INPUT :NAME PLYR :GLOSS "planting year(s) to simulate"
				    :ID-CODE YEAR :FORMAT (OR ONT::NUMBER ONT::LIST)
				    :ID-CODE-CONSTRAINT (2018 2019 2020)
				    :REQUIREMENTS :REQUIRED)
				   (INPUT :NAME PLYR1 :GLOSS "planting year(s) to simulate"
				    :ID-CODE YEAR :FORMAT (OR ONT::NUMBER ONT::LIST)
				    :constant-value (LIST 1980 2010)
				    :REQUIREMENTS :REQUIRED)
				   (INPUT :NAME LOCATION-FILE :GLOSS
				    "image file of geographic region to simulate" :ID-CODE
				    LOCATION :FORMAT ONT::COUNTRY-CODE :REQUIREMENTS
				    :REQUIRED)))
		  ))

  #|
   (im::send-msg '(TELL :content (define-service :name compute-food-shock
	:component ABN
		   :input (
			   ; required
			   (input :name crid
				  :gloss "Crop (or weed) species identifier"
				  :id-code CRID  ; ICASA
				  :id-code-constraint (WHT MAZ) ; <== add others 
				  :format ont::code
				  :requirements :required)
			   (input :name location
				  :gloss "geographic location"
				  :id-code LOCATION 
				  :format ont::country-code ; ISO or "WD" (world)
				  :requirements :required)
			   (input :name sc_year
				  :gloss "Simulation start year"
				  :id-code SC_YEAR ; ICASA
				  :format ont::number ; year 
				  :requirements :required)
			   #|
			   (input :name prod ; or change in production
				  :gloss "Harvest production at harvest maturity (dry wt)"
				  :id-code PROD  ; pSIMS
				  :unit ONT::kiloTonnes
				  :format ont::number
				  :requirements :required)
			   |#
			   (input :name d_prod ; or change in production
				  :gloss "Reduction in Harvest production at harvest maturity (dry wt)"
				  :id-code (Delta PROD) ; should be negative
				  :unit ONT::percentage
				  :format ont::number
				  :requirements :required)
			   (input :name loc_affected
				  :gloss "geographic locations of output"
				  :id-code LOC_AFFECTED
				  :format (list ont::country-code) ; e.g., (MX)
				  :requirements :optional)  ; default is LOCATION
			   )
			   
		   :output ((output :name consumption
				    :gloss "consumption"
				    :id-code CONSUM
				    :unit ONT::kCal
				    :format (table loc-affected ont::country-code consumption ont::number))
			    (output :name d_consumption
				    :gloss "change in consumption"
				    :id-code D_CONSUM
				    :unit ONT::percentage
				    :format (table loc-affected ont::country-code d_consumption ont::number))

			    ; add also supply, reserves, exports, imports
			    )
		   

		   )))
  
	(im::send-msg '(TELL :content (define-service :name compute-price
		   :component TWIST
		   :input (
			   ; required
			   (input :name crid
				  :gloss "Crop (or weed) species identifier"
				  :id-code CRID  ; ICASA
				  :id-code-constraint (WHT) ; only wheat (actually only red winter wheat)
				  :format ont::code
				  :requirements :required)
			   (input :name location
				  :gloss "geographic location"
				  :id-code LOCATION 
				  :format ont::country-code ; ISO or "WD" (world)
				  :requirements :required)
			   (input :name shock_year ; we should be able to do multiple shocks too
				  :gloss "shock year"
				  :id-code SHOCK_YEAR ; not the start of the simulation
				  :format ont::number ; year 
				  :requirements :required)
			   #|
			   (input :name prod ; or change in production
				  :gloss "Harvest production at harvest maturity (dry wt)"
				  :id-code PROD  ; pSIMS
				  :unit ONT::kiloTonnes
				  :format ont::number
				  :requirements :required)
			   |#
			   (input :name d_prod ; both increases and decreases
				  :gloss "Change in Harvest production at harvest maturity (dry wt)"
				  :id-code (Delta PROD)
				  :unit ONT::percentage  ; we should be able to do multiple shocks too
				  :format ont::number
				  :requirements :required)
			   (input :name loc_affected
				  :gloss "geographic locations of output"
				  :id-code LOC_AFFECTED
				  :format (list ont::country-code)
				  :requirements :optional)  ; default is LOCATION
			   (input :name years_affected
				  :gloss "years to output"
				  :id-code YEARS_AFFECTED
				  :format ont::number 
				  :requirements :required)
			   )
			   
		   :output ((output :name price
				    :gloss "price"
				    :id-code PRICE
				    :unit (RATIO1 ont::US_DOLLAR ont::MT) 
				    :format (table time1 ont::year loc-affected ont::country-code price ont::number))
			    (output :name d_price
				    :gloss "change in price"
				    :id-code D_PRICE
				    :unit ont::percentage 
				    :format (table time1 ont::year loc-affected ont::country-code d_price ont::number))

			    ; add also supply, consumption
			    )
		   
		   )))
  |#

  
  #|
 
  ;; also define a data-source
  
  (im::send-msg '(TELL :content (define-service :name compute-weather
		   :component WTH_SERVER
		   :input (
			   ; required
			   (input :name location
				  :gloss "geographic location"
				  :id-code LOCATION
				  :format ont::geoloc ; <== 
				  :requirements :required)
			   (input :name ref_year
				  :gloss "Simulation start year"
				  :id-code SC_YEAR ; ICASA
				  :format ont::number ; year 
				  :requirements :required)
			   (input :name num_years
				  :gloss "Duration of experiment in years"
				  :id-code EXP_DUR ; ICASA
				  :format ont::number 
				  :requirements :required) ; we could make this optional with default value
			   (input :name spatial_res
				  :gloss "spatial resolution"
				  :id-code SPATIAL_RES
				  :unit ont::geo-degree ; assume square, e.g., 2 degrees x 2 degrees
				  :format ont::number 
				  :requirements :required)
			   #| ; maybe we don't need this?  Always daily?
			   (input :name temporal_res1
				  :gloss "temporal resolution"
				  :id-code T_RES
				  :unit ont::date 
				  :format ont::number
				  :requirements :required)
			   |#

			   ; optional
			   (input :name weather_source1  ; e.g., AGMERRA
				  :gloss "Source of daily weather data for calculated means or weather generator parameters" ; too specific?  daily
				  :id-code WST_SOURCE ; ICASA
				  :format ont::code
				  :requirements :optional)
			   )
			   
		   :output (			   
			    (output :name TMAX
				    :gloss "Temperature of air, maximum"
				    :id-code TMAX ; ICASA
				    :unit ONT::CELSIUS
				    :format (netcdf (time1 ont::number tmax ont::number))			    
				    )
			    (output :name wth_comp
				    :gloss "Weather composite"
				    :id-code WTH_COMP
				    :format (netcdf (time1 ont::number SRAD ont::number TMIN ont::number TMAX ont::number RAIN ont::number)) ; ICASA
				    )
			    
			    ; TMIN, etc
			    )
		   )))
  
|#  

  
  )


