
; What happens to the price of wheat in Mexico if we cut the amount of fertilizer by 50%?

; spaceman

(request :content (compute-geo-location
	 :description "Mexico"
    	 :format 2deg-tile
	 :output (location)))

(reply :content (ANSWER
       	:location (file 2deg-tile "/Users/teentext/cwms/etc/Spaceman/cache/...")
    	       ))

; pSIMS

(request :content (compute-crop-production
    :crid WHT 
    :location (file 2deg-tile "/Users/teentext/cwms/etc/Spaceman/cache/...")
    :ref_year 2008
    :num_years 1
    :fen_tot (* fen_tot 0.5)
    :output (hwam prod)))  ; remove the other variables from the output file

(reply :content (ANSWER
		 :hwam ...
		 :prod (file netcdf "/Users/teentext/cwms/etc/CropModeller/cache/psims000.nc4")))

; reduce 

(request :content (reduce
	 :input_val (file netcdf "/Users/teentext/cwms/etc/CropModeller/cache/psims000.nc4")
	 :operator (total crop_prod1 :over (lat long) :for (year 2008) ; with weight = ... ?
	 ;:operator (for-each year (total crop_prod1 :over <lat, long>))
	 :output (res)))  ; one number

(reply :content (ANSWER
	 :res 10250000
	 ))

;;;
; called above pSIMS/reduce twice to get difference in production
;;;

; spaceman

(request :content (compute-geo-location
	 :description "Mexico"  ; "World"
    	 :format ont::country-code
	 :output (location)))

(reply :content (ANSWER
       	:location MX  ; "WD"
    	       ))

; ABN

(request :content (compute-food-shock
    :crid WHT 
    :location MX
    :sc_year 2008
    :d_prod -3.21
    :output (consumption))) ; or maybe d_consumption

(reply :content (ANSWER
    :consumption ((MX 987)) ; for WD this will be a list of ((MX 987) (AU 765)...)
    ))


; TWIST

(request :content (compute-price
    :crid WHT
    :location WD
    :shock_year 2008
    :d_prod -3.21
    :years_affected 10
    :output (price))) 

(reply :content (ANSWER
    :price ((2008 WD 56.7) (2009 WD 59.2) (2010 WD 51.4)...)
    ))

