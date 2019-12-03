(in-package :pdflearn)

(defun get-edge-coord-estimates (old-sit new-sit region-map dim b-side)
  "Estimate the coordinate of the edge of the target B region indicated by dim
   and b-side, using each of the relevant constraints learned in old-sit as well
   as the region-map from old-sit to new-sit. Return the list of estimates,
   each with the old and new A region descriptions."
  ;; for each constraint relevant to this edge, whose A is mapped
  (loop for c in (situation-constraints old-sit)
		       ;; TODO try more than just first
	for new-a-id = (first (gethash (constraint-a c) region-map))
	when (and new-a-id
		  (eq (car dim) (constraint-dimension c))
		  (eq b-side (constraint-b-side c))
		  (eq (situation-target-region old-sit)
		      (constraint-b c)))
	;; compute where the constraint says the edge should be in
	;; the new situation
	collect
	   (let ((new-a-desc
		   (find new-a-id (situation-regions new-sit)
			 :key #'id)))
	     (multiple-value-bind (coord old-a-desc)
		 (estimate-edge-coord old-sit c new-a-desc)
	       (format t "edge ~s-~s estimated to be at ~s by:~%  ~s~%  ~s~%"
			 b-side (car dim) coord new-a-desc c)
	       (list coord old-a-desc new-a-desc)))
	))

(defun modes (l &key (test #'eql))
  "Return the list of distinct values in l that occur more times than any other
   values in l. Return NIL if the l is empty (i.e. no values occur at all)."
  (loop with max-count = 0
	with modes = nil
        for e in (remove-duplicates l :test test)
	for c = (count e l :test test)
	do (cond
	     ((> c max-count)
	       (setf max-count c
		     modes (list e)))
	     ((= c max-count)
	       (push e modes))
	     )
	finally (return modes)))

(defun average (l)
  "Compute the average of a list of numbers. Return NIL if the list is empty."
  (when l
    (/ (reduce #'+ l) (length l))))

(defun align-assoc-with-matches (old-sit new-sit region-map)
  "Attempt to add entries to the region-map hash aligning each assoc-with match
   region in old-sit to one in new-sit."
  ;; NOTE: I don't think this is technically guaranteed to find the mapping
  ;; with the minimum total distance, but I think it should be close enough in
  ;; practice
  (loop with scored-pairs = nil
        ;; get the region distance + match distance for each pair of matches
        with old-page = (situation-page old-sit)
        with old-regions = (situation-regions old-sit)
	with new-regions = (situation-regions new-sit)
	for old-match in (situation-assoc-with-matches old-sit)
	for old-id = (find-arg-in-act old-match :region)
	for old-region = (find old-id old-regions :key #'id)
	do
	  (loop for new-match in (situation-assoc-with-matches new-sit)
	        for new-id = (find-arg-in-act new-match :region)
		for new-region = (find new-id new-regions :key #'id)
		for dist =
		  (+ (region-distance-metric new-region old-region old-page)
		     (match-distance-metric new-match old-match))
		do (push (list old-id new-id dist) scored-pairs)
		)
	finally
	  ;; sort the pairs by increasing distance metric
	  (setf scored-pairs (sort scored-pairs #'< :key #'third))
	  ;; assign each pair neither of whose sides has already been assigned,
	  ;; to region-map
	  (loop with new-ids-already-used = nil
		for (old-id new-id dist) in scored-pairs
		unless (or (gethash old-id region-map)
		           (member new-id new-ids-already-used))
		do (setf (gethash old-id region-map) (list new-id))
		   (push new-id new-ids-already-used)
		)
	))

(defun handle-find (msg args)
  (destructuring-bind (&key rule page ((:assoc-with new-assoc-with) nil))
		      args
    (let ((old-sit (gethash rule *rules*))
    	  (new-sit (make-situation :page page))
	  ;; map old region IDs to lists of possibly-corresponding new ones
	  (region-map (make-hash-table :test #'eq))
	  (target-coords (list :page page)))
      (unless old-sit
        (error "unknown rule: ~s" rule))
      (unless (typep old-sit 'situation)
        (error "wrong type of rule"))
      (detect-reference-regions page)
      (with-slots ((old-target-region target-region)
		   (old-assoc-with assoc-with)
                   (old-assoc-with-matches assoc-with-matches)
		   (old-regions regions)
		   (old-constraints constraints)
		   (old-column-headings column-headings))
		  old-sit
	;; if there were any assoc-with matches in the learned situation,
	;; search for the assoc-with text on the new page, record the new
	;; matches in the new situation, and attempt to align the two sets of
	;; matches, recording the alignment in region-map
        (when old-assoc-with-matches
	  (setf (situation-assoc-with new-sit)
		(or new-assoc-with old-assoc-with))
	  (setf (situation-assoc-with-matches new-sit)
	        (search-page (situation-assoc-with new-sit) page))
	  (loop for match in (situation-assoc-with-matches new-sit)
		for region-desc = (find-arg-in-act match :region)
		for region-id = (id region-desc)
		do (add-region-to-situation new-sit region-desc
		       :with-relation nil)
		   (setf (second (member :region match)) region-id)
		)
	  (align-assoc-with-matches old-sit new-sit region-map)
	  )
	;; search for exact matches for column heading texts, if any, and add
	;; them to region-map
	(when old-column-headings
	  (loop for (old-id content) in old-column-headings
	        for matches = (search-page content page)
		do
		  (dolist (match matches)
		    (destructuring-bind (_ &key context region
					 &allow-other-keys) match
		        (declare (ignore _))
		      (when (strings-match-p content context)
			(push (id region) (gethash old-id region-map)))))
		))
	; TODO use row heading ONT type somehow
	;; find the most similar regions to each yet-unmapped region from the A
	;; side of a constraint in the situation
	(loop for old-region-id in
		(remove-duplicates (mapcar #'constraint-a old-constraints))
	      for old-region = (find old-region-id old-regions :key #'id)
	      unless (gethash old-region-id region-map) ; unless already mapped
	      do (loop for new-region in
			 (find-most-similar-regions old-region page)
		       do (add-region-to-situation new-sit new-region
			      :with-relation nil)
			  (pushnew (id new-region)
				   (gethash old-region-id region-map))
		       )
	      )
	)
      (select-only-regions page (mapcar #'id (situation-regions new-sit)))
      (format t "region-map:~%")
      (loop for k being the hash-keys of region-map using (hash-value v)
            do (format t "  ~s => ~s~%" k v))
      ; TODO try the below for different specific 1:1 region maps
      ;; for each edge of the target region
      (dolist (dim +dimensions+)
        (dolist (b-side '(min max))
	  ;; get estimates for the edge's coordinate from each relevant
	  ;; constraint
	  (let* ((ests (get-edge-coord-estimates
			   old-sit new-sit region-map dim b-side))
		 ;; get the most frequent estimates
		 (est-modes (modes (mapcar #'car ests) :test #'=))
		 (old-target-desc
		   (find (situation-target-region old-sit)
			 (situation-regions old-sit)
			 :key #'id)))
	    ;; if there's more than one mode, and some are for A regions that
	    ;; are supposed to be inside the target regions, use only those
	    ;; estimates
	    (when (and (> (length est-modes) 1)
		       (some (lambda (e)
			       (and (member (first e) est-modes :test #'=)
				    (inside-p (second e) old-target-desc)))
			     ests))
	      (setf est-modes
		    (delete-if-not
		      (lambda (m)
			(some (lambda (e)
				(and (= (first e) m)
				     (inside-p (second e) old-target-desc)))
			      ests))
		      est-modes)))
	    ;; add the estimate to target-coords with the right keyword
	    ;; labeling it
	    ; TODO? weight average by inverse distance score?
	    (push (average est-modes) target-coords)
	    (push (intern (format nil "~s-~a" b-side (symbol-name (car dim)))
			  :keyword)
		  target-coords)
	    )))
      (format t "(make-region ~(~{~s~^ ~}~))~%" target-coords)
      ;; construct new target region from the above-computed coords, and select
      ;; it, getting its ID from PDFExtractor
      (let* ((new-target-region-desc (apply #'make-region target-coords))
             (new-target-region-id (select-region new-target-region-desc)))
	; TODO
	; - check that all old relations continue to hold (if not, try the next region map option, or fall back to just selecting the old region on the new page)
	(setf (gethash new-target-region-id *region-to-rule*) rule)
        (reply-to-msg msg 'reply :content
	  `(report :content (answer :target-region ,new-target-region-id)))))))

(defun irrelevance-score (rule page-id)
  "Get a nonnegative number that is closer to zero the more relevant the given
   finding rule is to the given page, such that different rules for the same
   page may be compared."
  ;; get the average distance score for the regions used by the rule, except
  ;; for the target region (which wouldn't be there yet)
  ;; FIXME? I'm not sure this is so great for comparing two rules whose regions are generally of different sizes
  (average
    (mapcar
      (lambda (rule-region)
        (find-arg-in-act
	  (first (find-most-similar-regions rule-region page-id))
	  :distance))
      (remove (situation-target-region rule)
	      (situation-regions rule)
	      :key #'id)
      )))

(defun handle-get-relevant-find-rules (msg args)
  (destructuring-bind (&key page limit soft-limit) args
    (detect-reference-regions page)
    (loop for rule-id being the hash-keys of *rules* using (hash-value rule)
          when (typep rule 'situation)
	    collect (list rule-id (irrelevance-score rule page))
	    into scored-rule-ids
	  finally
	    ;; sort by increasing irrelevance
	    (setf scored-rule-ids (sort scored-rule-ids #'< :key #'second))
	    ;; truncate the list according to soft-limit
	    (when (and soft-limit (> (length scored-rule-ids) soft-limit))
	      (loop for i upfrom soft-limit
	            while (and (< i (length scored-rule-ids))
		               (= (second (nth i scored-rule-ids))
			          (second (nth (- i 1) scored-rule-ids))))
		    finally
		      (setf scored-rule-ids (subseq scored-rule-ids 0 i))
		    ))
	    ;; truncate the list according to limit
	    (when (and limit (> (length scored-rule-ids) limit))
	      (setf scored-rule-ids (subseq scored-rule-ids 0 limit)))
	    ;; answer
	    (reply-to-msg msg 'reply :content
	      `(report :content (answer :rules ,scored-rule-ids)))
	    )))
