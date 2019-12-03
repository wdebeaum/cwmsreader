(in-package :pdflearn)

(defstruct situation
  target-region ; ID of the region to be found
  page ; description of the page all of this is on
  table-cells ; list of table-cell structures from reading the HTML table file
  assoc-with ; string to search for
  (assoc-with-matches nil) ; matches for assoc-with on page
  regions ; descriptions of relevant regions
  (relations nil) ; descriptions of relevant relations among the regions
  (constraints nil) ; constraints on specific edges of target-region as (find-related-region-args related-regions) pairs
  (column-headings nil) ; (region-id "heading content") pairs
  (row-headings nil) ; (region-id ONT::type) pairs
  )

(defun add-region-to-situation (sit region-desc &key (with-relation t))
  "Make sure the described region is in the situation, adding it (without
   :page) if a region with the same ID is not already there. If :with-relation
   is T, find the relation between the new region and the target region and add
   it to the situation as well. If it's NIL, don't add a relation. If it's a
   relation, add that relation. A relation will only be added if the region is."
  (with-slots (target-region regions relations) sit
    (unless (find (id region-desc) regions :key #'id)
      (push (remove-arg-in-act region-desc :page) regions)
      (cond
        ((eq t with-relation)
	  (push (relate-regions (id region-desc) target-region) relations))
	(with-relation
	  (push with-relation relations))
	)
      )))

(defun ont-type-abstract-p (ont-type)
  "Is ont-type so abstract that we suspect some bogus types among the types
   that it is the common ancestor of?"
  (member ont-type '(
      ont::referential-sem
      ont::situation-root
      ont::abstract-object
      ont::phys-object
      ont::predicate
      ont::property-val
      )))

(defun approximate-common-ancestor-ont-type (ont-types)
  "Return the lowest common ancestor of all or most of the given ONT types."
  (unless ont-types
    (error "can't find common ancestor of an empty list of ONT types"))
  (let* ((nodupes (remove-duplicates ont-types))
	 (of-all (reduce #'om::common-ancestor nodupes)))
    (cond
      ((not (ont-type-abstract-p of-all))
        of-all)
      ((> (length ont-types) 2)
	;; we have many ont types and the result is abstract; try leaving out
	;; one type at a time
	(loop for left-out in ont-types
	      when (= 1 (count left-out ont-types))
	      do (let* ((most (remove left-out nodupes))
			(of-most (reduce #'om::common-ancestor most)))
		  (unless (ont-type-abstract-p of-most)
		    (return of-most)))
	      finally
		(warn "unable to find non-abstract common ancestor ONT type for rows by leaving one out; returning abstract ~s for row ONT types ~s" of-all ont-types)
		(return of-all)
	      ))
      (t ; abstract, but too few
        (warn "unable to find non-abstract common ancestor ONT type for few rows; returning abstract ~s for row ONT types ~s" of-all ont-types)
	of-all)
      )))

(defun learn-to-find-headings (sit)
  (with-slots (target-region table-cells page constraints
	       column-headings row-headings) sit
    ;; for each row- or column-heading cell from the HTML table
    (loop with first-col-heading = nil
	  with last-col-heading = nil
	  with last-row-heading = nil
	  for cell in table-cells
	  for chp = (table-cell-column-heading-p cell)
	  for rhp = (table-cell-row-heading-p cell)
	  for hp = (or chp rhp)
	  ;; get its text content and search for it on the PDF page
	  for content = (when hp (table-cell-content cell))
	  for matches = (when hp (search-page content (id page)))
	  ;; get only matches whose context is just the heading content,
	  ;; and whose region lies within the target region
	  for good-matches =
	    (remove-if-not
	      (lambda (match)
		(destructuring-bind (_ &key context region
				     &allow-other-keys) match
		    (declare (ignore _))
		  (and (strings-match-p content context)
		       (region-contains-region-p target-region (id region))
		       )))
	      matches)
	  ;; if there is exactly one such match (i.e. we're not confused
	  ;; about which region the heading came from)
	  when (= 1 (length good-matches))
	  do
	    (destructuring-bind (_ &key region &allow-other-keys)
				(first good-matches)
		(declare (ignore _))
	      (when chp ; this is a column heading
		;; add the matched region to the situation
		(add-region-to-situation sit region)
	        ;; remember the text
		(push (list (id region) content) column-headings)
		;; constrain min-ys
		;; TODO? check that this makes a close constraint, instead
		;; of blindly assuming y min min
		(push (make-constraint
			:dimension (second +dimensions+) ; y
			:a (id region) :a-side 'min
			:b target-region :b-side 'min)
		      constraints)
		;; track first and last column headings
		(when (or (null first-col-heading)
			  (< (min-x region) (min-x first-col-heading)))
		  (setf first-col-heading region))
		(when (or (null last-col-heading)
			  (> (max-x region) (max-x last-col-heading)))
		  (setf last-col-heading region))
		)
	      ;; for a row heading, remember the ONT type
	      (when rhp ; this is a row heading
	        ;; remember the ONT type
		(let ((ont-type 'ont::country ; DEBUGGING
			#|(lf-content-type
			  (send-utterance-and-wait content
			    :channel 'heading))|#))
		  (push (list (id region) ont-type) row-headings))
		;; track last row heading
		;; FIXME often whole columns or sections of columns are merged into single paragraph regions, so the "last" row heading is really just the last one this hasn't happened to, and might not be near the bottom of the target region
		(when (or (null last-row-heading)
			  (> (max-y region) (max-y last-row-heading)))
		  (setf last-row-heading region))
		)
	      )
	  finally
	    ;; add constraints using extreme headings
	    (when column-headings
	      (push (make-constraint
		      :dimension (first +dimensions+) ; x
		      :a (id first-col-heading) :a-side 'min
		      :b target-region :b-side 'min)
		    constraints)
	      (push (make-constraint
		      :dimension (first +dimensions+) ; x
		      :a (id last-col-heading) :a-side 'max
		      :b target-region :b-side 'max)
		    constraints))
	    (when row-headings
	      (format t "row-headings:~%  ~s~%" row-headings)
	      ;; only add the last row heading, and only if we can find a
	      ;; common ancestor ONT type for it
	      (let ((ont-types (remove nil (mapcar #'second row-headings))))
	        (when ont-types
		  (let ((ont-type
			  (approximate-common-ancestor-ont-type ont-types)))
		    (setf row-headings `((,(id last-row-heading) ,ont-type)))
		    (add-region-to-situation sit last-row-heading)
		    (push (make-constraint
			    :dimension (second +dimensions+) ; y
			    :a (id last-row-heading) :a-side 'max
			    :b target-region :b-side 'max)
			  constraints)
		    )))
	      )
	  ; TODO? find the set of possible relations between one row heading and the next, and use that during execution to try to iteratively find the bottom of the table (or just hardcode it to :vertically before/after :horizontally overlap/etc.? maybe hardcode vertical and learn horizontal?)
	  )))

(defun handle-learn-to-find (msg args)
  (destructuring-bind (&key target-region (table-file nil) (assoc-with nil))
		      args
    (let* ((rule-id (gentemp "RULE"))
	   (target-region-desc (get-pdf-extractor-description target-region))
	   (page (find-arg-in-act target-region-desc :page)) ; NOTE: no bounds
	   (page-id (id page))
	   (sit
	     (make-situation
	       :target-region target-region
	       :page (get-pdf-extractor-description page-id) ; including bounds
	       :table-cells
		 (when table-file
		   (read-html-table-from-pdf-extractor
		       (find-arg-in-act table-file :name)))
	       :assoc-with assoc-with
	       :regions (list (remove-arg-in-act target-region-desc :page))
	       )))
      (detect-reference-regions page-id)
      (when assoc-with
        ;; search for matches for assoc-with on the page
        (setf (situation-assoc-with-matches sit)
	      (search-page assoc-with page-id))
	;; move the structure from :region in each match to situation-regions,
	;; and leave the ID in its place (also remove redundant :page arg).
	(loop for match in (situation-assoc-with-matches sit)
	      for region-desc = (find-arg-in-act match :region)
	      for region-id = (id region-desc)
	      do (add-region-to-situation sit region-desc)
		 (setf (second (member :region match)) region-id)
	      )
	)
      (when table-file
        (learn-to-find-headings sit))
      (find-constraints sit)
      ; TODO [old]
      ; - figure out how to abstract sit to a rule that when executed finds the target-region
      ; ? as a special case, try to match text of top left and top right regions within the target-region, since they are likely column headings
      ; ? maybe also match properties of the bottom left/right regions, like their dimensions, or the ONT type resulting from parsing their contents
      ;; show only the regions we kept in the new rule
      (select-only-regions
          (id (situation-page sit)) (mapcar #'id (situation-regions sit)))
      (add-rule rule-id sit)
      (setf (gethash target-region *region-to-rule*) rule-id)
      (reply-to-msg msg 'reply :content
        `(report :content (answer :rule ,rule-id)))
      )))
