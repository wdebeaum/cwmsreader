(in-package :pdflearn)

(defstruct int-reln ; interval relation type
  name ; A name B
  inverse ; B inverse A
  constrained-rank ; rank of this type for the dimension we're constraining
  unconstrained-rank ; rank of this type for the other dimension
  side-pairs-constrained ; pairs of min and max for the sides of the two intervals that may constrain each other; the closest pair(s) will be used
  )

(defun make-int-relns ()
  (let ((inverses '(
          ; forward	inverse		side-pairs-constrained (for forward)
          (before	after		((max min)))
	  (meets	met-by		((max min)))
	  (overlaps	overlapped-by	((min min) (max min) (max max)))
	  (finished-by	finishes	((max max)))
	  (around	inside		((min min) (max max)))
	  (starts	started-by	((min min)))
	  (equal	equal		((min min) (max max)))
	  ))
        (constrained-ranking '(
	  (equal)			; ((min min) (max max))
	  (starts finishes)		; ((min min)) ((max max))
	  (meets met-by) 		; ((max min)) ((min max))
	  (started-by finished-by)	; ((min min)) ((max max))
	  (inside)			; ((min min) (max max))
	  (before after)		; ((max min)) ((min max))
	  (around)			; ((min min) (max max))
	  (overlaps overlapped-by)	; ((min min) (max min) (max max)) ((min min) (min max) (max max))
	  ))
	(unconstrained-ranking '(
	  (equal)
	  (starts finishes)
	  (meets met-by)
	  (started-by finished-by)
	  (around inside)
	  (overlaps overlapped-by)
	  (before after)
	  ))
	relns)
    ;; make all the structures, and set :inverse and :side-pairs-constrained
    (loop for (forward inverse side-pairs) in inverses do
      (push (make-int-reln
		:name forward
		:inverse inverse
		:side-pairs-constrained side-pairs)
	    relns)
      (unless (eq forward inverse)
        (push (make-int-reln
		  :name inverse
		  :inverse forward
		  :side-pairs-constrained (mapcar #'reverse side-pairs))
	      relns))
      )
    ;; set :constrained-rank
    (loop for names in constrained-ranking
          for rank upfrom 1
	  do (loop for name in names
	           for reln = (find name relns :key #'int-reln-name)
		   do (setf (int-reln-constrained-rank reln) rank)
		   )
	  )
    ;; set :unconstrained-rank
    (loop for names in unconstrained-ranking
          for rank upfrom 1
	  do (loop for name in names
	           for reln = (find name relns :key #'int-reln-name)
		   do (setf (int-reln-unconstrained-rank reln) rank)
		   )
	  )
    ;; sort in the order we try sending them to PDFExtractor
    (sort relns #'< :key #'int-reln-constrained-rank)))

(defvar +int-relns+ (make-int-relns))
(defvar +all-overlapping-int-reln-names+
  (set-difference (mapcar #'int-reln-name +int-relns+) '(before after)))

(defvar +dimensions+ '(
  (:x :w :horizontally)
  (:y :h :vertically)
  ))

(defun other-dimension (dim)
  "Given one element of +dimensions+, return the other one."
  (first (remove (first dim) +dimensions+ :key #'first)))

(defstruct constraint
  dimension ; :x or :y
  (a nil) ; ID of region A
  a-side ; 'min or 'max
  (b nil) ; ID of region B
  b-side ; 'min or 'max
  )

(defun reverse-constraint (c)
  (with-slots (dimension a a-side b b-side) c
    (make-constraint
        :dimension dimension
	:a b :a-side b-side
	:b a :b-side a-side)))

(defun constraint-order-by (c)
  "Make an :order-by argument from a constraint. min->asc, max->desc."
  (with-slots (dimension a-side) c
    (list
      (intern (format nil "~a-~a" (symbol-name a-side)
				  (symbol-name dimension)))
      (if (eq a-side 'min) 'asc 'desc)
      )))

(defun edge-coord (region-desc edge-name)
  (ecase edge-name
    (min-x (min-x region-desc))
    (min-y (min-y region-desc))
    (max-x (max-x region-desc))
    (max-y (max-y region-desc))
    ))

(defun edge-displacement (a-desc order-by b-desc)
  (let ((edge-name (first order-by)))
    (- (edge-coord a-desc edge-name) (edge-coord b-desc edge-name))))

(defun edge-distance (a-desc order-by b-desc)
  (abs (edge-displacement a-desc order-by b-desc)))

(defun overlap-size (a-desc dim b-desc)
  "Return the size, in pixels, of the overlap between regions A and B along the
   given dimension."
  (let* ((a-min (find-arg-in-act a-desc (first dim)))
         (b-min (find-arg-in-act b-desc (first dim)))
	 (a-size (or (find-arg-in-act a-desc (second dim)) 1))
	 (b-size (or (find-arg-in-act b-desc (second dim)) 1))
	 (a-max (1- (+ a-min a-size)))
	 (b-max (1- (+ b-min b-size))))
    (if (or (< a-max b-min) (< b-max a-min))
      0 ; no overlap
      (let ((sorted-coords (sort (list a-min a-max b-min b-max) #'<)))
        ;; when there is overlap, it's
        ;; 1 + the difference of the middle two coordinates
        (1+ (- (third sorted-coords) (second sorted-coords))))
      )))

(defun estimate-edge-coord (old-sit c new-a-desc)
  "Given an old situation and a constraint in it, as well as a new A-region
   description, return an estimate for the coordinate of the relevant edge of
   the B-region in the new situation. Also return the description of the old
   A-region."
  (let* ((old-a-desc
	   (find (constraint-a c) (slot-value old-sit 'regions) :key #'id))
	 (old-b-desc
	   (find (constraint-b c) (slot-value old-sit 'regions) :key #'id))
	 ;; while finding constraints, we're interested in ordering A regions,
	 ;; but right now we're interested in constructing a B region, so we
	 ;; must reverse the constraint
	 (order-by (constraint-order-by (reverse-constraint c)))
	 (edge-name (first order-by))
	 (d (edge-displacement old-b-desc order-by old-a-desc)))
    (values
      (+ d (edge-coord new-a-desc edge-name))
      old-a-desc)))

(defun find-constraints-between-edges (sit target-region-desc dim a-side b-side)
  "Find the highest-ranked relation(s) for which we can find some A-regions
   whose a-side is related to the target (B) region's b-side along dim, and add
   them to sit."
  (with-slots (target-region regions relations constraints) sit
    (loop with side-pair = (list a-side b-side)
	  with prev-rank = 0
	  with found = nil ; list of (constraint relation) pairs
	  with min-distance = nil
	  for reln-type in +int-relns+
	  for rank = (int-reln-constrained-rank reln-type)
	  when (member side-pair (int-reln-side-pairs-constrained reln-type)
		       :test #'equalp)
	  do
	     ;; finish early if we already found some results and we finished a
	     ;; rank
	     (when (and found (> rank prev-rank))
	       (loop-finish))
	     (setf prev-rank rank)
	     ;; find related A-regions with this relationship, ordered so that
	     ;; the a-sides closest to b-side are first
	     (let* ((generic-constraint ; no A region yet
		      (make-constraint :dimension (car dim)
				       :a-side a-side
				       :b target-region
				       :b-side b-side))
		    (order-by (constraint-order-by generic-constraint))
		    (args (list
		      ;; constrained dimension
		      (third dim) (int-reln-name reln-type)
		      ;; unconstrained dimension (just make sure it overlaps)
		      (third (other-dimension dim))
		        +all-overlapping-int-reln-names+
		      :region target-region
		      :order-by order-by
		      :soft-limit 1
		      ))
		    (related-regions (find-related-regions args))
		    )
	       (when related-regions
		 ;; keep the (constraint relation) pairs with the smallest
		 ;; distance between the relevant edges of A and B (and
		 ;; temporarily put the whole region desc in A instead of just
		 ;; the ID)
		 (let ((dist (edge-distance
				 (first related-regions)
				 order-by
				 target-region-desc))
		       (new-found
		         (loop for a in related-regions
			       for c = (copy-structure generic-constraint)
			       do (setf (constraint-a c) a)
			       collect
			         (list c
				       `(relation
				         :a ,(id a) 
				         ,(third dim) ,(int-reln-name reln-type)
					 :b ,target-region))))
		       )
		   (cond
		     ((or (null min-distance) (< dist min-distance))
		       (setf found new-found
			     min-distance dist))
		     ((= dist min-distance)
		       (setf found (append found new-found)))
		     ))
		   ))
	     ; TODO apply unconstrained-rank somehow (find-related-regions for each rank, with that rank's options in args?)
	     ; TODO prefer more overlap in unconstrained direction, i.e. #'> :key (λab (overlap-size a (other-dimension dim) b))
	  finally
	    ;; replace regions with their IDs in constraints, and store region
	    ;; descs in regions and relation descs in relations
	    (loop for (c reln) in found do
	      (with-slots (a) c
	        (add-region-to-situation sit a :with-relation reln)
		(setf a (id a))
		(push c constraints)
		))
	  )))

(defun find-constraints (sit)
  (with-slots (target-region regions relations constraints) sit
    (let ((target-region-desc (find target-region regions :key #'id)))
      ; TODO also add constraint for nearest assoc-with match if we have any,
      ; replacing any similar non-assoc-with constraint

      ;; for each edge of the target region (B) paired with an edge of the
      ;; found region (A) in the same dimension
      (dolist (dim +dimensions+)
        (dolist (b-side '(min max))
	  (dolist (a-side '(min max))
	    (find-constraints-between-edges sit target-region-desc
		dim a-side b-side)))))
    (format t "Found constraints:~%  ~s~%with relations:~%  ~s~%among regions:~%  ~s~%" constraints relations regions)
    ))

