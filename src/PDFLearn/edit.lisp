(in-package :pdflearn)

(defstruct table-edit-rule
  ;; list of N PDFExtractor edit structures, with coordinates abstracted
  (edits nil)
  ;; list of N+1 (num-rows num-columns) pairs: before, after, and between edits
  sizes
  ;; region-finding rule ID for finding the table's origin
  origin-found-with
  ; TODO? also save headings
  )

(defun abstract-coord (concrete-coord size &optional (min 0))
  "Return an abstracted version of coord; that is, if it's close to size,
   return a number that adding size to will give back the original coord,
   otherwise return the original coord. Optionally subtract min first.
   Abstracted coordinates work like array indices in Perl; first is 0, second
   is 1, last is -1, second to last is -2, etc."
  (decf concrete-coord min)
  (if (> concrete-coord (/ size 2))
    (- concrete-coord size)
    concrete-coord))

(defun concrete-coord (abstract-coord size &optional (min 0))
  "Inverse of abstract-coord."
  (+
    (if (< abstract-coord 0)
      (+ abstract-coord size)
      abstract-coord)
    min))

(defun abstract-edit (concrete-edit size-after origin)
  "Return two values: a new version of edit with abstracted coordinates, and
   the size of the table (a (rows cols) pair) before the edit."
  (destructuring-bind (verb &key
			;; all possible edit arguments
			first last
			row column
			rows columns
			first-row
			first-column
			last-row
			last-column
			at-x
			others
			content
			annotations
			type
			heading-for
		      ) concrete-edit
      (declare (ignore others rows columns))
    (let* ((num-rows-after (first size-after))
	   (num-cols-after (second size-after))
	   (num-rows-before num-rows-after)
	   (num-cols-before num-cols-after))
      (values
	(ecase verb
	  (delete-rows
	    (incf num-rows-before (1+ (- last first)))
	    `(delete-rows
	      :first ,(abstract-coord first num-rows-before)
	      :last ,(abstract-coord last num-rows-before)
	      ))
	  (delete-columns
	    (incf num-cols-before (1+ (- last first)))
	    `(delete-columns
	      :first ,(abstract-coord first num-cols-before)
	      :last ,(abstract-coord last num-cols-before)
	      ))
	  (merge-rows
	    (incf num-rows-before (- last first))
	    `(merge-rows
	      :first ,(abstract-coord first num-rows-before)
	      :last ,(abstract-coord last num-rows-before)
	      ))
	  (merge-columns
	    (incf num-cols-before (- last first))
	    `(merge-columns
	      :first ,(abstract-coord first num-cols-before)
	      :last ,(abstract-coord last num-cols-before)
	      ))
	  (merge-cells ; size unaffected
	    `(merge-cells
	      :first-row ,(abstract-coord first-row num-rows-before)
	      :first-column ,(abstract-coord first-column num-cols-before)
	      :last-row ,(abstract-coord last-row num-rows-before)
	      :last-column ,(abstract-coord last-column num-cols-before)
	      ))
	  (split-column
	    (decf num-cols-before)
	    `(split-column
	      :at-x ,(abstract-coord at-x (width origin) (min-x origin))
	      ))
	  (edit-cell ; size unaffected
	    `(edit-cell
	      :row ,(abstract-coord row num-rows-before)
	      :column ,(abstract-coord column num-cols-before)
	      ,@(when content (list :content content))
	      ,@(when annotations (list :annotations annotations))
	      ,@(when type (list :type type))
	      ,@(when heading-for (list :heading-for heading-for))
	      ))
	  (edit-cells ; size unaffected
	    `(edit-cells
	      :first-row ,(abstract-coord first-row num-rows-before)
	      :first-column ,(abstract-coord first-column num-cols-before)
	      :last-row ,(abstract-coord last-row num-rows-before)
	      :last-column ,(abstract-coord last-column num-cols-before)
	      ,@(when annotations (list :annotations annotations))
	      ,@(when type (list :type type))
	      ))
	  ((merge-tables select-and-reorder-rows select-and-reorder-columns)
	    (error "PDFLearn is not able to learn table edits of type ~s" verb))
	  )
	(list num-rows-before num-cols-before)
	))))

(defun concrete-edit (abstract-edit size-before origin)
  "Inverse of abstract-edit."
  (destructuring-bind (verb &key
			;; all possible edit arguments
			first last
			row column
			first-row
			first-column
			last-row
			last-column
			at-x
			content
			annotations
			type
			heading-for
		      ) abstract-edit
    (let* ((num-rows-before (first size-before))
	   (num-cols-before (second size-before))
	   (num-rows-after num-rows-before)
	   (num-cols-after num-cols-before))
      (values
	(ecase verb
	  (delete-rows
	    (let ((conc-first (concrete-coord first num-rows-before))
		  (conc-last (concrete-coord last num-rows-before)))
	      (decf num-rows-after (1+ (- conc-last conc-first)))
	      `(delete-rows :first ,conc-first :last ,conc-last)))
	  (delete-columns
	    (let ((conc-first (concrete-coord first num-cols-before))
		  (conc-last (concrete-coord last num-cols-before)))
	      (decf num-cols-after (1+ (- conc-last conc-first)))
	      `(delete-columns :first ,conc-first :last ,conc-last)))
	  (merge-rows
	    (let ((conc-first (concrete-coord first num-rows-before))
		  (conc-last (concrete-coord last num-rows-before)))
	      (decf num-rows-after (- conc-last conc-first))
	      `(merge-rows :first ,conc-first :last ,conc-last)))
	  (merge-columns
	    (let ((conc-first (concrete-coord first num-cols-before))
		  (conc-last (concrete-coord last num-cols-before)))
	      (decf num-cols-after (- conc-last conc-first))
	      `(merge-columns :first ,conc-first :last ,conc-last)))
	  (merge-cells ; size unaffected
	    `(merge-cells
	      :first-row ,(concrete-coord first-row num-rows-before)
	      :first-column ,(concrete-coord first-column num-cols-before)
	      :last-row ,(concrete-coord last-row num-rows-before)
	      :last-column ,(concrete-coord last-column num-cols-before)
	      ))
	  (split-column
	    (incf num-cols-after)
	    `(split-column
	      :at-x ,(concrete-coord at-x (width origin) (min-x origin))
	      ))
	  (edit-cell ; size unaffected
	    `(edit-cell
	      :row ,(concrete-coord row num-rows-before)
	      :column ,(concrete-coord column num-cols-before)
	      ,@(when content (list :content content))
	      ,@(when annotations (list :annotations annotations))
	      ,@(when type (list :type type))
	      ,@(when heading-for (list :heading-for heading-for))
	      ))
	  (edit-cells ; size unaffected
	    `(edit-cells
	      :first-row ,(concrete-coord first-row num-rows-before)
	      :first-column ,(concrete-coord first-column num-cols-before)
	      :last-row ,(concrete-coord last-row num-rows-before)
	      :last-column ,(concrete-coord last-column num-cols-before)
	      ,@(when annotations (list :annotations annotations))
	      ,@(when type (list :type type))
	      ))
	  ; NOTE: we never see unlearnable edits here
	  )
	(list num-rows-after num-cols-after)
	))))

(defun handle-learn-to-edit (msg args)
  (destructuring-bind (&key table) args
    ;; get edit history, current table dimensions, and original region from
    ;; PDFExtractor
    (destructuring-bind (_ &key origin edits num-rows num-columns)
	(find-arg-in-act (request-pdfextractor `(get-history :of ,table))
			 :content)
	(declare (ignore _))
      ;; make a new rule
      (let ((rule-id (gentemp "RULE"))
            (rule (make-table-edit-rule
		      :sizes `((,num-rows ,num-columns))
		      :origin-found-with (gethash (id origin) *region-to-rule*)
		      )))
	;; work backwards through edit history to figure out dimensions at each
	;; step, and abstract cell coordinates in each edit
	;; TODO ignore automatically-added delete-columns edits for blank columns (might have to detect this during execution instead of learning?)
        (loop with size = (car (table-edit-rule-sizes rule))
	      for edit in (reverse edits)
	      do (multiple-value-bind (abs-edit prev-size)
		     (abstract-edit edit size origin)
		   (push abs-edit (table-edit-rule-edits rule))
		   (push prev-size (table-edit-rule-sizes rule))
		   )
	      )
	;; save the rule
	(add-rule rule-id rule)
	(format t "learned:~%~s~%" rule)
	;; reply with new rule id
	(reply-to-msg msg 'reply :content
	  `(report :content (answer :rule ,rule-id)))
	))))

(defun handle-edit (msg args)
  (destructuring-bind (&key ((:rule rule-id)) table) args
    (destructuring-bind (_ &key origin edits num-rows num-columns)
	(find-arg-in-act (request-pdfextractor `(get-history :of ,table))
			 :content)
	(declare (ignore _ edits))
      (let ((rule (gethash rule-id *rules*)))
        (unless rule
	  (error "unknown rule: ~s~%" rule-id))
        (unless (typep rule 'table-edit-rule)
	  (error "wrong type of rule"))
	(let* ((abstract-edits (table-edit-rule-edits rule))
	       (expected-size (car (last (table-edit-rule-sizes rule))))
	       (actual-size (list num-rows num-columns))
	       concrete-edits
	       table-desc)
	  ;; turn abstract edits into concrete edits, tracking size as we go
	  (dolist (ae abstract-edits)
	    (multiple-value-bind (ce size-after)
	        (concrete-edit ae actual-size origin)
	      (push ce concrete-edits)
	      (setf actual-size size-after)
	      ))
	  (setf concrete-edits (nreverse concrete-edits)) ; undo push reversal
	  ;; make sure we would end up with the right number of columns
	  (unless (= (second expected-size) (second actual-size))
	    ; TODO? try to adapt the edits when this happens instead of just failing
	    (error "applying these edits would result in a table with ~s columns, but ~s columns expected:~%  (~{~s~%   ~})~%" (second actual-size) (second expected-size) concrete-edits))
	  ;; actually do the edits, keeping the last table description we get
	  ;; from PDFExtractor
	  (dolist (ce concrete-edits)
	    ; TODO? propagate failures
	    (setf table-desc
		  (unwrap-answer
		    (request-pdfextractor `(edit-table :table ,table :edit ,ce))
		    :table)))
	  ;; answer with the final table description, and the concrete edits we
	  ;; made
	  (reply-to-msg msg 'reply :content `(report :content (answer
	    :table ,table-desc
	    :edits ,concrete-edits
	    )))
	  )))))

