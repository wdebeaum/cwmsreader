(in-package :pdflearn)

(defstruct heading
  text
  first-row
  first-col
  last-row
  last-col
  lf)

(defun make-all-headings (cells)
  (mapcan
    (lambda (cell)
      (multiple-value-bind (first-row first-col last-row last-col)
	  (table-cell-heading-for cell)
	(when first-row ; the cell is a heading
	  (let ((lf (send-utterance-and-wait
		      (table-cell-content cell) :channel 'heading)))
	    (list (make-heading
		    :text (table-cell-content cell)
		    :first-row first-row
		    :first-col first-col
		    :last-row last-row
		    :last-col last-col
		    :lf lf))))))
    cells))

(defun heading-for-cell-p (heading cell)
  "Does the given heading apply to the given cell?"
  (with-slots (first-row first-col last-row last-col) heading
    (with-slots (row col) cell
      (and (<= first-row row last-row) (<= first-col col last-col)))))

(defun lf-is-location-p (lf)
  "Does the main content term of the given LF have a type that is a non-strict
   subtype of ONT::location?"
  (om::sublf (lf-content-type lf) 'ont::location om::*lf-ontology*))

(defun make-location-term (figure ground)
  (let ((id (gentemp "V" :ont)))
    (values `(term :lf (ont::f ,id (:* ont::in-loc w::in)
			       :figure ,figure :ground ,ground)
                   :var ,id
		   ; TODO? :sem :input :start :end
		   )
	    id)))

(defvar +number-junk-chars+ '(#\, #\Space #\Tab #\Newline))
(defun number-junk-char-p (c) (member c +number-junk-chars+ :test #'char=))
(defun number-char-p (c) (or (number-junk-char-p c) (digit-char-p c)))
(defun parse-number (str) (parse-integer (remove-if #'number-junk-char-p str)))

(defun make-number-term (value)
  (let ((id (gentemp "V" :ont)))
    (values `(term :lf (ont::a ,id ont::number :value ,value)
		   :var ,id
		   ; TODO? :sem :input :start :end
		   )
	    id)))

(defun make-lf-for-cell (cell headings)
  "replace ID of each LF term in each heading, and construct one big LF using the first heading as the root, any location-typed headings as IN-LOCs, and the numeric value of the cell contents as :SIZE"
  (incf *uttnum*)
  (let* ((cell-text (table-cell-content cell))
         (heading-lfs (mapcar (lambda (h) (copy-lf (heading-lf h))) headings))
         (cell-lf (pop heading-lfs)))
    (multiple-value-bind (cell-terms cell-content-id)
	(lf-content-terms cell-lf)
      (let ((cell-content-term (find-term cell-content-id cell-terms)))
	(dolist (heading-lf heading-lfs)
	  (when (lf-is-location-p heading-lf)
	    (format t "~&  yes, it is a location~%")
	    (multiple-value-bind (heading-terms heading-content-id)
		(lf-content-terms heading-lf)
	      ;; make a new location term
	      (multiple-value-bind (loc-term loc-id)
		  (make-location-term cell-content-id heading-content-id)
		;; add loc-term and heading-terms to cell-terms
	        (add-terms-to-lf cell-lf (cons loc-term heading-terms))
		;; point to loc-term from cell-content-term
		(add-args-to-term cell-content-term `(:location ,loc-id))
		))))
	;; if the cell-text represents a number, get that number and put it in
	;; the :size argument of the cell-content-term
	(when (every #'number-char-p cell-text)
	  (let ((value (parse-number cell-text)))
	    (multiple-value-bind (size-term size-id)
	        (make-number-term value)
	      (add-terms-to-lf cell-lf (list size-term))
	      (add-args-to-term cell-content-term `(:size ,size-id))
	      )))
	))
    cell-lf))

(defun handle-extract-table-cells (msg args)
    (declare (ignore msg))
  (destructuring-bind (&key file) args
    (destructuring-bind (_ &key name format) file
        (declare (ignore _))
      (unless (equalp format "text/html")
        (error "PDFLearn can only extract table cells from HTML tables"))
      (read-html-table-from-pdf-extractor name) ; set *cells*
      (loop with headings = (make-all-headings *cells*) ; make heading structs and send text through TextTagger to Parser on :channel heading
	    for cell in *cells*
	    for cell-headings =
	      (remove-if-not (lambda (h) (heading-for-cell-p h cell)) headings)
	    when cell-headings
	      do (send-lf-to-im (make-lf-for-cell cell cell-headings)))
      )))

(defun handle-new-speech-act (msg lf)
    (declare (ignore msg))
  (when (eq 'heading (lf-channel lf))
    (warn "got unexpected new-speech-act on :channel heading")))

