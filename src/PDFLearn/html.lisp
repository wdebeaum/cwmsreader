(in-package :pdflearn)

;;;; quick&dirty parser for HTML tables saved by PDFExtractor
;;;; NOTE: will not work for HTML in general, or even for HTML tables not saved
;;;; by PDFExtractor

(defstruct table-cell
  row
  col
  (attributes nil)
  (content nil)
  )

(defun parse-alpha-integer (str)
  "Parse an integer written in base 26 using letters A-Z instead of digits."
  (parse-integer
      ;; convert str to use 0-9A-P instead of A-JK-Z (assumes ASCII/UTF-8)
      (map 'string
        (lambda (c)
	  (cond
	    ((char<= #\A c #\J)
	      (code-char (+ (- (char-code c) (char-code #\A)) (char-code #\0))))
	    (t
	      (code-char (+ (- (char-code c) (char-code #\K)) (char-code #\A))))
	    ))
	str)
      :radix 26))

(defun parse-spreadsheet-range (str)
  "Turn e.g. \"B3-D9\" into (values 2 1 8 3)."
  (let* ((first-row-pos (position-if #'digit-char-p str))
	 (last-col-pos (position-if #'alpha-char-p str :start first-row-pos))
	 (last-row-pos (position-if #'digit-char-p str :start last-col-pos))
	 (first-col (parse-alpha-integer (subseq str 0 first-row-pos)))
	 (first-row
	   (1- (parse-integer (subseq str first-row-pos (1- last-col-pos)))))
	 (last-col
	   (parse-alpha-integer (subseq str last-col-pos last-row-pos)))
	 (last-row (1- (parse-integer (subseq str last-row-pos))))
	 )
    (values first-row first-col last-row last-col)))

(defun table-cell-heading-for (cell)
  "Return the range of cell indices that the given cell is a heading for, or
   nil if it's not a heading."
  (let ((title (find-arg (table-cell-attributes cell) :title)))
    (when title
      (let ((heading-for-pos (search " heading for " title)))
        (when heading-for-pos
	  (parse-spreadsheet-range (subseq title (+ heading-for-pos 13))))))))

(defun table-cell-row-heading-p (cell)
  (let ((title (find-arg (table-cell-attributes cell) :title)))
    (when title
      (search "row heading for " title))))

(defun table-cell-column-heading-p (cell)
  (let ((title (find-arg (table-cell-attributes cell) :title)))
    (when title
      (search "column heading for " title))))

(defun scan-characters (f keep-scanning-p)
  "Read one character at a time from stream f, until calling keep-scanning-p on
   the character returns nil (or until EOF). Return a string of the scanned
   characters."
  (loop for c = (read-char f nil)
        while (and c (funcall keep-scanning-p c))
	collect c into cs
	finally (return (funcall #'concatenate 'string cs))))

(defun scan-substrings (f keep-discard-or-finish-fn)
  "Read one character at a time from stream f, appending the character to a
   string of kept characters, and calling the keep-discard-or-finish-fn on the
   string. If the function returns :keep, we keep the new character in the
   string and keep going. If the function returns :discard, we reset the string
   to empty and keep going. If the function returns :finish (or EOF is
   reached), we return the string."
  (loop with str = ""
        for c = (read-char f nil) while c
	do (setf str (concatenate 'string str (list c)))
	   ;(format t "~a~%" str)
	   (ecase (funcall keep-discard-or-finish-fn str)
	     (:keep nil)
	     (:discard (setf str ""))
	     (:finish (return str))
	     )
	finally (return str)
	))

(defun string-begins-p (str prefix)
  "Does the string str begin with the substring prefix?"
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))

(defun unescape-entity (amp-str)
  "Given an HTML entity like \"&amp;\", return what it represents, like \"&\"."
  (cond ; note: ecase won't work because strings aren't necessarily eql
    ((string= amp-str "&amp;") "&")
    ((string= amp-str "&lt;") "<")
    ((string= amp-str "&gt;") ">")
    ((string= amp-str "&quot;") "\"")
    (t (error "unknown HTML entity ~s" amp-str))
    ))

(defun text-content (html-str)
  "Return the text content of the given HTML string, with tags removed and
   entities unescaped."
  ;; for each substring of plain text
  (loop with start = 0
	for end =
	  (position-if (lambda (c) (member c '(#\& #\<))) html-str :start start)
	while end
	append
	  (list
	    (subseq html-str start end) ; save that substring...
	    ;; ...and the interpretation of the following entity or tag, while
	    ;; setting start to be after that entity or tag
	    (ecase (char html-str end)
	      (#\&
		(setf start (1+ (position #\; html-str :start (1+ end))))
		(unescape-entity (subseq html-str end start))
		)
	      (#\<
		(setf start (1+ (position #\> html-str :start (1+ end))))
		"" ; TODO? make this a space, depending on which tag we saw
		)
	      )
	    )
	into text-strs
	;; finally concatenate all the stuff we saved, along with the plain
	;; text substring after the last entity or tag
	finally (return (format nil "~{~a~}~a"
				text-strs
				(subseq html-str start)))
	))

(defvar *current-row-index* -1)
(defvar *current-col-index* -1)
(defvar *rowspans* nil)
(defvar *current-cell* nil)
(defvar *cells* nil)

(defun next-row ()
  (incf *current-row-index*)
  (setf *current-col-index* -1)
  (setf *rowspans* (mapcar #'1- *rowspans*))
  ;(format t "row ~s has rowspans ~s~%" *current-row-index* *rowspans*)
  )

(defun next-col ()
  (incf *current-col-index*)
  ;; further increment col index while there are previous cells rowspanning
  ;; over the current one
  (loop for rs = (nth *current-col-index* *rowspans*)
        until (or (null rs) (<= rs 0))
	do (incf *current-col-index*))
  (setf *current-cell*
        (make-table-cell :row *current-row-index* :col *current-col-index*))
  (push *current-cell* *cells*)
  ;(format t "col ~s~%" *current-col-index*)
  )

(defun update-rowspans ()
  "Update *rowspans* according to the rowspan/colspan attributes of the
   *current-cell*."
  (let* ((attrs (table-cell-attributes *current-cell*))
	 (rowspan (parse-integer (or (find-arg attrs :rowspan) "1")))
	 (colspan (parse-integer (or (find-arg attrs :colspan) "1")))
	 (last-col (1- (+ *current-col-index* colspan))))
    (when (and (> rowspan 0) (> colspan 0)
	       (or (> rowspan 1) (> colspan 1)))
      ;(format t "row ~s col ~s has rowspan=~s colspan=~s last-col=~s~%" *current-row-index* *current-col-index* rowspan colspan last-col)
      (when (>= last-col (length *rowspans*))
	(setf *rowspans*
	      (nconc *rowspans*
		     (make-list (- (1+ last-col) (length *rowspans*))
				:initial-element 0))))
      (loop for col from *current-col-index* upto last-col
	    do (setf (nth col *rowspans*) rowspan))
      )))

(defun read-attribute-name (f)
  (let ((attr-name (scan-characters f (lambda (c) (not (char= c #\=))))))
    (push (intern (string-upcase attr-name) :keyword)
	  (table-cell-attributes *current-cell*))))

(defun read-attribute-value (f)
  (unless (char= #\" (read-char f))
    (error "expected \" after = in attribute"))
  (push (text-content (scan-characters f (lambda (c) (not (char= c #\")))))
        (table-cell-attributes *current-cell*))
  )

(declaim (ftype (function (stream) null) read-more-attributes))

(defun read-cell-attributes (f)
  (read-attribute-name f)
  (read-attribute-value f)
  (read-more-attributes f)
  )

(defun read-more-attributes (f)
  (ecase (read-char f)
    (#\Space (read-cell-attributes f))
    (#\> nil)
    ))

(defun read-cell-content (f)
  (let (content)
    (scan-substrings f
      (lambda (str)
        (cond
	  ((string= "</t" str) :finish)
	  ((string-begins-p "</t" str) :keep)
	  (t
	    (push str content)
	    :discard)
	  )))
    (setf (table-cell-content *current-cell*)
          (text-content (apply #'concatenate 'string (nreverse content))))
    ))

(defun read-after-<th-or-<td (f)
  (case (read-char f)
    (#\Space
      (next-col)
      (read-cell-attributes f)
      (setf (table-cell-attributes *current-cell*)
            (nreverse (table-cell-attributes *current-cell*))) ; undo push rev.
      (update-rowspans)
      (read-cell-content f))
    (#\>
      (next-col)
      (read-cell-content f))
    (otherwise ; false alarm such as <thead>
      nil)
    ))

(defun read-after-<t (f)
  (case (read-char f)
    (#\r ; row
      (next-row))
    ((#\h #\d) ; heading or data cell
      (read-after-<th-or-<td f))
    (otherwise ; false alarm such as <title> or <tbody>
      )
    ))

(defun read-until-<t (f)
  (scan-substrings f
    (lambda (str)
      (cond
	((string= "<t" str) :finish)
	((string-begins-p "<t" str) :keep)
	(t :discard)
	))))

(defun postprocess-cells ()
  (setf *cells* (nreverse *cells*)) ; undo push reversal
  )

(defun read-html-table-from-pdf-extractor (filename)
  (with-open-file (f filename :direction :input)
    (setf *current-row-index* -1
	  *current-col-index* -1
	  *rowspans* nil
	  *current-cell* nil
	  *cells* nil)
    (loop until (string= "" (read-until-<t f))
          do (read-after-<t f)
	  )
    )
  (postprocess-cells)
  *cells*)

