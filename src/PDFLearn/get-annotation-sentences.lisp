;;;; get-annotation-sentences.lisp - get one annotation sentence per line from an HTML table file saved by PDFExtractor
;;;; USAGE:
;;;; sbcl --load get-annotation-sentences.lisp /path/to/input/table.html /path/to/output/annotation-sentences.txt

;; load html.lisp the quick&dirty way
(defpackage :pdflearn (:use :common-lisp))
(in-package :pdflearn)
(defun find-arg (l k) (second (member k l)))
(load "html.lisp")

(defun heading-var (cell)
  (cond
    ((table-cell-column-heading-p cell) "[column heading]")
    ((table-cell-row-heading-p cell) "[row heading]")
    (t nil)
    ))

(defun replace-in-title (cell var val)
  "Replace occurrences of var with val in cell's title attribute, if any."
  (let ((title (find-arg (table-cell-attributes cell) :title)))
    (when title
      (loop with start = 0
            for pos = (search var title :start2 start)
	    while pos
	    do (setf title (concatenate 'string
			       (subseq title 0 pos)
			       val
			       (subseq title (+ pos (length var)))))
	    finally (setf (second (member :title (table-cell-attributes cell)))
			  title)
	    ))))

(defun get-annotation-sentences (input-table-file output-text-file)
  (read-html-table-from-pdf-extractor input-table-file)
  ;; replace [cell]
  (dolist (cell *cells*)
    (unless (heading-var cell)
      (replace-in-title cell "[cell]" (table-cell-content cell))))
  ;; replace [row heading] and [column heading]
  (loop for heading in *cells*
        for var = (heading-var heading)
	when var ; when it is a heading
	do (multiple-value-bind (first-row first-col last-row last-col)
	       (table-cell-heading-for heading)
	     (dolist (cell *cells*)
	       (unless (heading-var cell)
	         (with-slots (row col) cell
	           (when (and (<= first-row row last-row)
		              (<= first-col col last-col))
		     (replace-in-title cell var (table-cell-content heading))
		     )))))
	)
  ;; output one title per line
  (with-open-file (o output-text-file :direction :output :if-exists :supersede)
    (dolist (cell *cells*)
      (let ((title (find-arg (table-cell-attributes cell) :title)))
	(when title
	  (unless (heading-var cell)
	    (format o "~a~%" (substitute #\Space #\Newline title)))))))
  )

(if (= 3 (length sb-ext:*posix-argv*))
  (get-annotation-sentences (second sb-ext:*posix-argv*) (third sb-ext:*posix-argv*))
  (format *error-output* "USAGE: sbcl --load get-annotation-sentences.lisp /path/to/input/table.html /path/to/output/annotation-sentences.txt~%")
  )
(sb-ext:quit)
