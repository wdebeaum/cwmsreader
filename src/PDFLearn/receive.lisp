(in-package :pdflearn)
(in-component :pdflearn)

(defvar *define-service-messages* nil)

(defun component-name ()
  (slot-value *component* 'dfc::name))

(defmacro defservice (name handler &key input output)
  `(progn
    (defcomponent-handler
      '(request &key :content (,name . *))
      ,handler
      :subscribe t)
    (push `(tell :content (define-service
	    :name ,',name
	    :component ,(component-name)
	    :input ,',input
	    :output ,',output
	    ))
          *define-service-messages*)
    ))

;;; learn.lisp

(defservice learn-to-find #'handle-learn-to-find
  :input (
    (input
      :name target-region
      :gloss "ID of the rectangular region from PDFExtractor to learn how to find"
      :id-code region
      :format ont::symbol
      :requirements :required
      )
    (input
      :name table-file
      :gloss "HTML table saved by PDFExtractor for the table extracted from the target region"
      :id-code file
      :format ont::list
      :requirements :optional
      )
    (input
      :name assoc-with
      :gloss "Text associated with the target region"
      :id-code text
      :format ont::string
      :requirements :optional
      )
    )
  :output (
    (output
      :name rule
      :gloss "ID for the rule that was learned"
      :id-code rule
      :format ont::symbol
      )
    )
  )

;;; find.lisp

(defservice find #'handle-find
  :input (
    (input
      :name rule
      :gloss "ID for the learned rule to use"
      :id-code rule
      :format ont::symbol
      :requirements :required
      )
    (input
      :name page
      :gloss "ID of the page to find a region on"
      :id-code page
      :format ont::symbol
      :requirements :required
      )
    (input
      :name assoc-with
      :gloss "Text associated with the target region"
      :id-code text
      :format ont::string
      :requirements :optional
      )
    )
  :output (
    (output
      :name target-region
      :gloss "ID of the rectangular region from PDFExtractor that was found"
      :id-code region
      :format ont::symbol
      )
    )
  )

(defservice get-relevant-find-rules #'handle-get-relevant-find-rules
  :input (
    (input
      :name page
      :gloss "ID of the page to find a region on"
      :id-code page
      :format ont::symbol
      :requirements :required
      )
    (input
      :name limit
      :gloss "maximum number of rules to get"
      :id-code limit
      :format ont::number
      :requirements :optional
      )
    (input
      :name soft-limit
      :gloss "approximate maximum number of rules to get"
      :id-code soft-limit
      :format ont::number
      :requirements :optional
      )
    )
  :output (
    (output
      :name rules
      :gloss "list of rule IDs paired with irrelevance scores, most relevant first"
      :id-code rules
      :format ont::list
      )
    )
  )

;;; edit.lisp

(defservice learn-to-edit #'handle-learn-to-edit
  :input (
    (input
      :name table
      :gloss "ID of the already-edited table from PDFExtractor"
      :id-code table
      :format ont::symbol
      :requirements :required
      )
    )
  :output (
    (output
      :name rule
      :gloss "ID for the rule that was learned"
      :id-code rule
      :format ont::symbol
      )
    )
  )

(defservice edit #'handle-edit
  :input (
    (input
      :name table
      :gloss "ID of the table from PDFExtractor to be edited"
      :id-code table
      :format ont::symbol
      :requirements :required
      )
    (input
      :name rule
      :gloss "ID for the learned rule to use"
      :id-code rule
      :format ont::symbol
      :requirements :required
      )
    )
  :output (
    (output
      :name table
      :gloss "description of the table after editing"
      :id-code table
      :format ont::list
      )
    (output
      :name edits
      :gloss "list of edits that were made to the table"
      :id-code edits
      :format ont::list
      )
    )
  )

;;; parse-table.lisp

(defservice extract-table-cells #'handle-extract-table-cells
  :input (
    (input
      :name file
      :gloss "HTML table saved by PDFExtractor, whose cells are to be extracted from"
      :id-code file
      :format ont::list
      :requirements :required
      )
    )
  :output nil ; doesn't reply
  )

(defcomponent-handler
  '(tell &key :content (new-speech-act *))
  #'handle-new-speech-act
  :subscribe t)

;;; for LearningGUI

(defun handle-list-rules (msg args)
    (declare (ignore args))
  (loop for id being the hash-keys of *rules* using (hash-value rule)
        collect
	  (etypecase rule
	    (situation
	      `(find-rule
	          :id ,id
		  ,@(when (situation-assoc-with rule)
		      `(:assoc-with ,(situation-assoc-with rule)))
		  ))
	    (table-edit-rule
	      `(edit-rule
		  :id ,id
		  :origin-found-with ,(table-edit-rule-origin-found-with rule)))
	    )
	into rules
	finally (reply-to-msg msg 'reply :content
		  `(report :content (answer :rules ,rules)))
	))

(defservice list-rules #'handle-list-rules
  :input nil
  :output (
    (output
      :name rules
      :gloss "list of structures summarizing each rule"
      :id-code rules
      :format ont::list
      )
    )
  )

;;; cwc module stuff (see ../util/cwc/StandardCWCModule.java)

(defvar *already-declared-capabilities* nil)

(defun declare-capabilities ()
  (dolist (msg *define-service-messages*)
    (send-msg msg))
  (setf *already-declared-capabilities* t))

(defun declare-capabilities-once (msg)
    (declare (ignore msg))
  (unless *already-declared-capabilities*
    (declare-capabilities)))

(defcomponent-handler
  '(tell &key :content (i-am-here &key :who cwmsagent))
  #'declare-capabilities-once
  :subscribe t)

(defun handle-restart (msg args)
    (declare (ignore msg args))
  (clear-cache)
  (setf *already-declared-capabilities* nil)
  (send-msg '(request :content (are-you-there :who cwmsagent)))
  )

(defcomponent-handler
  '(request &key :content (restart . *))
  #'handle-restart
  :subscribe t)

(defcomponent-cancellation-pattern
  '(request &key :content (restart . *)))

