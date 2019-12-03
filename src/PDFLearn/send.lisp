(in-package :pdflearn)

;;; sending requests to PDFExtractor

(defvar *cache* (make-hash-table :test #'equalp)
  "Map the content of previous requests to PDFExtractor to the content of the
   corresponding replies, and map text strings to the LFs resulting from parsing
   them.")

(defun clear-cache ()
  (setf *cache* (make-hash-table :test #'equalp)))

(defun request-pdfextractor (request-content)
  (copy-tree
    (let ((cached (gethash request-content *cache*)))
      (if cached
	cached
	(setf (gethash request-content *cache*)
	      (send-and-wait
		`(request :receiver pdfextractor :content ,request-content)))
	))))

(defun unwrap-answer (reply-content kw)
  "Given a reply content of the form:
    `(report :content (answer ,kw ,foo))
   return foo."
  (find-arg-in-act (find-arg-in-act reply-content :content) kw))

(defun detect-reference-regions (page-id)
  ;; make sure PDFExtractor has paragraph regions for this page; ditto rulings
  (loop for request-content in `(
	  (detect-paragraph-regions :page ,page-id)
	  (detect-ruling-regions :page ,page-id)
	  )
        for regions =
	  (unwrap-answer
	    (send-and-wait
	      `(request :receiver pdfextractor :content ,request-content))
	    :regions)
	do
	  ;; cache the regions as if they were replies to describe requests for
	  ;; their ids
	  (dolist (region regions)
	    (setf (gethash `(describe :what ,(id region)) *cache*) region))
	))

(defun get-pdf-extractor-description (what)
  (unwrap-answer
    (request-pdfextractor `(describe :what ,what))
    :description))

(defun relate-regions (a b)
  "Return a (relation ...) structure relating regions with IDs a and b."
  (cons 'relation (cdr ; rplaca, but don't mess up cache
    (find-arg-in-act
      (request-pdfextractor `(relate-regions :a ,a :b ,b))
      :content))))

(defun find-related-regions (args)
  (unwrap-answer
    (request-pdfextractor `(find-related-regions ,@args))
    :regions))

(defun region-contains-region-p (container-id content-id)
  "Does the given container region contain the given content region?"
  (destructuring-bind (_ &key vertically horizontally &allow-other-keys)
		      (relate-regions content-id container-id)
      (declare (ignore _))
    (and (member vertically   '(inside starts finishes equal))
         (member horizontally '(inside starts finishes equal)))))

(defun find-most-similar-regions (region-desc page-id)
  "Find the regions on the given page that are most similar to the given region
   description (by coordinates, not :id and :page)."
  (let ((region-arg
	  (insert-arg-in-act
	    (remove-arg-in-act
	      (remove-arg-in-act region-desc :id)
	      :page)
	    :page page-id)))
    (unwrap-answer
      (request-pdfextractor `(find-similar-regions
				:region ,region-arg :soft-limit 1))
      :regions)))

(defun search-page (for in)
  (unwrap-answer
    (request-pdfextractor `(search :for ,for :in ,in))
    :matches))

(defun normalize-string-for-match (str)
  "Downcase str and replace each run of whitespace with a single space. NOTE:
   this is different from util::normalize-string."
  (let* ((tokens (util:split-string str
		     ;; FIXME this definition of "whitespace" isn't quite the
		     ;; same as PDFExtractor/Java's. Oh well.
		     :on '(#\Space #\Newline #\Tab)))
         (nonempty-tokens (remove "" tokens :test #'string=)))
    (format nil "~(~{~a~^ ~}~)" nonempty-tokens)))

(defun strings-match-p (a b)
  "Do strings a and b match in the same way that PDFExtractor matches strings
   when processing its search request? That is, ignoring case and extra
   whitespace."
  (string= (normalize-string-for-match a) (normalize-string-for-match b)))

(defun select-region (region-desc)
  "Select a new region in PDFExtractor and return its ID."
  ;(request-pdfextractor `(select :what ,region-desc)) ;no reply, never returns
  (send-msg `(request :receiver pdfextractor :content (select :what ,region-desc)))
  ;; HACK: PDFExtractor really ought to just return the ID to us as an answer
  ;; to the above request, but it's complicated by the fact that select
  ;; requests also work for table selections, and those don't have IDs. So
  ;; instead we just immediately request the most similar region to the one we
  ;; just made, which ought to be that region itself.
  (id (first (find-most-similar-regions
	         (remove-arg-in-act region-desc :page)
		 (find-arg-in-act region-desc :page))))
  )

(defun select-only-regions (page region-ids)
  "Select only the given regions on their page (assuming they are all on the
   same page); deselect any others on that page."
  (send-msg `(request :receiver pdfextractor :content (deselect :what (rectangles :page ,page))))
  (dolist (r region-ids)
    (send-msg `(request :receiver pdfextractor :content (select :what (rectangle :id ,r)))))
  )

;;; sending text to be parsed/LFs to be extracted from

(defun send-utterance-and-wait (text &key (channel 'desktop))
  "Like dfc::send-and-wait, but for utterance and new-speech-act instead of
   request and reply. Send the utterance, and wait for a corresponding output
   from the Parser, deferring any other messages received in the meantime. Then
   return the LF."
  (let ((cached (gethash text *cache*)))
    (when cached
      (return-from send-utterance-and-wait (copy-tree cached))))
  ; TODO? temporarily subscribe to new-speech-act? except Facilitator has only a stub for unsubscribe. also, it would be weird subscribing and unsubscribing for each utterance when we're doing a bunch in a row
  (incf *uttnum*)
  (send-msg `(tell :content (utterance :text ,text :uttnum ,*uttnum* :direction input :channel ,channel)))
  (loop for msg = (dfc::component-read-msg dfc::*component*) do
    (cond
      ((null msg)
	(throw :exit 0))
      ((not (listp msg))
	(error 'dfc::message-error :comment "message is not a list"))
      ((dfc::is-cancellation-msg msg)
	(format t "got cancellation message!~%")
	(setq dfc::*pending-messages* (list msg))
	(throw :main-loop nil))
      (t
	(multiple-value-bind (matched-p lf)
	    (dfc::match-msg-pattern '(tell &key :content (new-speech-act *)) msg)
	  (when (and matched-p
		     (eq channel (lf-channel lf))
		     (eq *uttnum* (lf-uttnum lf)))
	    (setf (gethash text *cache*) (copy-tree lf))
	    (return lf))))
      )
    (logging2:log-message :note (list :deferred-message msg))
    (setq dfc::*pending-messages* (append dfc::*pending-messages* (list msg)))
    ))

(defun send-lf-to-im (lf)
  ;(send-msg `(tell :receiver IM :content (start-conversation)))
  (send-msg `(tell :receiver IM :content (new-speech-act ,lf))))

