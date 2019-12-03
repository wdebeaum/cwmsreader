(in-package :pdflearn)

;;;; functions dealing with LFs from the Parser's new-speech-act messages

(defun id-p (x)
  "Is x an LF term ID (variable) symbol like V12345?"
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\V)
       (every #'digit-char-p (subseq (symbol-name x) 1))))

(defun replace-term-id (old-id id-map)
  "Helper for copy-lf. Return a new ID to replace old-id with, using the one
   from id-map if it's already there, and storing it in id-map if not."
  (multiple-value-bind (new-id present-p) (gethash old-id id-map)
    (if present-p
      new-id
      (setf (gethash old-id id-map) (gentemp "V" :ont))
      )))

(defun copy-lf (lf &optional (id-map (make-hash-table :test #'eq)))
  "Return a copy of lf with all IDs replaced, :channel changed from heading
   to cell, :uttnum changed to *uttnum*, and some other parts dropped."
  (cond
    ((listp (car lf)) ; list of UTTs
      (mapcar (lambda (e) (copy-lf e id-map)) lf))
    ((eq 'utt (car lf)) ; single UTT
      (destructuring-bind (_ &key type root terms
      			   ;; we copy these over even though they will be wrong
      			   start end words tree
			   &allow-other-keys
			  ) lf
          (declare (ignore _))
        `(utt :type ,type :channel cell :root ,(replace-term-id root id-map)
	      :terms ,(copy-lf terms id-map) :uttnum ,*uttnum*
	      :start ,start :end ,end :words ,words :tree ,tree)))
    ((eq 'compound-communication-act (car lf)) ; CCA
      (destructuring-bind (_ &key acts
      			   start end words tree
			   &allow-other-keys
			  ) lf
          (declare (ignore _))
	`(compound-communication-act
	    :acts ,(copy-lf acts id-map)
	    :start ,start :end ,end :words ,words :tree ,tree)))
    ((eq 'term (car lf)) ; individual LF term
      (destructuring-bind (_ &key ((:lf term)) var sem
			   input start end
			   &allow-other-keys
			  ) lf
          (declare (ignore _))
	(destructuring-bind (indicator id type &rest args) term
	  `(term
	    :lf
	      (,indicator ,(replace-term-id id id-map) ,type
		,@(mapcar
		    (lambda (arg)
		      (cond
			((id-p arg) (replace-term-id arg id-map))
			((and (listp arg) (every #'id-p arg))
			  (mapcar (lambda (x) (replace-term-id x id-map)) arg))
			(t arg)
			))
		    args))
	    :var ,(replace-term-id var id-map)
	    :sem ,sem
	    :input ,input :start ,start :end ,end
	    ))))
    (t (error "unknown type of LF or part of LF:~%  ~s~%" lf))
    ))

(defun get-term-id (term)
  "Get the term ID (variable) from one of the (TERM...) structures from the
   Parser."
  (find-arg-in-act term :var))

(defun find-term (term-id terms) (find term-id terms :key #'get-term-id))

(defun add-args-to-term (term args)
  "Destructively modify the :LF of term to include the given args."
  (let ((lf (find-arg-in-act term :lf)))
    (setf (cdr lf) (nconc (cdr lf) args))))

(defun lf-content-terms (lf)
  "Get the terms of (the first fragment of) the lf except for the root
   (speechact) term, and the ID in the speechact's :content arg, as multiple
   values. If :type W::SA_FRAGMENT (no speechact term), return all the terms
   and the :root ID."
  (cond
    ((listp (car lf)) (lf-content-terms (car lf)))
    ((eq 'utt (car lf))
      (destructuring-bind (_ &key type root terms &allow-other-keys) lf
          (declare (ignore _))
	(if (eq type 'w::sa_fragment)
	  (values terms root)
	  (let* ((root-term (find-term root terms))
		 (content-id (find-arg (cdddr (find-arg-in-act root-term :lf))
				       :content)))
	    (unless (and root-term content-id)
	      (error "LF with :type ~s missing :root or SA :content:~%  ~s~%"
		     type lf))
	    (values (remove root-term terms :test #'eq) content-id))
	  )))
    ((eq 'compound-communication-act (car lf))
      (lf-content-terms (find-arg-in-act lf :acts)))
    (t (error "can't find terms in lf:~%  ~s~%" lf))
    ))

(defun lf-content-type (lf)
  "Get the simple type of the term that is the :content of the speechact term."
  (multiple-value-bind (terms content-id) (lf-content-terms lf)
    (let* ((content-term (find-term content-id terms))
           (content-type (third (find-arg-in-act content-term :lf))))
      (when (and (listp content-type)
		 (= 3 (length content-type))
		 (eq :* (first content-type)))
        (setf content-type (second content-type)))
      content-type)))

(defun add-terms-to-lf (lf terms)
  "Destructively add the given terms to (the first fragment of) the lf."
  (cond
    ((listp (car lf)) (add-terms-to-lf (car lf) terms))
    ((eq 'utt (car lf))
      (let ((old-terms (find-arg-in-act lf :terms)))
        (setf (cdr old-terms) (nconc (cdr old-terms) terms))))
    ((eq 'compound-communication-act (car lf))
      (add-terms-to-lf (find-arg-in-act lf :acts) terms))
    (t (error "can't find terms in lf:~%  ~s~%" lf))
    ))

(defun lf-channel (lf)
  "Get the :channel argument from some part of a new-speech-act."
  (cond
    ((listp (car lf)) (lf-channel (car lf)))
    ((eq 'utt (car lf)) (find-arg-in-act lf :channel))
    ((eq 'compound-communication-act (car lf))
      (lf-channel (find-arg-in-act lf :acts)))
    (t (error "can't find channel in lf:~%  ~s~%" lf))
    ))

(defun lf-uttnum (lf)
  "Get the :uttnum argument from some part of a new-speech-act."
  (cond
    ((listp (car lf)) (lf-uttnum (car lf)))
    ((eq 'utt (car lf)) (find-arg-in-act lf :uttnum))
    ((eq 'compound-communication-act (car lf))
      (lf-uttnum (find-arg-in-act lf :acts)))
    (t (error "can't find uttnum in lf:~%  ~s~%" lf))
    ))

