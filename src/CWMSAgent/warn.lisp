;;;;
;;;; warn.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 24 Sep 2003
;;;; Time-stamp: <Tue Oct 31 15:46:16 EDT 2017 jallen>
;;;;

(in-package :cwmsagent)

(defun da-warn (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :warn msg)
    (format *trace-output* "~&cwms: warning: ~A~%" msg)))

(defun da-debug (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :debug msg)
    (format *trace-output* "~&cwms: ~A~%" msg)))

(defvar *step* nil)
(defvar *trace-level* nil)

(defun set-debug-level (&key level)
  (case level
    (off (trace-off))
    (debug (trace-on 1))
    (otherwise (trace-on 2))))

(defun trace-on (n)
  (if (numberp n)
      (setq *trace-level* n)
    (da-warn "Step level must be a number")))

(defun trace-off nil
  (setq *trace-level* nil)
  (setq *step* nil))

(defun trace-msg (n &rest args)
  (when *trace-level*
    (when (>= *trace-level* n)
      (let ((msg (apply #'format nil args)))
	(format *trace-output* "~%CWMS: ~A" (concatenate 'string (case n (1 "") (2 "   ") (3 "     ") (4 "       "))
			       msg))
	
	(when *step* 
	  (format *trace-output* "~%CWMS:     at level ~S, change?:" *trace-level*)
	  (let ((x (read-line)))
	    (if (not (string= x ""))
		(let ((new (read-from-string x)))
		  (if (numberp new) (setq *trace-level* new)
		    (eval x))))))))
    (values)))

(defun debug-pause (n &rest args)
  (when *trace-level*
    (if (>= *trace-level* n)
	(apply #'break args))))

(defun trace-pprint (n msg x)
  (when (and *trace-level* (>= *trace-level* n))
    (format *trace-output* msg)
    (pprint x *trace-output*))
  (values))
