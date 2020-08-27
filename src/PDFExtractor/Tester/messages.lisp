(in-package :tester)

(defvar +doc-dir+ "/home/will/jobs/ihmc/cwms/reading/")
(defvar +docs+ '(
  ("62800.pdf" 0 1)
  ("a-I4256E.pdf" 0 1 4 5 6 9 12 14 16)
  ("Akobo.pdf" 0)
  ("AP103-9-Final.pdf" 2 4 6 7 21 79 80 89)
  ("Assessment_of_maize_growth_and_yield_using_crop_mo.pdf" 1 2 3 4 5 7 9 12)
  ("Atlas-Final-Web-Version-6_14.pdf" 4 5 6 8 9 10 12 13 14 15 16 24 25 27 31)
  ))
(defvar *test-msg* nil)
(defvar *log* nil)
(defvar *agenda* nil)

(defun unwrap-answer (reply-content key)
  (find-arg-in-act (find-arg-in-act reply-content :content) key))

(defun log-region-contents (region)
  (destructuring-bind (&key id x y w h &allow-other-keys)
      (cdr region)
    (format *log* "##### ~sx~s+~s+~s #####~%" w h x y)
    (let* ((req `(request :receiver pdfextractor :content
			  (get-content :of ,id)))
	   (content (unwrap-answer (send-and-wait req) :content)))
      (format *log* "~a~%" content))
    ))

(defun handle-displayed (msg args)
    (declare (ignore msg))
  (destructuring-bind (&key what &allow-other-keys) args
    (destructuring-bind (&key id &allow-other-keys) (cdr what)
      (loop with req = `(request :receiver pdfextractor :content
				 (detect-paragraph-regions :page ,id))
            with regions = (unwrap-answer (send-and-wait req) :regions)
	    for region in regions
	    do (log-region-contents region)
	    )))
  (next))

(defcomponent-handler
  '(tell &key :content (report &key :content (displayed . *)))
  #'handle-displayed
  :subscribe t)

(defun done ()
  (close *log*)
  (reply-to-msg *test-msg* 'tell :content '(done))
  )

(defcomponent-handler
  '(tell &key :content (report &key :content (failure . *)))
  (lambda (msg args) (declare (ignore msg args)) nil))

(defun next ()
  (if *agenda*
    (let* ((item (pop *agenda*))
	   (filename (car item))
	   (page (cdr item)))
      (format *log* "########## ~s page ~s ##########~%" filename page)
      (send-msg `(request :receiver pdfextractor :content
	(display :file (file :name ,(concatenate 'string +doc-dir+ filename)
			     :format "application/pdf")
		 :page ,page))))
    (done)))

(defun handle-test (msg args)
    (declare (ignore args))
  (setf *test-msg* msg)
  (setf *log* (open "test.log" :direction :output :if-exists :supersede))
  (loop for (doc . pages) in +docs+
	do (dolist (page pages)
	     (push (cons doc page) *agenda*))
	)
  (setf *agenda* (nreverse *agenda*))
  (next))

(defcomponent-handler
  '(request &key :content (test . *))
  #'handle-test
  :subscribe t)

