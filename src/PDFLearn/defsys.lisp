(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :om)
  (load #!TRIPS"src;OntologyManager;defsys"))

(unless (find-package :w)
  (load #!TRIPS"src;LexiconManager;w-pkg"))

(dfc:defcomponent :pdflearn
		  :nicknames (:pl)
		  :use (:util :om :common-lisp)
		  :system (
		    :depends-on (:util :om)
		    :components (
		      "lfs"
		      "send"
		      "regions"
		      "html"
		      "parse-table"
		      "constraints"
		      "learn"
		      "find"
		      "edit"
		      "receive"
		      )))

(defvar *uttnum* 0 "The last :uttnum used to send an utterance to the Parser.")
(defvar *rules* (make-hash-table :test #'eq) "Maps rule IDs to situations (region finding rules) or table-edit-rules")
(defvar *rules-file* #!TRIPS"etc;pdflearn.rules" "The file where rules are saved.")
(defvar *keep-rules-in-file* t "If T, load rules from *rules-file* on startup, and save all rules there each time we learn one. If NIL, don't use the *rules-file*.")
(defvar *region-to-rule* (make-hash-table :test #'eq) "Maps region IDs to the rules that found them or were learned from them.")

(defun clear-rules ()
  (setf *rules* (make-hash-table :test #'eq))
  (setf *region-to-rule* (make-hash-table :test #'eq))
  )

(defun save-rules ()
  (when *keep-rules-in-file*
    (with-open-file (f *rules-file* :direction :output :if-exists :supersede)
      (loop for id being the hash-keys of *rules* using (hash-value rule)
	    do (format f "~s~%  ~s~%" id rule)
	    ))))

(defun add-rule (id rule)
  (setf (gethash id *rules*) rule)
  (save-rules)
  )

(defun load-rules ()
  (when (and *keep-rules-in-file* (probe-file *rules-file*))
    (format *trace-output* ";; loading PDFLearn rules from ~s~%" *rules-file*)
    (with-open-file (f *rules-file* :direction :input)
      (loop for id = (read f nil)
            for rule = (read f nil)
	    while (and id rule)
	    do (setf (gethash id *rules*) rule)
	    ))))

(dfc:defcomponent-method dfc:init-component :after ()
  (load-rules))
