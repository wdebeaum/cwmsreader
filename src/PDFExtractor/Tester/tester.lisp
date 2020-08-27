(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
                       :name "trips")))

(load #!TRIPS"src;PDFExtractor;Tester;defsys")

(dfc:load-component :tester)

(defun run ()
  (setf io::*transport* :socket)
  (dfc:run-component :tester))

