(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;PDFLearn;defsys")
(dfc:load-component :pdflearn)
(defun run ()
  (dfc:run-component :pdflearn))

