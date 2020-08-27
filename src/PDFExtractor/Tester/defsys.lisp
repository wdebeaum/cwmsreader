(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
                       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(defpackage :ont) ; so we can read colors

(dfc:defcomponent :tester
		  :use (:util :common-lisp)
		  :system (
		    :depends-on (:util)
		    :components (
		      "messages"
		      )))
