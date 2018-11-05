(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;cwmsAgent;defsys")

(mk:load-system :cwmsAgent)


