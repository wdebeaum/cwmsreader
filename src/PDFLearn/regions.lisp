(in-package :pdflearn)

;;; accessors for rectangular regions from PDFExtractor
(defun id (r) (find-arg-in-act r :id))
(defun min-x (r) (find-arg-in-act r :x))
(defun min-y (r) (find-arg-in-act r :y))
(defun width  (r) (or (find-arg-in-act r :w) 1))
(defun height (r) (or (find-arg-in-act r :h) 1))
(defun max-x (r) (1- (+ (min-x r) (width  r))))
(defun max-y (r) (1- (+ (min-y r) (height r))))
(defun center-x (r) (/ (+ (max-x r) (min-x r)) 2))
(defun center-y (r) (/ (+ (max-y r) (min-y r)) 2))

(defun make-region (&key
		    (page nil)
		    (min-x nil) (x min-x)
		    (min-y nil) (y min-y)
		    (max-x x) (w (1+ (- max-x x)))
		    (max-y y) (h (1+ (- max-y y))))
  (unless (every #'numberp (list x y w h))
    (error "not enough coordinates"))
  `(rectangle
     ,@(when page (list :page page))
     :x ,x :y ,y :w ,w :h ,h))

(defun inside-p (inner outer)
  "Is inner inside outer? Both are region descriptions."
  (and (< (min-x outer) (min-x inner) (max-x inner) (max-x outer))
       (< (min-y outer) (min-y inner) (max-y inner) (max-y outer))))

(defun region-distance-metric (r target page)
  "Given two region descriptions (and a page description), return their
   distance metric (the same one used by PDFExtractor)."
  (let* ((page-bb (find-arg-in-act page :bounds))
         (page-width (width page-bb))
	 (page-height (height page-bb))
	 (delta-center-x (abs (- (center-x r) (center-x target))))
	 (delta-center-y (abs (- (center-y r) (center-y target))))
	 (target-width (width target))
	 (target-height (height target))
	 (delta-width (abs (- (width r) target-width)))
	 (delta-height (abs (- (height r) target-height)))
	 )
    (+ (/ delta-center-x page-width) (/ delta-center-y page-height)
       (/ delta-width target-width) (/ delta-height target-height))))

(defun match-distance-metric (new-match old-match)
  "Like region-distance-metric, but for text matches. Does not include
   region-distance-metric for the matches' regions."
  (let* ((old-context-length (length (find-arg-in-act old-match :context)))
	 (old-found-center (/ (+ (find-arg-in-act old-match :start)
				 (find-arg-in-act old-match :end))
			      2))
	 (new-found-center (/ (+ (find-arg-in-act new-match :start)
				 (find-arg-in-act new-match :end))
			      2))
	 (delta-found-center (abs (- old-found-center new-found-center)))
	 )
    (/ delta-found-center old-context-length)))

