;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Wed Oct 27 10:20:15 2010 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2010 Madhu.  All Rights Reserved.
;;;
;;; Convenience API over CL-SVG
;;;
(defpackage "SVG-USER" (:use "CL"))
(in-package "SVG-USER")

#+nil
(require 'cl-svg)


;;; ----------------------------------------------------------------------
;;;
;;; SCENE TO FILE
;;;

(defmacro with-scene-to-file
    ((scene-var &key (file #p"/dev/shm/test.svg") (ndiv 7) width height
		(length 400);evaluated
		rest bindings)
     &body body)
  (if (and (endp (cdr body)) (find (car (car body)) '(let* let)))
      (setq bindings (append (cadr (car body)) bindings)
	    body (cddr (car body))))
  `(let* ((*width* (or ,width ,length ,height (error "specify width")))
	  (*height* (or ,height ,length *width* (error "specify height")))
	  (*length* (let ((len (min *width* *height*)))
		      (unless (eql len ,length)
			(when ,length (format t "with-scene-to-file setting length=~A instead of ~A" len ,length)))
		      len))
	  (*ndiv* ,ndiv)
	  (*adiv* (float (/ *length* *ndiv*)))
	  (*center* (complex  (/ *width* 2) (/ *height* 2)))
	  (*scene* (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
					  :height *height*
					  :width *width* ,@rest))
	  (,scene-var *scene*)
	  ,@bindings)
     (declare (special *scene*
		       *length* *height* *width*
		       *ndiv* *adiv* *center*))
     (multiple-value-prog1 (progn ,@body)
       (with-open-file (stream ,file :direction :output :if-exists :supersede)
	 (svg:stream-out stream *scene*)))))

;;; ----------------------------------------------------------------------
;;;
;;; DRAW FUNCTIONS (instead of macros)
;;;

(defun call-svg-draw (scene shape-and-params draw-opts &optional func)
  (let ((elem (svg::make-svg-element (car shape-and-params)
				     (append (cdr shape-and-params)
					     draw-opts))))
    (svg::add-element scene elem)
    (when func (funcall func elem))
    elem))

(defun annot-text (scene x y text &rest draw-opts)
  (let ((cont (call-svg-draw scene
			     `(:text :x ,x :y ,y)
			     draw-opts)))
    (svg::add-element cont text)
    cont))

(defun draw-rect (scene x1 y1 width height &rest draw-opts)
  (call-svg-draw scene
		 `(:rect :x ,x1 :y ,y1 :width ,width :height ,height)
		 draw-opts))

(defun draw-triangle (scene p1 p2 p3 &rest draw-opts)
  (call-svg-draw scene
		 (list :path
		       :id :generate
		       :d (svg:path
			    (svg:move-to (realpart p1) (imagpart p1))
			    (svg:line-to (realpart p2) (imagpart p2))
			    (svg:line-to (realpart p3) (imagpart p3))
			    (svg:line-to (realpart p1) (imagpart p1))
			    (svg:close-path)))
		 draw-opts))


;;; ----------------------------------------------------------------------
;;;
;;; (RANDOM) RADIAL GRADIENTS
;;;

(defun make-radial-gradient (scene center radius id)
  (svg:make-radial-gradient
      scene
      (:id (or id :generate)
	   :fx (realpart center) :fy (imagpart center)
	   :cx (realpart center) :cy (imagpart center)
	   :r radius
	   "gradientUnits" "userSpaceOnUse")))

(defun hsv-to-rgb (h s v)	   ;2005-04-23 by way of DHA
  (declare (type (integer 0 360) h)
	   (type (integer 0 100) s v))
  (let ((h (/ h 60.0)) (s (/ s 100.0)) (v (/ v 100.0)))
    (multiple-value-bind (i f) (floor h)
      (if (evenp i) (setq f (- 1 f)))
      (let ((m (* v (- 1 s))) (n (* v (- 1 (* s f)))))
	(format nil "rgb(~{~a~^, ~})" (mapcar (lambda (x) (floor (* x 255)))
	       (ecase i
		 ((0 6) (list v n m))
		 (1 (list n v m))
		 (2 (list m v n))
		 (3 (list m n v))
		 (4 (list n m v))
		 (5 (list v m n)))))))))

(defun make-nstops (n h)
  "Make N gradient stops for hue h of varying brightness (value)"
  (assert (< h 360))
  (loop for i from 0 to n
	for percent = (* 100 (/ i n))
	for percent-string = (format nil (if (integerp percent)
					     "~D%"
					     "~3,2F%")
				     percent)
	for s =  100
	for v =  (floor (- 100 percent))
	for color = (hsv-to-rgb h s v)
	collect (cl-svg:gradient-stop :color color
				      :offset percent-string)))

(defun make-mhues-uniform (m)
  (loop for i below m collect (floor (* i 360 (/ m)))))

(defun make-mhues-random (m &optional (random-state *random-state*))
  (flet ((integer-between (min max)
	   "Return NUMBER such that MIN <= NUMBER <= MAX."
	   (cond ((< min max) (+ min (random (- max min -1) random-state)))
		 ((= min max) min)
		 (t (error "(min max) ~S out of order." (list min max))))))
    (loop for i below m collect (integer-between 0 360))))

(defun make-mgradients-nstops (mgradients nstops &optional (type :uniform))
  (mapcar (lambda (h) (make-nstops nstops h))
	  (ecase type
	    (:uniform (make-mhues-uniform mgradients))
	    (:random (make-mhues-random mgradients)))))

(defun make-radial-gradients-from-nstops-list (nstops-list center radius scene &optional id)
  (loop for nstop-forms in nstops-list
	for x = (make-radial-gradient scene center radius id)
	do (loop for stop in nstop-forms do (svg:add-element x stop))
	collect x))

#+nil
(let ((nstops 6) (mgradients 18) (type :random))
  (with-scene-to-file (scene :file #p"/dev/shm/test.svg" :ndiv mgradients)
    (let ((gradients
	   (mapcar #'svg:xlink-href
		   (make-radial-gradients-from-nstops-list
		    (make-mgradients-nstops mgradients nstops type)
		    *center* *length* scene))))
      (loop with y = 0 for i from 0 for x = (* i *adiv*) for colour in gradients
	    do (draw-rect scene x y *adiv* (/ *length* 2) :fill colour))
      (loop with x = 0 for i from 0 for y = (+ (/ *length* 2) (* i *adiv* 1/2))
	    for colour in gradients
	    do (draw-rect scene x y *length* *adiv* :fill colour)))))

