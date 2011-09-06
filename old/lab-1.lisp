(defpackage :cg-lab-1
  (:use #:cl #:lambda-reader)
  (:shadow #:intersection))

(in-package :cg-lab-1)

;; &pre
(with-lambda-reader (#\# #\~))

;; &nan
(defconstant +nan+ '+nan+)

(defconstant +nan-point+
  (make-point :x +nan+ :y +nan+))

;; &types
(defstruct (point (:conc-name pt-))
  x y)

(defstruct (line-segment (:conc-name ls-))
  (pt-1 +nan-point+ :type point)
  (pt-2 +nan-point+ :type point))

;; &syntax
(defmacro with-point-coords ((x y of pt &rest bindings) &body body)
  `(symbol-macrolet ((,x (pt-x ,pt)) (,y (pt-y ,pt)))
     ,(if bindings `(with-point-coords ,bindings ,@body)
                   `(progn ,@body))))

(defmacro with-line-segment-coords ((x1 y1 x2 y2 of ls &rest bindings) &body body)
  `(with-point-coords (,x1 ,y1 of (ls-pt-1 ,ls))
     (with-point-coords (,x2 ,y2 of (ls-pt-2 ,ls))
       ,(if bindings `(with-line-segment-coords ,bindings ,@body)
                     `(progn ,@body)))))

;; &helper
(defun make-ls (x1 y1 x2 y2)
  (make-line-segment :pt-1 (make-point :x x1 :y y1)
                     :pt-2 (make-point :x x2 :y y2)))

(defun pt-in-ls (pt ls)
  (with-line-segment-coords (x1 y1 x2 y2 of ls)
    (with-point-coords (x y of pt)
      (and (<= (min x1 x2) x (max x1 x2))
           (<= (min y1 y2) y (max y1 y2))))))

;; &main
(defun intersection (ls-1 ls-2)
  (with-line-segment-coords (x1 y1 x2 y2 of ls-1
                             x3 y3 x4 y4 of ls-2)
    (let* ((c (- (* (- y2 y1)
                    (- x4 x3))
                 (* (- y4 y3)
                    (- x2 x1))))
           (x (if (zerop c)
                  +NaN+
                  (/ (- (* (- (* x1 y2)
                              (* x2 y1))
                           (- x4 x3))
                        (* (- (* x3 y4)
                              (* x4 y3))
                           (- x2 x1)))
                     c)))          
           (y (if (eq x +NaN+)
                  +NaN+
                  (if (/= x3 x4)
                      (/ (- (* x (- y4 y3))
                            (* x3 x4)
                            (- (* x4 y3)))
                         (- x4 x3))
                      (+ y1 (/ (* (- x x1)
                                  (- y2 y1))
                               (- x2 x1))))))
           (result (make-point :x x :y y)))
      (when (and (not (or (eq +NaN+ x) (eq +NaN+ y)))
                 (point-in-range result ls-1)
                 (point-in-range result ls-2))
        result))))

(defun intersection (ls-1 ls-2)
  (with-line-segment-coords (x1 y1 x2 y2 of ls-1
                             x3 y3 x4 y4 of ls-2)
    (cond
      ((and (= x1 x2) (= y1 y2) (pt-in-ls (ls-pt-1 ls-1) ls-2))
       (ls-pt-1 ls-1))
      ((and (= x3 x4) (= y3 y4) (pt-in-ls (ls-pt-1 ls-2) ls-1))
       (ls-pt-1 ls-2))
      ))

;; &test
(defparameter *ls-1* (make-ls 0 0 0 0))
(defparameter *ls-2* (make-ls 0 0 1 1))

(point-in-range (make-point :x 0 :y 0) *ls-2*)

(intersection *ls-1* *ls-2*)
> #S(POINT :X 1 :Y 4/3)

;; &post
(without-lambda-reader)







