(defpackage :cg-dsl
  (:use :cl
        :anaphora
        :alexandria        
        :split-sequence)
  (:export #:defshape
           #:defop
           #:defop*
           #:defctor))

(in-package :cg-dsl)

;; typecheck
(deftype slot-designator ()
  `(or list symbol))

(defun check-slot-designators (list)
  (awhen (find-if-not (of-type 'slot-designator) list)
    (error "~a is not valid slot-designator." it)))

;; dotted-notation transformer aggregator
;;  -- builds list of bindings for symbol-macrolet

(defun aggregate-dot-transformers (list)
  (remove-duplicates
   (mapcar (lambda (x)
             `(,x ,(reduce (lambda (prefix postfix)
                             `(slot-value ,prefix ',postfix))
                           (mapcar 'intern
                                   (split-sequence #\. (string x))))))
           (remove-if-not (lambda (x)
                            (and (symbolp x)
                                 (find #\. (string x))))
                          (flatten list)))
   :test 'equal))

;; basic
(defmacro defshape (name &body slot-designators)
  (check-slot-designators slot-designators)
  `(defstruct ,name
     ,@(loop :for sd :in slot-designators
             :for slot = (ensure-list sd)
             :collect `(,(car slot); :initarg
                       ;,(make-keyword (car slot))
                       ,@(cdr slot)))))

(defmacro defop (name args &body body)
  `(defun ,name ,(mapcar 'car args)
     (declare ,@(mapcar 'reverse args))
     (symbol-macrolet ,(aggregate-dot-transformers body)
       ,@body)))

(defmacro defop* (name args &body body)
  `(defmethod ,name ,args
     (symbol-macrolet ,(aggregate-dot-transformers body)
       ,@body)))

(defmacro defctor (class name args &body body)
  `(defun ,(format-symbol t "MK-~a" name) ,args
     (,(format-symbol t "MAKE-~a" class) ,@body)))