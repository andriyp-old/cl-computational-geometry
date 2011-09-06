; (asdf:clear-source-registry)
; (ql:quickload "cg-dsl")

(defpackage :cg-labs
  (:use :cl :cg-dsl :iter
        :anaphora))

(in-package :cg-labs)

;; inline-оптимизации
(declaim (inline ls-len
                 pt= pt< pt<=
                 mk-pt mk-ls mk-ls/pt
                 pt-dist ls-len pt-in-ls?
                 left-turn-p))

;; вспомогательные макросы
(defmacro sv (obj name)
  `(slot-value ,obj ,name))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
                             (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m)
                                        (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m)
                                        (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar
                      #'(lambda (arg)
                          `(unless (,op ,(car rest) ,arg)
                             (rotatef ,(car rest) ,arg)))
                      (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))


;; фигуры
(defshape point
  (x :type number)
  (y :type number))

(defshape line-segment
  (pt1 :type point)
  (pt2 :type point))

;; конструкторы
(defctor point pt (x y) :x x :y y)

(defctor line-segment ls (x1 y1 x2 y2)
  :pt1 (mk-pt x1 y1)
  :pt2 (mk-pt x2 y2))

(defctor line-segment ls/pt (pt1 pt2)
  :pt1 pt1
  :pt2 pt2)

;; базовые монотиповые операции
(defop pt= ((a point) (b point))
  "Проверяет, равны ли соответствующие координаты точек."
  (and (= a.x b.x) (= a.y b.y)))

(defop pt< ((a point) (b point))
  "Проверяет, меньше ли каждая из координат первой точки соответствующей координаты второй."
  (and (< a.x b.x) (< a.y b.y)))

(defop pt<= ((a point) (b point))
  "Проверяет, меньше-либо-равна ли каждая из координат первой точки соответствующей координаты второй."
  (and (<= a.x b.x) (<= a.y b.y)))

(defop ls= ((a line-segment) (b line-segment))
  "Проверяет, равны ли соответствующие точки отрезков."
  (or (and (pt= a.pt1 b.pt1) (pt= a.pt2 b.pt2))
      (and (pt= a.pt1 b.pt2) (pt= a.pt2 b.pt1))))

(defop pt-dist ((a point) (b point))
  "Вычисляет расстояние между двумя точками."
  (sqrt (+ (expt (- a.x b.x) 2)
           (expt (- a.y b.y) 2))))

(defop ls-len ((ls line-segment))
  "Вычисляет длину отрезка."
  (pt-dist ls.pt1 ls.pt2))

(defop pt-in-ls? ((pt point) (ls line-segment))
  "Проверяет, находится ли точка в отрезке."
  (let ((da (pt-dist pt ls.pt1))
        (db (pt-dist pt ls.pt2)))
    (= (+ da db) (ls-len ls))))

(defop angle ((ls1 line-segment) (ls2 line-segment))
  "Вычисляет угол между отрезками."
  (let ((u (- (atan (- ls1.pt2.y ls1.pt1.y)
                    (- ls1.pt2.x ls1.pt1.x))
              (atan (- ls2.pt2.y ls2.pt1.y)
                    (- ls2.pt2.x ls2.pt1.x)))))
    (if (< u pi) u
        (- (* 2 pi) u))))

(defop left-turn-p ((pt0 point) (pt1 point) (pt2 point))
  "Определяет, находится ли точка pt2 строго слева pt0->pt1."
  (< 0 (- (* (- pt2.y pt0.y) (- pt1.x pt0.x))
          (* (- pt2.x pt0.x) (- pt1.y pt0.y)))))

;; обобщённые функции
(defgeneric intersect (shape-1 shape-2)
  (:documentation "Вычисляет фигуру, являющуюся пересечением даных."))

(defgeneric print-shape (shape)
  (:documentation "Распечатывает представление фигуры на экран."))

;; политиповые операции
(defop* intersect ((pt1 point) (pt2 point))
  (when (pt= pt1 pt2) pt1))

(defop* intersect ((pt point) (ls line-segment))
  (when (pt-in-ls? pt ls) pt))

(defop* intersect ((ls line-segment) (pt point))
  (intersect pt ls))

(defop* intersect ((ls1 line-segment) (ls2 line-segment))
  (let ((p1 ls1.pt1) (p2 ls1.pt2)
        (p3 ls2.pt1) (p4 ls2.pt2))
    (cond
      ;; специальные случаи
       ; -- определённо не пересекаются
      ((let ((x- p3.x) (x+ p4.x)
             (y- p3.y) (y+ p4.y))
         (when (< x+ x-) (rotatef x+ x-))
         (when (< y+ y-) (rotatef y+ y-))
         (or (and (< p1.x x-) (< p2.x x-))
             (and (> p1.x x+) (> p2.x x+))
             (and (< p1.y y-) (< p2.y y-))
             (and (> p1.y y+) (> p2.y y+))))
       nil)
       ; -- совпадения точек
      ((or (pt= p1 p2) (pt= p1 p3) (pt= p1 p4))
       (intersect p1 ls2))
       
      ((or (pt= p3 p4) (pt= p3 p2))
       (intersect p3 ls1))
      ;; параллельный случай
      ((zerop (angle ls1 ls2))
       (sortf pt<= p1 p2 p3 p4)
       (when (and (pt-in-ls? p2 ls1) (pt-in-ls? p2 ls2)
                  (pt-in-ls? p3 ls1) (pt-in-ls? p3 ls2))
         (if (pt= p2 p3) p2
             (mk-ls/pt p2 p3))))
      ;; обыкновенный случай
      (t (let* ((a1 (- p1.y p2.y)) (b1 (- p2.x p1.x))
                (c1 (- (* p1.x p2.y) (* p2.x p1.y)))
                (a2 (- p3.y p4.y)) (b2 (- p4.x p3.x))
                (c2 (- (* p3.x p4.y) (* p4.x p3.y)))
                (w (- (* a1 b2) (* a2 b1))))
           (when (/= w 0)
             (let ((pt (mk-pt (/ (- (* c2 b1) (* c1 b2)) w)
                              (/ (- (* a2 c1) (* a1 c2)) w))))
               (when (and (pt-in-ls? pt ls1)
                          (pt-in-ls? pt ls2))                 
                 pt))))))))

;; локализация точки в простом многоугольнике
(defop localize ((lines list) (pt-z point))
  (let (test-ls)
    ;; построение тестовой линии
    (let ((min-x (iter (for ls :in lines)
                       (minimize (min ls.pt1.x
                                      ls.pt2.x)))))
      (when (< pt-z.x min-x)
        (return-from localize :out))
      (setq test-ls
            (mk-ls min-x pt-z.y
                   pt-z.x pt-z.y)))
    ;; цикл по отрезкам
    (iter (for ls :in lines)
          (when (pt-in-ls? pt-z ls)
            (return-from localize :on))
          (for isect := (intersect test-ls ls))
          (when (and (typep isect 'point)
                     (or (> ls.pt1.y pt-z.y)
                         (> ls.pt2.y pt-z.y)))
            (sum 1 :into n))
          (finally
           (return (if (= 0 (mod n 2))
                       :out :in))))))

;; построение выпуклой оболочки методом Грэма (graham-scan)
(defop convex-hull ((points list))
  (let ((pt0 (car points)))

    (iter (for pt :in (cdr points))
          (when (or (< pt.y pt0.y)
                    (and (= pt.y pt0.y)
                         (> pt.x pt0.x)))
            (setq pt0 pt)))

    (flet ((cmp (pt1 pt2)
             (or (< (atan (- pt1.y pt0.y) (- pt1.x pt0.x))
                    (atan (- pt2.y pt0.y) (- pt2.x pt0.x)))
                 (< (pt-dist pt0 pt1) (pt-dist pt0 pt2)))))
      
      (iter (with sorted := (sort points #'cmp))
            (with stack  := (list (cadr sorted)
                                  (car sorted)))
            
            (for pt2 :in (cddr sorted))

            (iter (for pt0 := (cadr stack))
                  (for pt1 := (car  stack))                  
                  
                  (when (left-turn-p pt0 pt1 pt2)
                    (push pt2 stack)
                    (finish))
                  
                  (pop stack)

                  (when (null (cdr stack))
                    (return-from convex-hull
                      sorted)))

            (finally (return stack))))))

;; операции вывода фигур
(defop* print-shape ((nothing t))
  (format t "nothing~%"))

(defop* print-shape ((pt point))
  (format t "point: (~a; ~a)~%" pt.x pt.y)
  pt)

(defop* print-shape ((ls line-segment))
  (format t "line-segment: ((~a; ~a); (~a; ~a))~%"
    ls.pt1.x ls.pt1.y ls.pt2.x ls.pt2.y)
  ls)

;; tests
(defun lab-1/test (x1 y1 x2 y2 x3 y3 x4 y4)
  (print-shape (intersect (mk-ls x1 y1 x2 y2)
                          (mk-ls x3 y3 x4 y4))))

(defun lab-2/test (x1 y1 x2 y2 x3 y3 x4 y4)
  (format t "~&angle = ~a"
          (angle (mk-ls x1 y1 x2 y2)
                 (mk-ls x3 y3 x4 y4))))

(defun lab-3/test (zx zy &rest polygon-coords)
  (localize
   (iter (with (px py) := polygon-coords)
         (for (x y) :on (cddr polygon-coords)
                    :by #'cddr)
         (collect (mk-ls px py x y))
         (setf px x py y))
   (mk-pt zx zy)))


;(disassemble 'lab-1/time-test)

;(lab-1/test 4 4 4 0 4 2 4 2)
;(lab-1/time-test 1000000)
;(disassemble 'test)
;(describe 'slot-value)

;(defvar *test-assoc* '((1 x) (2 y)))
;(push 3 (cdr (assoc 1 *test-assoc*)))

