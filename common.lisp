

(provide 'common)

(defpackage common
  (:use common-lisp)
  (:export p-apply
           f-compose
           seq))

(in-package common)

;--------------------------------------
;
;--------------------------------------
(defun p-apply (f arg &rest args-1)
  (lambda (&rest args-2)
    (apply f (append (cons arg args-1) args-2)) ))
(let
  ((f #'(lambda (a) (format nil "~A" a))))
  (assert (equal "1" (apply (p-apply f "1") '())))
)
(let
  ((f #'(lambda (a b) (format nil "~A,~A" a b))))
  (assert (equal "1,2" (apply (p-apply f "1" "2") '())))
  (assert (equal "1,2" (apply (p-apply f "1") '("2"))))
)
(let
  ((f #'(lambda (a b c) (format nil "~A,~A,~A" a b c))))
  (assert (equal "1,2,3" (apply (p-apply f "1" "2" "3") '())))
  (assert (equal "1,2,3" (apply (p-apply f "1" "2") '("3"))))
  (assert (equal "1,2,3" (apply (p-apply f "1") '("2" "3"))))
)
(let
  ((f #'(lambda (a b c d) (format nil "~A,~A,~A,~A" a b c d))))
  (assert (equal "1,2,3,4" (apply (p-apply f "1" "2" "3" "4") '())))
  (assert (equal "1,2,3,4" (apply (p-apply f "1" "2" "3") '("4"))))
  (assert (equal "1,2,3,4" (apply (p-apply f "1" "2") '("3" "4"))))
  (assert (equal "1,2,3,4" (apply (p-apply f "1") '("2" "3" "4"))))
)

;--------------------------------------
;
;--------------------------------------
(defun f-compose (func-1 func-2 &rest func-s)
  (lambda (arg-1 &rest arg-s)
    (first (reduce
             #'(lambda (f accum)
                 (list (apply f accum)) )
             (cons func-1 (cons func-2 func-s))
             :from-end t
             :initial-value (cons arg-1 arg-s) ))))
(let
  ((f (lambda (a) (format nil "f~Af" a)))
   (g (lambda (a) (format nil "g~Ag" a)))
   (h (lambda (a) (format nil "h~Ah" a))) )
  (assert (equal "gf1fg" (apply (f-compose g f) '(1))))
  (assert (equal "fg1gf" (apply (f-compose f g) '(1))))
  (assert (equal "hgf1fgh" (apply (f-compose h g f) '(1))))
)
(let
  ((f (lambda (a b c) (format nil "f~A,~A,~Af" a b c)))
   (g (lambda (a) (format nil "g~Ag" a)))
   (h (lambda (a) (format nil "h~Ah" a))) )
  (assert (equal "gf1,2,3fg" (apply (f-compose g f) '(1 2 3))))
  (assert (equal "hgf1,2,3fgh" (apply (f-compose h g f) '(1 2 3))))
)

;--------------------------------------
;
;--------------------------------------
(defun seq (i-start i-end &optional (interval 1))
  (reverse
    (rev-seq
      i-start
      i-end
      interval
      nil)))
(defun rev-seq (i-start i-end interval accum)
  (cond
    ((< interval 1) nil)
    ((< i-end i-start) accum)
    (t (rev-seq (+ i-start interval) i-end interval (cons i-start accum))) ))
(let
  ()
  (assert (equal '(0)     (seq 0 0)))
  (assert (equal '(0 1)   (seq 0 1)))
  (assert (equal '(0 1 2) (seq 0 2)))
  (assert (equal '()      (seq 0 (- 1))))
  (assert (equal '()      (seq 0 (- 2))))
  (assert (equal '(0 1 2) (seq 0 2 1)))
  (assert (equal '(0 2)   (seq 0 2 2)))
  (assert (equal '(0)     (seq 0 2 3)))
  (assert (equal '()      (seq 0 2 0)))
  (assert (equal '()      (seq 2 0 (- 1))))
)

