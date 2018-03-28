
(require 'common)
(use-package 'common)

(defun main ()
  (time (let ((products (pandigital-products (seq 1 4)))) (format t "--------: sum=~A, ~A~%" (sum (mapcar #'first products)) products)))
  (time (let ((products (pandigital-products (seq 1 5)))) (format t "--------: sum=~A, ~A~%" (sum (mapcar #'first products)) products)))
  (time (let ((products (pandigital-products (seq 1 6)))) (format t "--------: sum=~A, ~A~%" (sum (mapcar #'first products)) products)))
  ;(time (let ((products (pandigital-products (seq 1 8)))) (format t "--------: sum=~A, ~A~%" (sum (mapcar #'first products)) products)))
  (time (let ((products (pandigital-products (seq 1 9)))) (format t "--------: sum=~A, ~A~%" (sum (mapcar #'first products)) products)))
)

(defun pandigital-products (digit-list)
  (let
    ((accum nil))
    (for-each
      (permutations digit-list)
      #'(lambda (lst)
          (let
            ((begin 1)
             (end (1- (length lst))))
          (do
            ((i begin (1+ i)))
            ((>= i end) nil)
            (do
              ((j (1+ i) (1+ j)))
              ((> j end) nil)
              (let*
                 (
                  (a (to-integer (sub-list lst 0 i)))
                  (b (to-integer (sub-list lst i j)))
                  (c (to-integer (sub-list lst j)))
                 )
                 (if (equal c (* a b))
                   (progn
                     (format t "a=~A, b=~A, c=~A, a*b=~A~%" a b c (* a b))
                     (setf accum (cons (list c a b) accum)))
                   () ))
              )))))
    (remove-duplicates accum :key #'first)
  )
)

(defun to-integer (digits)
  (sum
    (mapcar
      #'*
      (order-lst (1- (length digits)))
      (reverse digits) )))

(defun order-lst (limit)
  (mapcar
    (p-apply #'expt 10)
    (seq 0 limit)) )

;(defun to-integer (digits)
;  (parse-integer
;    (apply
;      #'concatenate
;      (cons 'string (mapcar #'write-to-string digits)))))

(defun sub-list (lst i-start &optional (i-end (length lst)))
  (take
    (- i-end i-start)
    (drop
      i-start
      lst)))

(main)

