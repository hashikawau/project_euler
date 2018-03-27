
(require 'common)
(use-package 'common)

(defun main ()
  ;(let
  ;  ((lst '(1 2 3 4 5 6 7)))
  ;  (format
  ;    t
  ;    "~A: ~A~%"
  ;    lst
  ;    (mapcar
  ;      (p-apply #'count-up lst)
  ;      (reverse (seq 0 (1- (length lst)))))))

  ;(let
  ;  ((lst '(0 1 1 2 0 1 1 3)))
  ;  (format t "~A~%" (to-pound lst)))

  ;(let
  ;  ((lst '(0 1 1 2 0 1 1)))
  ;  (format t "~A~%" (to-pound-without-1 lst)))

  ;(let
  ;  ((lst '(0 1 1 2 0 1 1)))
  ;  (format
  ;    t
  ;    "~A -> ~%~A~%" lst (succ (p-apply #'test-if-under 200) lst)))

  (time
  (let*
    ((limit 200)
     (lst-lst (generator limit))
     ;(lst-sum (mapcar #'to-pound-without-1 lst-lst))
    )
    (format t "~A~%" (length lst-lst)) )
  )

)

(defun generator (limit-pound)
  (let
    ((init '(0 0 0 0 0 0 0)))
    (do
      ((each-coin-numbers-without-1
         init
         (succ (p-apply #'test-if-under limit-pound) each-coin-numbers-without-1))
       (accum
         nil
         (cons each-coin-numbers-without-1 accum)))
      ((null each-coin-numbers-without-1) accum)
      () )))

(defun succ (test-f each-coin-numbers-without-1)
  (first
    (remove-if-not
      test-f
      (count-up-patterns each-coin-numbers-without-1) )))

(defun test-if-under (limit-pound each-coin-numbers-without-1)
  (<= (to-pound-without-1 each-coin-numbers-without-1) limit-pound))

(defun to-pound-without-1 (each-coin-numbers-without-1)
  (to-pound (append each-coin-numbers-without-1 '(0))))

(defun to-pound (each-coin-numbers)
  (sum
    (mapcar #'* '(200 100 50 20 10 5 2 1) each-coin-numbers) ))

; (1 2 3 4) -> ((1 2 3 5) (1 2 4 0) (1 3 0 0) (2 0 0 0))
(defun count-up-patterns (digits)
  (let
    ((num-digits (length digits)))
    (mapcar
      (p-apply #'count-up digits)
      (reverse (seq 0 (1- num-digits))) )))

; (1 2 3 4) 3 -> (1 2 3 5)
; (1 2 3 4) 2 -> (1 2 4 0)
; (1 2 3 4) 1 -> (1 3 0 0)
; (1 2 3 4) 0 -> (2 0 0 0)
(defun count-up (digits order)
  (append
    (take order digits)
    (list (1+ (nth order digits)))
    (repeat 0 (- (length digits) order 1))))



(main)

