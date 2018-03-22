
; -------------------------------------
(defun comp (f g)
  (lambda (x1 &optional (x2 :nouse) (x3 :nouse))
    (funcall
      f
      (apply
        g
        (remove-if #'nouse-p (list x1 x2 x3)))) ))

(defun currying (f x1 &optional (x2 :nouse))
  (lambda (y1 &optional (y2 :nouse))
    (apply
      f
      (remove-if #'nouse-p (list x1 x2 y1 y2)) )))

(defun nouse-p (x) (equal x :nouse))

; -------------------------------------
(defun prime-p (n)
  (cond
    ((< n 2) nil)
    ((equal n 2) t)
    ((evenp n) nil)
    (t (every (currying (comp #'not #'factor-p) n) (seq 3 (floor (sqrt n)) 2))) ))

(defun seq (i-start i-end &optional (interval 1)(accum nil) )
  (if
    (< i-end i-start)
    accum
    (seq (+ i-start interval) i-end interval (cons i-start accum))))

(defun factor-p (n x) (zerop (mod n x)))

; -------------------------------------
(defun main ()
  ;(let ((a 1) (b 41) (n 0)) (format t "~A~%" (quadratic a b n)))
  ;(let ((a -79) (b 1601) (n 0)) (format t "~A~%" (quadratic a b n)))
  ;(let ((f (currying #'+ 1 -10))) (format t "~A~%" (apply f '(2))))
  ;(let ((a 1) (b 41)) (format t "~A~%" (gen-seq (currying #'quadratic a b) (comp #'not #'prime-p))))
  ;(let ((a 1) (b 41)) (format t "~A~%" (consecutive-primes a b)))
  ;(let ((a 1) (b 41)) (format t "~A~%" (length (gen-seq (currying #'quadratic a b) (comp #'not #'prime-p)))))
  (let ((limit-a 999) (limit-b 1000)) (format t "~A~%" (quadratic-primes limit-a limit-b)))
)

; -------------------------------------
(defun quadratic-primes (limit-a limit-b)
  (let
    ((accum nil))
    (do
      ((a (- limit-a) (1+ a)))
      ((> a limit-a) nil)
      (do
        ((b (- limit-b) (1+ b)))
        ((> b limit-b) nil)
        (let
          ((primes (consecutive-primes a b)))
          (if
            (< 10 (length primes))
            (setf accum (cons (list a b (length primes) (* a b)) accum))
            nil ))
      ))
    (sort accum #'> :key #'third)
  ))

(defun consecutive-primes (a b)
  (gen-seq
    (currying #'quadratic a b)
    (comp #'not #'prime-p) ))

(defun gen-seq (generator stop-cond &optional (i 0) (accum nil))
  (let
    ((v (apply generator (list i))))
    (if
      (apply stop-cond (list v))
      accum
      (gen-seq generator stop-cond (1+ i) (cons v accum)) )))

(defun quadratic (a b n)
  (+ (* n n) (* a n) b))

(main)

