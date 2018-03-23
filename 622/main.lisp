
(require 'common)
(use-package 'common)

(defun main ()
  ;(format t "~A~%" (reduce #'(lambda (i accum) (cons (riffle-shuffle (first accum)) accum)) (seq 1 6) :from-end t :initial-value (list (seq 1 10))))
  ;(format t "~A~%" (take 5 (seq 1 10)))
  ;(format t "~A~%" (drop 5 (seq 1 10)))
  ;(format t "~A~%" (remove-if-not #'(lambda (n-sn) (equal (second n-sn) 8)) (mapcar #'(lambda (n) (list n (s n))) (seq 2 1000 2))))
  ;(let ((limit 60)) (format t "~A~%" (remove-if-not #'(lambda (n-sn) (equal (second n-sn) limit)) (mapcar #'wrapper (seq 2 1000 2)))))
  ;(for-each
  ;  (seq 2 100 2)
  ;  #'(lambda (i)
  ;      (format t "~A: ~A~%" i (s i))))

  ;(format t "~A~%" (initial-array-p #(0 1 2 3)))
  ;(format t "~A~%" (initial-array-p #(0 1 2 4)))
  ;(let ((n 4)) (format t "s(~A): ~A~%" n (next-array (make-array n :initial-contents (seq 0 (1- n))))))
  ;(let ((n 6)) (format t "s(~A): ~A~%" n (next-array (make-array n :initial-contents (seq 0 (1- n))))))
  ;(let ((n 4)) (format t "s(~A): ~A~%" n (s-2 n)))
  ;(let ((n 6)) (format t "s(~A): ~A~%" n (s-2 n)))
  ;(let ((limit 8)) (format t "~A~%" (remove-if-not #'(lambda (n-sn) (equal (second n-sn) limit)) (mapcar #'wrapper-2 (seq 2 1000 2)))))
  ;(let
  ;  ((limit 60))
  ;  ;(with-open-file (output (make-pathname :name "result-2.log") :direction :output :if-exists :append)
  ;    (format t "~A~%"
  ;            (remove-if-not
  ;              #'(lambda (n-sn)
  ;                  (equal (second n-sn) limit))
  ;              ;(mapcar (p-apply #'wrapper-2 t) (seq 10000 100000 2)))
  ;              (mapcar (p-apply #'wrapper-2 t) (seq 2 1000 2)))
  ;    ;)
  ;    ))

  ;(let
  ;  ((limit 8))
  ;  (for-each
  ;    ;(remove-if-not
  ;    ;  #'(lambda (lst) (equal limit (length lst)))
  ;      (mapcar
  ;        #'shuffle-patterns-lst
  ;        (mapcar
  ;          #'(lambda (i) (seq 0 (1- i)))
  ;          (seq
  ;            2
  ;            (expt 2 limit)
  ;            2)))
  ;    ;)
  ;    #'(lambda (arr)
  ;        (format t "~A: ~A~%" (length (first arr)) (length arr))
  ;        (for-each
  ;          arr
  ;          #'(lambda (a)
  ;              (format t "~A~%" a))))))


  ;(let
  ;  ((limit 10))
  ;  (for-each
  ;    (seq 2 (expt 2 limit) 2)
  ;    #'(lambda (i)
  ;        (format t "~A: ~A~%" i (s-3 i)) )))

  ;(time
  ;(let
  ;  ((limit 6))
  ;  (let
  ;    ((lst (sn-list limit)))
  ;    (format t "~A: ~A, ~A~%" limit (sum lst) lst)))
  ;)

  ;(let
  ;  ((limit 13))
  ;  (let
  ;    ((lst (sn-list-2 limit)))
  ;    (format t "~A: ~A~%" limit lst)))

  (time
  (let*
    ((rot 60)
     (limit (expt 2 rot))
     (candidates (mapcar #'1+ (list-factors (1- limit))))
     (result (remove-if-not #'(lambda (x) (equal rot (s-3 x))) candidates))
    )
    (format t "~A: ~A~%" rot candidates)
    (format t "    ~A -> ~A~%" result (sum result))
  )
  )

  ;(time (format t "~A~%" (list-factors (expt 2 40))))

)

(defun list-factors (n)
  (let*
    ((limit (floor (sqrt n)))
     (lst (list-factors-under limit n)))
    (sort
      (remove-duplicates
        (append
          lst
          (mapcar #'(lambda (x) (/ n x)) lst) ))
      #'< )))
(defun list-factors-under (limit n)
  (do
    ((i 1 (1+ i))
     (accum nil))
    ((> i limit) accum)
    (if (zerop (mod n i))
      (setf accum (cons i accum))
      ()) ))

(defun list-factors-bak (n)
  (remove-if-not
    #'(lambda (x)
        (zerop (mod n x)))
    (seq 1 n)))

(defun sn-list-2 (rot-n)
  (do
    ((i 2 (+ 2 i))
     (limit (expt 2 rot-n))
     (accum nil))
    ((> i limit) accum)

    (let
      ((sss (s-list i)))
      (if (equal rot-n (length sss))
          (setf accum (cons (list i sss) accum))
          () ) )))

(defun s-list (n)
  (do
    ((pos (next-pos-2 n 1) (next-pos-2 n pos))
     (accum (list 1) (cons pos accum)))
    ((equal 1 pos) (reverse accum))
    () ))

(defun sn-list (rot-n)
  (do
    ((i 2 (+ 2 i))
     (limit (expt 2 rot-n))
     (accum nil))
    ((> i limit) accum)

    (if (equal rot-n (s-3 i rot-n))
       (setf accum (cons i accum))
       () ) ))

(defun s-3 (n &optional (limit nil))
  (do
    ((pos (next-pos-2 n 1) (next-pos-2 n pos))
     (accum 1 (1+ accum)))
    ((equal 1 pos) accum)
    (if
      (and
        (not (null limit))
        (> accum limit))
      (return nil)
      () )))

(defun next-pos-2 (n pos)
  (if (< pos (floor n 2))
    (* 2 pos)
    (- (* 2 pos) (- n 1)) ))

(defun wrapper-2 (output n)
  (let ((sn (s-2 n)))
    (format output "~A: ~A~%" n sn)
    (list n sn) ))

(defun transes (n)
  (let
    ((initial-array (make-array n :initial-contents (seq 0 (1- n)))))
    (do
      ((arr (next-array initial-array)
            (next-array arr))
       (i 1
          (1+ i))
       (accum (list initial-array) (cons arr accum)))
      ((initial-array-p arr) accum)
      nil)))

(defun s-2 (n)
  (let
    ((initial-array (make-array n :initial-contents (seq 0 (1- n)))))
    (do
      ((arr (next-array initial-array)
            (next-array arr))
       (i 1
          (1+ i)))
      ((initial-array-p arr) i)
      (if (> i 60) (return nil)))))

(defun next-array (arr)
  (let
    ((n (array-dimension arr 0)))
    (map
      'array
      (p-apply #'next-pos (floor n 2))
      arr)))

(defun next-pos (m p)
  (if
    (< p m)
    (* 2 p)
    (+ 1 (* 2 (- p m))) ))

(defun initial-array-p (arr)
  (dotimes
    (i (array-dimension arr 0) t)
    (if (not (equal i (aref arr i)))
        (return nil)
        nil )))

(defun wrapper (n)
  (let ((sn (s n)))
    (format t "~A: ~A~%" n sn)
    (list n sn) ))

(defun s (n)
  ;(length (shuffle-patterns (seq 1 n))) )
  (shuffle-patterns (seq 1 n)) )


(defun riffle-shuffle (lst)
  (let
    ((m (floor (length lst) 2)))
    (reduce
      #'append
      (mapcar
        #'list
        (take m lst)
        (drop m lst) ))))

;(defun shuffle-patterns (initial-state)
;  (do
;    ((next-state (riffle-shuffle initial-state)
;                 (riffle-shuffle next-state))
;     (accum (list initial-state)
;            (cons next-state accum)) )
;    ((equal next-state initial-state)
;     accum)
;    nil
;    ))

(defun shuffle-patterns (initial-state)
  (do
    ((i 1 (1+ i))
     (next-state (riffle-shuffle initial-state)
                 (riffle-shuffle next-state))
     ;(accum (list initial-state)
     ;       (cons next-state accum))
     (accum 1
            (1+ accum))
    )
    ((equal next-state initial-state)
     accum)
    nil
    ))

(defun shuffle-patterns-lst (initial-state)
  (do
    ((i 1 (1+ i))
     (next-state (riffle-shuffle initial-state)
                 (riffle-shuffle next-state))
     ;(accum (list initial-state)
     ;       (cons next-state accum))
     (accum nil
            (cons next-state accum))
    )
    ((equal next-state initial-state)
     (reverse (cons next-state accum)))
    ;(if (< 10 (length accum)) (return nil))
    ))

(main)

