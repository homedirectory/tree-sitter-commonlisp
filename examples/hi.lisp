(defun greet (a b c)
  (declare (number x))
  "greets you"
  t ; true
  nil #| 123 #| 321 |# |#
  (print "hello world"))

(defun fun (a &rest args &optional (x 1) &key (k1 3)))
(defun fun (a &optional (x 1) &rest args &key (k1 3)))
(defun fun (a &optional (x 1) &rest args &key (k1 3) &allow-other-keys))
(defun fun (a &aux x (b 3)))

(funcall #'+ 1 2)

(setf vec #(1 2 3))
(setf vec #5(1 2 3))
(setf bitvec #22*10101010)

#:abc

#.123

(setf path #p"/home/123")

(setf arr1 #1A(1 2 3))

'(1 . 2)

`(1 ,2 ,@(3 4) ,.(3 4))

(+ 123 #x123 #b111 #o777)

pkg:sym
:keyword

((lambda (x y) (+ x y)) 1 2)

#'(lambda (x y) (+ x y))

(let ((x 5) (y 3)) (+ x y))
(let* ((x 5) (y 3)) (+ x y))

(defvar x 10 "hello")
(defparameter *x* 100 "world")

(setf ** 'huh)

'(a b c)

;; struct
#s(man joe 32)
