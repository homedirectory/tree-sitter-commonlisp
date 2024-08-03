(defun greet (a b c)
  (declare (number x))
  "greets you"
  t ; true
  nil #| 123 #| 321 |# |#
  (print "hello world"))

(defun fun (a b &rest args &optional (x 1) &key (k1 3) &aux (w 1)))
(defun fun (a &optional (x 1) &rest args &key (k1 3)))
(defun fun (a &optional (x 1) &rest args &key (k1 3) &allow-other-keys))
(defun fun (a &aux x (b 3)))
(defun (setf fun) (v x) v)

(defmacro destr (a (b (c &optional x)) &optional ((o))))

(destructuring-bind (a (a &optional b ((&key k x)))))

(defmacro generate-stuff (a &whole b &body body &environment env)
  "doc"
  123)

(defmethod add-em :around ((x integer) (y (eql 1)))
  (1+ x))

(funcall #'+ 1 2)

(setf vec #(1 2 3))
(setf vec #5(1 2 3))
(setf bitvec #22*10101010)

#:abc

#.123

(setf path #p"/home/123")

(setf arr1 #1A(1 2 3))

'(1 . 2)
'(a b c)

`(1 ,2 ,@(3 4) ,.(3 4))

(+ 123 #x123 #b111 #o777)

pkg:sym
:keyword

(pkg:fun 1 2 (:key 1))

((lambda (x y) (+ x y)) 1 2)

#'(lambda (x y) (+ x y))

(let ((x 5) (y 3)) (+ x y))
(let* ((x 5) (y 3)) (+ x y))

(defvar x 10 "hello")
(defvar none)
(defparameter *x* 100 "world")

(let () 3)

(setf ** 'huh)

'(a b c)

;; struct
#s(man joe 32)

(+ #C(2 1) #c(1 2)) ; complex

(destructuring-bind (x (y &rest ys)) '(1 (2)) (+ x y))

(defclass a:b (a) (x (y :type (or string number))))

(defstruct a b)

(defun list (a b c list) list)
(let ((list x)))
(list 1)
#'list

; comments inside forms
(let ((a ;what
        1)
      ; h
      (b #||# 2)))

(with-slots (x (y my-slot)) nil
  (print (+ x y))
  (list 1 2 x y))
