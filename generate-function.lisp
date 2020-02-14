(ql:quickload 'opticl)
(use-package :opticl)

(defmacro defweightedrandom (name &body body)
  (let ((total (loop for form in body sum (car form)))
	(val (gensym)))
    `(defun ,name ()
       (let ((,val (random ,total)))
	 (cond 
	   ,@(loop for form in body
		   for x = (car form) then (+ x (car form))
		   collect `((> ,x ,val) ,(cadr form))))))))

(defun w-square (x) (* x x))
(defun root (x) (sqrt (abs x)))
(defun w-mod (x y) (mod x (if (= y 0) 2 y)))
(defun divide (x y) (/ x (if (= y 0) (+ 1 (random 300) )y)))

(defun w-or (x y) (logorc1 (floor x) (floor y)))
(defun w-and (x y) (logand (floor x) (floor y)))
(defun w-nor (x y) (lognor (floor x) (floor y)))
(defun w-xor (x y) (logxor (floor x) (floor y)))
(defun w-gcd (x y) (gcd (ceiling x) (ceiling y)))
(defun w-lcm (x y) (lcm (ceiling x) (ceiling y)))
(defun w-log (x) (if (= 0 x) 0 (log (abs x))))
(defun w-cosh (x) (cosh (w-log  x)))
(defun l2-norm (x y a b) (sqrt (+ (expt (- x a) 2) (expt (- y b) 2))))

(defweightedrandom random-func
  (8 '+)
  (8 '-)
  (8 '*)
  (1 'root)
  (1 'w-log)
  (6 'w-mod)
  (3 'divide)
  (4 'w-or)
  (2 'w-and)
  (1 'w-nor)
  (8 'w-xor)
  (2 ' w-gcd)
  (2 ' w-lcm)
  (1 'sin)
  (2 'w-square)
					; (10 'atan)
  (5 'w-cosh)
  (6 'l2-norm))

(defweightedrandom rand-val
  (1 (random-int))
  (5 (rand-exp))
  (3 (rand-sym)))

(defweightedrandom rand-sym
  (2 'x)
  (2 'y)
  (1 'c))

(defun random-value ()
  (if (= (random 2) 0)
      (random-int)
      (rand-exp)))

(defun random-int ()
  (+ 1(random 999)))

(defun random-float ()
  (random 999.0))

(defun random-number ()
  "return random int or float"
  (if (equal (random 2) 0)
      (random-int)
      (random-float)))

(defparameter max-exps 25)
(defparameter cur-exps 0)

(defun rand-exp ()
  (if (> cur-exps max-exps)
      (random-value)
      (progn
	(setq cur-exps (+ 1 cur-exps))
	(let ((func (random-func)))
	  (cond
	    ((find func '(sin w-square sqrt root w-log atan w-cosh))
	     (list func (rand-val)))
	    ((find func '(l2-norm))
	     (list func (rand-val) (rand-val) (rand-val) (rand-val)))
	    (t (list func (rand-val) (rand-val))))))))

(defun rand-root ()
  (setq cur-exps 0)
  (list 'lambda '(x y c) (rand-exp)))

(defun gen-img (width height)
  (let* ((root (rand-root))
	 (func (eval root))
	 (img (opticl:make-8-bit-gray-image height width)))
    (print root)
    (loop for x below width do
      (loop for y below height do
	(setf (opticl:pixel img y x)   
	      (mod (floor (funcall func (+ 1 x) (+ y 1))) 256))))
    (opticl:write-png-file "./output/out.png" img)))

(defun gen-rgb-img (width height)
  (let* ((root (rand-root))
	 (func (eval root))
	 (img (opticl:make-8-bit-rgb-image height width)))
    (print root)
    (loop for x below width do
      (loop for y below height do
	(setf (opticl:pixel img y x)
	      (8-bit-rgb-wrapper func x y))))
    (opticl:write-png-file "./output/out.png" img)))

(defun recover-img (width height function)
  (let* ((func (eval function))
	 (img (opticl:make-8-bit-rgb-image height width)))
    (print function)
    (loop for x below width do
      (loop for y below height do
	(setf (opticl:pixel img y x)
	      (8-bit-rgb-wrapper func x y))))
    (opticl:write-png-file "./output/out.png" img)))

(defun 8-bit-rgb-wrapper (func x y)
  (values (8-bit-wrapper func x y 3)
	  (8-bit-wrapper func x y 9)
	  (8-bit-wrapper func x y 80)))

(defun 8-bit-wrapper (func x y c)
  (mod (floor (funcall func (+ x 1) (+ y 1) c)) 256))
