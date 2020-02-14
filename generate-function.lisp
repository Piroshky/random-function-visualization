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

(defun square (x) (* x x))
(defun root (x) (sqrt (abs x)))
(defun w-mod (x y) (mod x (if (= y 0) 2 y)))
(defun divide (x y) (/ x (if (= y 0) (+ 1 (random 300) )y)))

(defun w-or (x y) (logorc1 (floor x) (floor y)))
(defun w-and (x y) (logand (floor x) (floor y)))
(defun w-nor (x y) (lognor (floor x) (floor y)))
(defun w-xor (x y) (logxor (floor x) (floor y)))
(defun w-gcd (x y) (gcd (ceiling x) (ceiling y)))
(defun w-lcm (x y) (lcm (ceiling x) (ceiling y)))
(defun w-cosh (x) (cosh (log x)))

(defweightedrandom random-func
    (4 '+)
    (2 '-)
    (2 '*)
    (7 'root)
    (1 'log)
    (6 'w-mod)
    (3 'divide)
    (1 'w-or)
    (2 'w-and)
    (1 'w-nor)
    (2 'w-xor)
    (2 ' w-gcd)
    (2 ' w-lcm)
    (1 'sin)
    (5 'square)
    (10 'atan)
    (10 'w-cosh))

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
      (random-number)
      (random-exp)))

(defun random-int ()
  (+ 1(random 999)))

(defun random-float ()
  (random 999.0))

(defun random-number ()
  "return random int or float"
  (if (equal (random 2) 0)
      (random-int)
      (random-float)))

(defun rand-exp ()
  (let ((func (random-func)))
    (if (find func '(sin square sqrt root log atan cosh))
	(list func (rand-val))
	(list func (rand-val) (rand-val)))))

(defun rand-root ()
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
    (opticl:write-png-file "~/Desktop/out.png" img)))

(defun gen-rgb-img (width height)
  (let* ((root (rand-root))
	 (func (eval root))
	 (img (opticl:make-8-bit-rgb-image height width)))
    (print root)
    (loop for x below width do
      (loop for y below height do
	(setf (opticl:pixel img y x)
	      (8-bit-rgb-wrapper func x y))))
    (opticl:write-png-file "~/Desktop/out.png" img)))

(defun 8-bit-rgb-wrapper (func x y)
  (values (8-bit-wrapper func x y 3)
	  (8-bit-wrapper func x y 9)
	  (8-bit-wrapper func x y 80)))

(defun square (x) (* x x))

(defun 8-bit-wrapper (func x y c)
  (mod (floor (funcall func (+ x 1) (+ y 1) c)) 256))
