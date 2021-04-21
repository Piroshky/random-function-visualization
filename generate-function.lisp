(defpackage :random-function-visualization
  (:nicknames :rfv)
  (:use #:cl+qt)
  (:export #:gen-img
	   #:create-form
	   #:qt-gen-rgb-img
	   #:qt-recover-rgb-img
	   #:qt-load-rgb-img))
(in-package :random-function-visualization)
(in-readtable :qtools)

(defparameter *function-save-file* "functions"
  "file where functions will be saved to ")

;; Macro that lets you create functions that will randomly return
;; items depending on their weighting.
;;
;; See random-func, other random-___ functions for examples
(defmacro defweightedrandom (name params &body body)
  (let ((total (loop for form in body sum (car form)))
	(val (gensym))
	(rs (make-random-state t)))
    `(defun ,name ,params
       (let ((,val (random ,total (make-random-state t))))
	 (cond 
	   ,@(loop for form in body
		   for x = (car form) then (+ x (car form))
		   collect `((> ,x ,val) ,(cadr form))))))))

;; Functions definitions for use in the generated AST.
;; Mostly these are modified to prevent from evaluating to negative
;; numbers.
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
(defun euler-totient(x)(loop for i from 1 to (floor x) if (=(gcd (floor x) i)1)sum 1))
(defun int-sum (n) (/ (* n (+ n 1)) 2))

;; (x 'func-name) x is number of parameters func-name takes
(defweightedrandom random-func ()
  (5 '(2 +))
  (2 '(2 -))
  (5 '(2 *))
  (2 '(1 abs))
  (1 '(1 root))
  (1 '(1 w-log))
  (2 '(2 w-mod))
  (4 '(2 divide))
  (1 '(2 w-or))
  (1 '(2 w-and))
  (1 '(2 w-nor))
  (1 '(2 w-xor))
  (1 '(2 w-gcd))
  (1 '(2 w-lcm))
  (2 '(1 sin))
  (2 '(1 cos))
  (3 '(1 w-square))                                        
  (1 '(1 w-cosh))
  (1 '(1 tanh))
  (2 '(4 l2-norm))
  (2 '(2 min))
  (2 '(2 max))
  (2 '(1 int-sum))
  (2 'if)
  )

(defweightedrandom rand-value ()
  (2 (random-int))
  (1 (random-float))
  (5 (rand-exp))
  (4 (rand-sym)))

(defweightedrandom rand-sym ()
  (6 'x)
  (6 'y)
  (3 'r)
  (4 'theta)
  (5 'c))

(defun n= (a b)
  (not (= a b)))

(defweightedrandom rand-comparison-op ()
  (3 '>)
  (2 '<)
  (1 '=)
  (1 'n=))

(defun nand (a b)
  (not (and a b)))

(defun implies (a b)
  (or (not a) b))

(defun xor (a b)
  (not (eq a b)))

(defweightedrandom rand-logic-op ()
  (2 'and)
  (2 'or)
  (1 'eq)
  (2 'nand)
  (2 'xor))

(defweightedrandom rand-bool-exp (depth)
  (1 (rand-logic-exp depth))
  (2 (rand-comparison-exp)))

(defun rand-comparison-exp ()
  `(,(rand-comparison-op) ,(rand-value) ,(rand-value)))

(defun rand-logic-exp (depth)
  `(,(rand-logic-op) ,(limited-rand-bool-exp depth) ,(limited-rand-bool-exp depth)))

(defun limited-rand-bool-exp (depth)
  (if (> depth 10)
      (rand-comparison-exp)
      (rand-bool-exp (+ depth 1))))

(defun rand-if ()
  `(if ,(limited-rand-bool-exp 10)
       ,(rand-value)
       ,(rand-value)))

(defun random-int ()
  (+ 1(random 255)))

(defun random-float ()
  (random 999.0))

(defun random-number ()
  "return random int or float"
  (if (equal (random 2) 0)
      (random-int)
      (random-float)))

(defparameter max-exps 45)
(defparameter cur-exps 0)

(defun rand-exp ()
  (if (> cur-exps max-exps)
      (random-int)
      (let ((func-info (random-func)))
	(incf cur-exps)
	(cond ((listp func-info)
	       (destructuring-bind (num-params func-sym) func-info
		 
		 (let ((ret (list func-sym)))
		   (loop for i from 1 to num-params
			 do (nconc ret (list (rand-value))))
		   ret)))
	      ((eq 'if func-info)
	       (rand-if)
	       )))))

(defun rand-root ()
  (setq cur-exps 0)
  (list 'lambda '(x y r theta c) (rand-exp)))

(defun gen-img (width height &key (auto-save nil) (save-dir "./output/") (img-color :rgb))
  (make-random-state t)
  (ensure-directories-exist save-dir)
  (case img-color
    (:rgb (gen-rgb-img width height :auto-save auto-save :save-dir save-dir))
    ((:grey :gray) (gen-grey-img width height :auto-save auto-save :save-dir save-dir))
    (otherwise (error "~S unknown img-color type, chose :rgb or :grey" img-color))))

;;; gen-grey-img and gen-rgb-img are nearly the same, I figure it's
;;; easier to have seperate functions than having a case in the setf
;;; for the current pixel, which would cause a lot of needless
;;; computation if it doesn't get boiled out by the compiler.

(defun gen-grey-img (width height &key (auto-save nil) (save-dir "./output/"))
  (multiple-value-bind (root func) (generate-function)
    (let* ((img (opticl:make-8-bit-gray-image height width))
	   (birth (timestamp))
	   (image-path (concatenate 'string save-dir birth ".png"))
	   (c (random 255)))
      (print root)
      (loop for x below width do
	(loop for y below height do
	  (setf (opticl:pixel img y x)   
		(8-bit-wrapper func (coords x y height width) c))))
      (opticl:write-png-file image-path img)
      (if (or auto-save (y-or-n-p (format nil "save function `~a`?" birth)))
	  (with-open-file (str (concatenate 'string save-dir *function-save-file*)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)
	    (format str "~s~%~%"
		    (list birth :grey c root)))
	  (delete-file image-path)))))
 
(defun gen-rgb-img (width height &key (auto-save nil) (save-dir "./output/"))
  ;; Generates and 8bit rgb picture
  (multiple-value-bind (root func) (generate-function)
    (let* ((img (opticl:make-8-bit-rgb-image height width))
	   (birth (timestamp))
	   (image-path (concatenate 'string save-dir birth ".png"))
	   (r (random 255))
	   (g (random 255))
	   (b (random 255)))
      (unless auto-save
	(format t "~a~%(~a ~a ~a)" birth r g b)
	(print root))
      (loop for x below width do
	(loop for y below height do
	  (setf (opticl:pixel img y x)
		(8-bit-rgb-wrapper func (coords x y height width) r g b))))
      (opticl:write-png-file image-path img)
      (if (or auto-save (y-or-n-p (format nil "save function `~a`?" birth)))
	  (with-open-file (str (concatenate 'string save-dir *function-save-file*)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)
	    (format str "~s~%~%"
		    (list birth
			  :8-rgb (list r g b)
			  root)))
	  (delete-file image-path)))))

(defmacro match-bind (pattern object &body body)
  `(loop with ,pattern = ,object
         while nil
         finally (return (progn ,@body))))

(defun qt-gen-rgb-img (width height)
  ;; Generates and 8bit rgb picture
  (make-random-state t)
  (multiple-value-bind (root func) (generate-function)
    (let* ((img (q+:make-qimage width height (q+::qimage.format_rgb888)))
	   (birth (timestamp))
	   (r (random 255))
	   (g (random 255))
	   (b (random 255)))
      (loop for x below width do
	(loop for y below height do
	  (setf (q+:pixel img x y)
		(qt-888-wrapper func (coords x y height width) r g b))))
      (let ((*package* (find-package :random-function-visualization))
	    (form (create-form birth :8-rgb r g b root width height)))
	(list img
	      form
	      (format nil "~(~s~)~%~%" form)
	      birth
	      (format nil "~(~s~)" (nth 3 form))
	      )))))

(defun qt-recover-rgb-img (form)
  ;; returns the same values as qt-gen-rgb-img using `form'.
  ;; presumably the rgb/size values have been altered so a new time stamp is generated
  (match-bind (nil nil (r g b) root (width height)) form 
    (let ((img (q+:make-qimage width height (q+::qimage.format_rgb888)))
	  (func (eval root))
	  (birth (timestamp)))
      (loop for x below width do
	(loop for y below height do
	  (setf (q+:pixel img x y)
		(qt-888-wrapper func (coords x y height width) r g b))))
      (let ((*package* (find-package :random-function-visualization))
	    (form (create-form birth :8-rgb r g b root width height)))
	(list img form (format nil "~(~s~)~%~%" form) birth (format nil "~(~s~)" (nth 3 form)))))))

(defun qt-load-rgb-img (form)
  ;; returns the same values as qt-gen-rgb-img using `form'.
  (match-bind (birth color-type (r g b) root (width height)) form 
    (let ((img (q+:make-qimage width height (q+::qimage.format_rgb888)))
	  (func (eval root)))
      (loop for x below width do
	(loop for y below height do
	  (setf (q+:pixel img x y)
		(qt-888-wrapper func (coords x y height width) r g b))))
      (let ((*package* (find-package :random-function-visualization))
	    (form (create-form birth color-type r g b root width height)))
	(list img form (format nil "~(~s~)~%~%" form) birth)))))


(defun create-form (name-string color-type r g b root width height)
   (list name-string color-type (list r g b) root (list width height)))


(defun recover-img (width height info &key (save-dir "./recovered/"))
  (ensure-directories-exist save-dir)
  (destructuring-bind (name color-type channel-value function) info
    (cond ((equal :8-RGB color-type) (recover-rgb-img width height name channel-value function save-dir)))))

(defun recover-rgb-img (width height name channel-value function save-dir)
  (destructuring-bind (r g b) channel-value
    (let* ((func (eval function))
	   (img (opticl:make-8-bit-rgb-image height width)))
      (loop for x below width do
	(loop for y below height do
	  (setf (opticl:pixel img y x)
		(8-bit-rgb-wrapper func (coords x y height width) r g b))))
      (opticl:write-png-file (concatenate 'string save-dir
					  (write-to-string width) "-"
					  (write-to-string height) "-" name ".png")
			     img))))
      

;; This is noticeably faster than doing (q+:rgb (q+:qcolor-from-rgb ...))
(defun qt-888-wrapper (func coords r g b)
  (+ 4278190080
   (ash (8-bit-wrapper func coords r) 16)
   (ash (8-bit-wrapper func coords g) 8)
   (8-bit-wrapper func coords b)))


(defun 8-bit-rgb-wrapper (func coords r g b)
  (values (8-bit-wrapper func coords r)
	  (8-bit-wrapper func coords g)
	  (8-bit-wrapper func coords b)))

(defun 8-bit-wrapper (func coords c)
  (destructuring-bind (x y r theta) coords
    (mod (floor (funcall func (+ x 1) (+ y 1) r theta c)) 256)))

(defparameter 16-rgb-r 500)
(defparameter 16-rgb-g 300)
(defparameter 16-rgb-b 630)
(defun 16-bit-rgb-wrapper (func x y)
  (values (16-bit-wrapper func x y 16-rgb-r)
	  (16-bit-wrapper func x y 16-rgb-g)
	  (16-bit-wrapper func x y 16-rgb-b)))

(defun 16-bit-wrapper (func x y c)
  (mod (floor (funcall func (+ x 1) (+ y 1)  c)) 65536))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun timestamp()
  (local-time:format-timestring nil (local-time:now) :format '(:year "-" (:month 2) "-" (:day 2) "::" (:hour 2) ":" (:min 2) ":" (:sec 2) ":" (:msec 3))))

(defun rand-function ()
  (let* ((root (rand-root))
	 (func (eval root)))
    (values root func)))

(defun generate-root ()
  "returns a root that passes the requirements decided in validate-root"
  (let ((root (rand-root)))
    (if (validate-function root)
	root
	(generate-root))))

(defun generate-function ()
  (let ((root (generate-root)))
    (values root (eval root))))

;; (defun generate-function ()
;;   (handler-case (rand-function)
;;     (style-warning (c) (generate-function)))) 

(defun gen-batch (batch-size height width &key (save-dir "./output/") (img-color :rgb))
  (loop for i from 1 upto batch-size do
    (progn 
      (print i)
      (gen-img height width :auto-save t :save-dir save-dir :img-color img-color))))

(defun coords (x y height width)
  (destructuring-bind (r theta) (polar-coords x y height width)
    (list x y r theta)))

;; cartesian to polar coordinates
(defun polar-coords (x y height width)
  (let ((c-x (- x (/ width 2)))
	(c-y (- y (/ height 2))))
    (list (sqrt (+ (expt c-x 2) (expt c-y 2)))
	  (atan c-y c-x))))

(defun validate-function (func)
  (let ((body (third func)))
    (if (find-in-tree 'c body)
	(<= 2 (loop for sym in '(x y r theta) counting (find-in-tree sym body)))
	nil)
    ))

;; taken from stack overflow
(defun find-in-tree (item tree &key (test #'eql))                             
  (labels ((find-in-tree-aux (tree)                                           
	     (cond ((funcall test item tree)                                  
		    (return-from find-in-tree tree))                          
		   ((consp tree)                                              
		    (find-in-tree-aux (car tree))                             
		    (find-in-tree-aux (cdr tree))))))                         
    (find-in-tree-aux tree)))
