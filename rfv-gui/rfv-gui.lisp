(defpackage #:rfv-gui
  (:use #:cl+qt)
  (:export #:main))
(in-package :rfv-gui)
(in-readtable :qtools)

(defun merge-pathnames-to-native (base-directory file-string)
  (uiop:native-namestring
   (merge-pathnames base-directory file-string)))

(define-widget main-window (QWidget) ())

(defun basename (filepath-string)
  "returns the file's name without type extension"
  (nth-value 2 (uiop:split-unix-namestring-directory-components (uiop:split-name-type filepath-string))))

(defun normalize-form (form width height)
  "Adds width and height as older serilizations of forms didn't include them"
  (if (= (length form) 5)
      form
      (append form (list (list  width height)))))

(defun find-matching-form (form-name function-file-directory)
  (let ((*package* (find-package :random-function-visualization)))
  (iterate:iterate
    (iterate:for form in-file
		 (merge-pathnames-to-native function-file-directory "functions"))
    (iterate:finding form such-that (string= form-name (car form))))))

(defvar *save-dir* (uiop:parse-native-namestring (sb-posix:getcwd) :ensure-directory t))
(defvar *function-save-file* "functions")
(defvar *current-form*)
(defvar *current-form-string*)
(defvar *current-function-name*)
(defvar *current-function-string*)
(defvar *settings-modified* nil)
(defvar *text-modified* nil)

(define-subwidget (main-window width-spinbox) (q+:make-qspinbox main-window)  
  (setf (q+:minimum width-spinbox) 10)
  (setf (q+:maximum width-spinbox) 100000)
  (setf (q+:value width-spinbox) 500)
  (setf (q+:single-step width-spinbox) 10)
  (q+:set-fixed-size width-spinbox 70 30))

(define-subwidget (main-window height-spinbox) (q+:make-qspinbox main-window)
  (setf (q+:minimum height-spinbox) 10)
  (setf (q+:maximum height-spinbox) 100000)
  (setf (q+:value height-spinbox) 500)
  (setf (q+:single-step height-spinbox) 10)
  (q+:set-fixed-size height-spinbox 70 30))



(define-widget spinbox-slider (QWidget) () )

(define-subwidget (spinbox-slider spinbox) (q+:make-qspinbox spinbox-slider)
  (setf (q+:minimum spinbox) 0)
  (setf (q+:maximum spinbox) 255)
  (q+:set-fixed-size spinbox 53 30)
  )
(define-subwidget (spinbox-slider slider) (q+:make-qslider (Q+::QT.HORIZONTAL) spinbox-slider)
  (setf (q+:minimum slider) 0)
  (setf (q+:maximum slider) 255)
  (setf (q+:focus-policy slider) (Q+::QT.NO-FOCUS))
  )
(define-subwidget (spinbox-slider layout) (q+:make-qhboxlayout spinbox-slider)
  (q+:add-widget layout spinbox)
  (q+:add-widget layout slider))

(define-initializer (spinbox-slider setup-signals)
  (connect! spinbox (value-changed int) slider (set-value int))
  (connect! slider (value-changed int) spinbox (set-value int))
  )

(define-slot (spinbox-slider set-value) ((value int))
  (setf (q+:value spinbox) value)
  )

(define-subwidget (main-window red-spinbox-slider) (make-instance 'spinbox-slider))
(define-subwidget (main-window green-spinbox-slider) (make-instance 'spinbox-slider))
(define-subwidget (main-window blue-spinbox-slider) (make-instance 'spinbox-slider))


;; (define-subwidget (main-window red-slider) (q+:make-qslider main-window)
;;   (setf (q+:minimum red-slider) 0)
;;   (setf (q+:maximum red-slider) 255))

(define-subwidget (main-window save-dir-combobox) (q+:make-qcombobox main-window)
  (q+:add-item save-dir-combobox (uiop:native-namestring *save-dir*))
  (setf (q+:focus-policy save-dir-combobox) (Q+::QT.NO-FOCUS))
  ;; (setf (q+:editable save-dir-combobox) t) ;; If we make the combobox editable we need to validate that the path exists
  )

(define-subwidget (main-window browse-button) (q+:make-qpushbutton "Browse..." main-window)
  (q+:set-fixed-size browse-button 100 30)
  (setf (q+:focus-policy browse-button) (Q+::QT.NO-FOCUS)))

(define-subwidget (main-window generate-button) (q+:make-qpushbutton "&Generate" main-window)
  (setf (q+:shortcut generate-button) (q+:make-qkeysequence "G"))
  (q+:set-fixed-size generate-button 100 30)
  (setf (q+:focus-policy generate-button) (Q+::QT.NO-FOCUS)))
(define-subwidget (main-window save-button) (q+:make-qpushbutton "&Save" main-window)
  (setf (q+:shortcut save-button) (q+:make-qkeysequence "S"))
  (q+:set-fixed-size save-button 100 30)
  (setf (q+:focus-policy save-button) (Q+::QT.NO-FOCUS)))

(define-subwidget (main-window redraw-button) (q+:make-qpushbutton "&Redraw" main-window)
  (setf (q+:shortcut redraw-button) (q+:make-qkeysequence "R"))
  (q+:set-fixed-size redraw-button 100 30)
  (setf (q+:focus-policy redraw-button) (Q+::QT.NO-FOCUS)))

(define-subwidget (main-window load-button) (q+:make-qpushbutton "&Load Text" main-window)
  (setf (q+:shortcut load-button) (q+:make-qkeysequence "L"))
  (q+:set-fixed-size load-button 100 30)
  (setf (q+:focus-policy load-button) (Q+::QT.NO-FOCUS)))

(define-subwidget (main-window open-button) (q+:make-qpushbutton "&Open Image" main-window)
  (setf (q+:shortcut open-button) (q+:make-qkeysequence "O"))
  (q+:set-fixed-size open-button 100 30)
  (setf (q+:focus-policy open-button) (Q+::QT.NO-FOCUS)))

(define-subwidget (main-window function-name-label) (q+:make-qlabel "None" main-window))

(define-widget text-edit (QTextEdit) ())
(define-initializer (text-edit startup)
  (setf (q+:minimum-width text-edit) 100))
;; (define-override (text-edit key-press-event) (ev)
;;   (if)
;;   )

(define-subwidget (main-window text) (make-instance 'text-edit))



(define-signal (main-window startup-signal) ())
(define-signal (main-window draw-signal) ())

(define-initializer (main-window startup)
  (setf (q+:window-title main-window) "Random Function Visualization")
  (setf (q+:window-icon main-window) (q+:make-qicon "/home/drew/.quicklisp/local-projects/image-test/icon.png"))
  (setf (q+:focus-policy main-window) (q+:qt.strong-focus))
  (signal! main-window (startup-signal)))

(define-subwidget (main-window image-label) (q+:make-qlabel)
  ;; (setf (q+:pixmap image-label) (q+:make-qpixmap "./download.png"))
  (let* ((width (q+:value width-spinbox))
	 (height (q+:value height-spinbox))    
	 (my-image (q+:make-qimage width height (q+:qimage.format_rgb32))))

    (q+:fill my-image 0)
    (setf (q+:pixmap image-label) (q+:qpixmap-from-image my-image))))


(define-slot (main-window settings-modified) ()
  (declare (connected (slot-value red-spinbox-slider 'spinbox) (value-changed int)))
  (declare (connected (slot-value green-spinbox-slider 'spinbox) (value-changed int)))
  (declare (connected (slot-value blue-spinbox-slider 'spinbox) (value-changed int)))
  (declare (connected width-spinbox (value-changed int)))
  (declare (connected height-spinbox (value-changed int)))
  (setf *settings-modified* t))

(define-slot (main-window text-changed) ()
  (declare (connected text (text-changed)))
  (setf *text-modified* t))

(define-slot (main-window generate-new-image) () 
  (declare (connected generate-button (pressed)))
  (declare (connected main-window (startup-signal)))

  (let ((width (q+:value width-spinbox))
	(height (q+:value height-spinbox)))
    (destructuring-bind (my-image form form-str name function-str) (rfv:qt-gen-rgb-img width height)
      (setf *current-form* form)
      (setf (q+:pixmap image-label) (q+:qpixmap-from-image my-image))
      (q+:repaint image-label)

      (setf *current-form-string* form-str)
      (setf *current-function-name* name)
      (setf *current-function-string* function-str)

      (setf (q+:text text) function-str)
      (setf (q+:text function-name-label) name)
      (destructuring-bind (r g b) (nth 2 *current-form*)
	(setf (q+:value (slot-value red-spinbox-slider 'spinbox)) r)
	(setf (q+:value (slot-value green-spinbox-slider 'spinbox)) g)
	(setf (q+:value (slot-value blue-spinbox-slider 'spinbox)) b))))
  (setf *settings-modified* nil)
  (setf *text-modified* nil)
  (setf (q+:enabled save-button) t))

(define-slot (main-window load-text-form) ()
  (declare (connected load-button (pressed)))

  (let* ((read-form (with-input-from-string
		  (in (q+:to-plain-text text))
		(let ((*package* (find-package :random-function-visualization)))
		  (read in))))
	 (form (if (eq (car read-form) 'lambda)
		   (rfv::create-form
		    (rfv::timestamp)
		    :8-rgb
		    (q+:value (slot-value red-spinbox-slider 'spinbox))
		    (q+:value (slot-value green-spinbox-slider 'spinbox))
		    (q+:value (slot-value blue-spinbox-slider 'spinbox))
		    read-form
		    (q+:value width-spinbox) (q+:value height-spinbox))
		   read-form)))

    (destructuring-bind (my-image form form-str name) (rfv:qt-load-rgb-img form)
      (setf *current-form* form)
      (setf (q+:pixmap image-label) (q+:qpixmap-from-image my-image))
      (q+:repaint image-label)

      (setf *current-form-string* form-str)
      (setf *current-function-name* name)

      (setf (q+:text text) form-str)
      (setf (q+:text function-name-label) name)
      (destructuring-bind (r g b) (nth 2 *current-form*)
	(setf (q+:value (slot-value red-spinbox-slider 'spinbox)) r)
	(setf (q+:value (slot-value green-spinbox-slider 'spinbox)) g)
	(setf (q+:value (slot-value blue-spinbox-slider 'spinbox)) b)))
    )
  (setf *settings-modified* nil)
  (setf *text-modified* nil)
  (setf (q+:enabled save-button) t))
  
(define-slot (main-window open-img-file) ()
  (declare (connected open-button (pressed)))

  (let ((file-string (uiop:run-program
		      (list "kdialog" "--getopenfilename"
			    (uiop:native-namestring *save-dir*) "PNG Image File (*.png)")
		      :output :string :external-format :utf-8 :ignore-error-status t)
		     
	  ;; (org.shirakumo.file-select:existing
		     ;;  :title "Choose Image" :default *save-dir*)
	  ;; (q+::qfiledialog-get-open-file-name
	  ;; 	      main-window "Choose Image" (uiop:native-namestring *save-dir*)
	  ;; 	      "Images (*.png *.xpm *.jpg)")
	  ))
    (when (string/= file-string "")
      (let ((found-form (find-matching-form (basename file-string) (directory-namestring file-string))))
	;; (when (eq found-form nil) (alert "could not find ~s in file ~s~%"))
	(let ((form (normalize-form found-form (q+:value width-spinbox) (q+:value height-spinbox))))
	  (destructuring-bind (my-image load-form form-str name) (rfv:qt-load-rgb-img form)
	    (setf *current-form* load-form)
	    (setf (q+:pixmap image-label) (q+:qpixmap-from-image my-image))
	    (q+:repaint image-label)
 
	    (setf *current-form-string* form-str)
	    (setf *current-function-name* name)

	    (setf (q+:text text) form-str)
	    (setf (q+:text function-name-label) name)
	    (destructuring-bind (r g b) (nth 2 *current-form*)
	      (setf (q+:value (slot-value red-spinbox-slider 'spinbox)) r)
	      (setf (q+:value (slot-value green-spinbox-slider 'spinbox)) g)
	      (setf (q+:value (slot-value blue-spinbox-slider 'spinbox)) b)))
	)
     
      ))))

(define-slot (main-window set-save-dir) ()
  (declare (connected browse-button (pressed)))
  (let* ((file-string (q+:qfiledialog-get-existing-directory
		       main-window "Save World"
		       (uiop:native-namestring *save-dir*)))
	 (save-dir (uiop:directory-exists-p (uiop:parse-native-namestring file-string :ensure-directory t))))
    ;; if you hit cancel in qfiledialog it returns ""
    ;; and (directory-exists-p "") doesn't return nil, it returns the package's directory.
    ;; This seems obviously incorrect to me.
    ;; So check both file-string and save-dir.
    ;; This way if someone accidentally hits the browse button it doesn't unexpectedly change
    ;; the save directory.
    (when (and (string/= file-string "") save-dir)
      (setf (q+:enabled save-button) t)
      (setf *save-dir* save-dir)
      (let* ((dir-string (uiop:native-namestring *save-dir*)))
	(if (= (q+:find-text save-dir-combobox dir-string) -1)
	    (q+:add-item save-dir-combobox dir-string))
	(setf (q+:current-index save-dir-combobox) (q+:find-text save-dir-combobox dir-string))))))

(define-slot (main-window redraw-image) ()
  (declare (connected redraw-button (pressed)))

  (let* ((width (q+:value width-spinbox))
	 (height (q+:value height-spinbox))
	 (r (q+:value (slot-value red-spinbox-slider 'spinbox)))
	 (g (q+:value (slot-value green-spinbox-slider 'spinbox)))
	 (b (q+:value (slot-value blue-spinbox-slider 'spinbox)))
	 (func (nth 3 *current-form*))
	 (form (rfv:create-form
		*current-function-name*
		:8-rgb r g b
		func
		width height)))
    (destructuring-bind (my-image new-form form-string name function-str) (rfv:qt-recover-rgb-img form)
      (setf (q+:pixmap image-label) (q+:qpixmap-from-image my-image))
      (q+:repaint image-label)
      
      (when *settings-modified*
	(setf (q+:enabled save-button) t)
	(setf *settings-modified* nil)
	(setf *current-form-string* form-string)
	(setf *current-function-name* name)
	(setf (q+:text function-name-label) name)))))

(define-slot (main-window save-image) ()
  (declare (connected save-button (pressed)))
  (q+:save
   (q+:pixmap image-label)
   (merge-pathnames-to-native *save-dir* (concatenate 'string *current-function-name* ".png"))
   )
  (with-open-file (file (merge-pathnames-to-native *save-dir* *function-save-file*)
			:direction :output
			:if-exists :append
			:if-does-not-exist :create)
    (format file "~a" *current-form-string*))
  (setf (q+:enabled save-button) nil)
  (setf (q+:text function-name-label) (concatenate 'string *current-function-name* " (saved)")))


(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (flet ((make-right-aligned-label (text)
	   (let ((label (q+:make-qlabel text main-window)))
	     (setf (q+:alignment label) (q+::qt.align-right))

	     label)))

    (let ((save-layout (q+:make-qhboxlayout))
	  (save-label (q+:make-qlabel "Save in directory:")))
      (setf (q+:alignment save-label) (q+::qt.align-right))
      (q+:set-fixed-size save-label 120 30)
      ;; (Q+:SET-SIZE-POLICY save-label (q+:qsizepolicy.minimum) (q+:qsizepolicy.minimum))
      (q+:add-widget save-layout save-label)
      (q+:add-widget save-layout save-dir-combobox)

      (q+:add-widget save-layout browse-button)
      
      (q+:add-layout layout save-layout))
    
    (let ((button-layout (q+:make-qhboxlayout)))
      (q+:add-stretch button-layout)
      (q+:add-widget button-layout generate-button)
      (q+:add-widget button-layout save-button)
      (q+:add-widget button-layout redraw-button)
      (q+:add-widget button-layout load-button)
      (q+:add-widget button-layout open-button)
      (q+:add-stretch button-layout)
      (q+:add-layout layout button-layout))

    (let ((settings-layout (q+:make-qhboxlayout)))
      
      (let ((size-settings-groupbox (q+:make-qgroupbox "image size"))
	    (size-settings-layout (q+:make-qformlayout)))
	(setf (q+:label-alignment size-settings-layout) (Q+:QT.ALIGN-RIGHT ))
	(q+:add-row size-settings-layout "width:" width-spinbox)
	(q+:add-row size-settings-layout "height:" height-spinbox)      
	(setf (q+:layout size-settings-groupbox) size-settings-layout)
	(q+:add-widget settings-layout size-settings-groupbox))

      (let ((image-settings-groupbox (q+:make-qgroupbox "color channel values"))
	    (image-settings-layout (q+:make-qformlayout)))
	(setf (q+:label-alignment image-settings-layout) (Q+:QT.ALIGN-RIGHT ))

	(q+:add-row image-settings-layout "red:" red-spinbox-slider)
	(q+:add-row image-settings-layout "green:" green-spinbox-slider)
	(q+:add-row image-settings-layout "blue:" blue-spinbox-slider)
	(setf (q+:layout image-settings-groupbox) image-settings-layout)
	(q+:add-widget settings-layout image-settings-groupbox))
      
      (q+:add-layout layout settings-layout)
      )    

    (let ((image-layout (q+:make-qhboxlayout)))
      ;; (q+:add-stretch image-layout)
      (q+:add-widget image-layout image-label)
      (q+:add-widget image-layout text)
      ;; (q+:add-stretch image-layout)
      (q+:add-layout layout image-layout ))

    (let ((name-label-layout (q+:make-qhboxlayout)))
      (q+:add-stretch name-label-layout)
      (q+:add-widget name-label-layout function-name-label)
      (q+:add-stretch name-label-layout)
      (q+:add-layout layout name-label-layout ))))

(defun main () 
  (with-main-window (window 'main-window)))
