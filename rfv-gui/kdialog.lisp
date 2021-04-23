#|
This file is an add-on to the file-select package, adapted from zenity.lisp
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Original Author: Nicolas Hafner <shinmera@tymoon.eu>
File Author: Andrew Valentine
|#

(defpackage #:org.shirakumo.file-select.kdialog
  (:use #:cl #:org.shirakumo.file-select)
  (:export #:kdialog))

(in-package #:org.shirakumo.file-select.kdialog)

(defclass kdialog (backend)
  ())

(defmethod finalize ((backend kdialog)))

(defun kdialog (&rest args)
  (uiop:run-program (list* "kdialog" (remove NIL args))
                    :output :string :external-format :utf-8))

(defun kdialog* (&key title default filter multiple save backend)
  (declare (ignore backend))
  (handler-case
      (let ((parts (org.shirakumo.file-select::split
                    #\Linefeed
                    (apply #'kdialog
			   (if (eq filter :directory) "--getexistingdirectory"
			       (if save "--getsavefilename"
				   "--getopenfilename"))
			   (format nil "~a" (if default (namestring default) "./"))
			   (format NIL "--title=~a" title)
                           (when multiple "--multiple")
			   (list (make-filter filter))
			   ;;this is a load-bearing (list) call
			   ;;I don't know why it's required. But it is.
			   ;;Other arguments can be strings.
			   ;;If this one isn't then SBCL freaks out with an incomprehensible
                           ;;type error. kwalitee
                           ))))
        (cond ((null parts) (values NIL NIL))
              (multiple (values parts T))
              (T (values (first parts) T))))
    (error ()
      (values NIL NIL)) 
    ))

(defun make-filter (filter)
  (etypecase filter
    ((eql :directory))
    (string (format nil "~a (*.~a)" filter filter))
    (list (format nil "~{~{~a (*.~a)~}~^|~}" filter))))

(defmethod new-with ((backend kdialog) &rest args)
  (apply #'kdialog* :save T args))

(defmethod existing-with ((backend kdialog) &rest args)
  (apply #'kdialog* args))
