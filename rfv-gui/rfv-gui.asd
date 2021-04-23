(defsystem "rfv-gui"
  :version "0.1.0"
  :author " "
  :license " "
;;  :build
  :depends-on ("qtools" "qtcore" "qtgui" "random-function-visualization" "iterate" "file-select")
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "rfv-gui-exec"
  :entry-point "rfv-gui:main"
  :components ((:file "kdialog")
	       (:file "rfv-gui")))
