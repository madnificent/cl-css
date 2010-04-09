(asdf:defsystem :cl-css
  :name "cl-css"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Common lisp CSS generator"
  :depends-on ()
  :components ((:file "packages")
	       (:file "base" :depends-on ("packages"))))
