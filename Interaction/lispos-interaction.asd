(cl:in-package #:asdf-user)

(defsystem lispos-interaction
  :depends-on (:mcclim)
  :components
  ((:file "packages")
   (:file "object-types")))
