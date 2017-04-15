(cl:in-package #:lispos-interaction)

(defclass object ()
  ((%creation-date
    :initform nil
    :initarg :creation-date
    :reader creation-date)
   (%modification-date
    :accessor modification-date)))

(defmethod initialize-instance :after ((object object) &key)
  (setf (modification-date object)
        (creation-date object)))



