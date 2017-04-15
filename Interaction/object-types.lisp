(cl:in-package #:lispos-interaction)

(defclass object ()
  ((%creation-date
    :initform (get-universal-time)
    :initarg :creation-date
    :reader creation-date)
   (%modification-date
    :accessor modification-date)
   (%author
    :initarg :author
    :reader :author)))

(defmethod initialize-instance :after ((object object) &key)
  (setf (modification-date object)
        (creation-date object)))

(defclass sound (object)
  ())

(defclass music (sound)
  ((%composer :initarg :composer :reader composer)
   (%performer :initarg :performer :reader performer)))

(defclass person (object)
  ((%name :initarg :name :accessor name)))

(defclass text-document (object)
  ((%contents :initarg :contents :accessor contents)))

(defclass latex-source (text-document)
  ())

(defclass email-message (object)
  ())

(defclass source-code (text-document)
  ((%language :initarg :language :reader language)))
