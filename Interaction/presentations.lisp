(cl:in-package #:lispos-interaction)

(clim:define-presentation-method clim:present
    (object
     (type person)
     stream
     (view clim:textual-view)
     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "Person ~a" (name object)))

(clim:define-presentation-method clim:present
    (object
     (type text-document)
     stream
     (view clim:textual-view)
     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "Text ~a ~a"
          (name (author object))
          (substitute #\Space #\Newline (subseq (contents object) 0 30))))

(clim:define-presentation-method clim:present
    (object
     (type music)
     stream
     (view clim:textual-view)
     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "Music ~a ~a"
          (name (composer object))
          (name (performer object))))

