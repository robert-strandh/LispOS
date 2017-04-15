(cl:in-package #:lispos-interaction)

(clim:define-application-frame browser ()
  ((%object-type :initform 'object :accessor object-type))
  (:panes (main :application
                :scroll-bars nil
                :display-function 'display-store)
          (inter :interactor
                 :scroll-bars nil))
  (:layouts (default (clim:vertically ()
                       (clim:scrolling (:width 500
                                        :height 500
                                        :scroll-bars :vertical)
                         main)
                       (clim:scrolling (:width 500
                                        :height 100
                                        :scroll-bars :vertical)
                         inter)))))

(defun display-store (frame pane)
  (loop for object in *store*
        when (typep object (object-type frame))
          do (clim:present object (type-of object) :stream pane)
             (format pane "                                                   ")
             (terpri pane)))

(defun browser ()
  (clim:run-frame-top-level (clim:make-application-frame 'browser)))

(defun bla ()
  (clim:run-frame-top-level (clim:make-application-frame 'bla)))

(define-browser-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-browser-command (filter :name t) ((type class))
  (clim:with-application-frame (frame)
    (setf (object-type frame) type)))
