(ql:quickload '(:weblocks :weblocks-ui :find-port))


(in-package :cl-user)
(defpackage player
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
           #:render
           #:update
           #:defwidget)
  (:import-from #:weblocks/actions
           #:make-js-action)
  (:import-from #:weblocks/app
           #:defapp))

(in-package player)


(defvar *base-dir*)

(setf *base-dir* "~/music")

(defapp player :prefix "/")



(defwidget recording ()
        ((file
          :initarg :file
          :accessor file)))

(defun make-recording (file)
        (make-instance 'recording :file file))

(defmethod play ((recording recording))
   (ignore-errors     
     (uiop:run-program "killall mpg123"))
   (let
     ((command (format nil "mpg123 '~a'" (namestring (file recording)))))
     ;((command (format nil "mpg123 '/home/jajis/music/salsa/La celosa.mp3'")))
     (format t "WILL RUN: ~a~%" command)
     (uiop:run-program command)))
   ;(update recording))


(defwidget recording-list ()
   ((recordings
     :initarg :recordings
     :accessor recordings)))

(defmethod render ((w recording))
  (with-html
    (:div
      (:button
        :onclick (make-js-action
                   (lambda (&key &allow-other-keys)
                           (play w)))
        "Play")
      (:span (namestring (file w))))))

(defmethod render ((w recording-list))
  (with-html
        (:h1 "Available recordings:")
        (:ul
          (loop for recording in (recordings w) do
                (:li (render recording))))))

(defun make-recording-list (rest)
  (let ((recordings (loop for file in rest
                       collect (make-recording file))))
    (make-instance 'recording-list :recordings recordings)))


(directory (format nil "~a~a" *base-dir* "/*/*.mp3"))

(mapcar #'file-namestring (directory (format nil "~a~a" *base-dir* "/*/*.mp3")))


(defmethod weblocks/session:init ((app player))
   (declare (ignorable app))
   (make-recording-list))


(defmethod weblocks/session:init ((app player))
   (declare (ignorable app))
   (make-recording-list
     (directory (format nil "~a~a" *base-dir* "/*/*.mp3"))))

(weblocks/debug:on)
(defvar *port*)

;(setf *port* (find-port:find-port))

(setf *port* 9999)

(weblocks/server:start :port *port*)

;(weblocks/debug:reset-latest-session)
