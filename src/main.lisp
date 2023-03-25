(defpackage yrmadis2/main
  (:use :cl)
  (:local-nicknames
   (:sdl2 :sdl2/all)
   (:gl :cl-opengl)))

(in-package yrmadis2/main)

(defvar *window* nil)
(defvar *gl* nil)

(defun init ()
  (setf *window* (sdl2:create-window "Yrmadis" 0 0 800 600 '(:opengl)))
  (setf *gl* (sdl2:gl-create-context *window*)))

(defun cleanup ()
  (sdl2:gl-delete-context *gl*)
  (sdl2:destroy-window *window*))

(defun render ()
  (gl:clear-color 0.2 0.6 0.6 1.0)
  (gl:clear :color-buffer-bit)
  (sdl2:gl-swap-window *window*))

(defun main ()
  (init)
  (unwind-protect
       (loop
        (restart-case
            (progn
              (render)
              (sleep 1/60))
          (quit ()
            (return))))
    (cleanup)))
     
                  
