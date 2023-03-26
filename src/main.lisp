(defpackage yrmadis2/main
  (:use :cl :3d-vectors :3d-matrices)
  (:local-nicknames
   (:sphere :yrmadis2/sphere)
   (:camera :yrmadis2/camera)
   (:cubemap :yrmadis2/cubemap)
   (:shaders :yrmadis2/shaders)
   (:sdl2 :sdl2/all)
   (:gl :cl-opengl))
  (:export
   #:main))

(in-package yrmadis2/main)

(defparameter *width* 800)
(defparameter *height* 600)
(defvar *window* nil)
(defvar *gl* nil)
(defvar *sphere* nil)
(defvar *camera* nil)
(defvar *cubemap* nil)
(defvar *shader* nil)

(defun init ()
  (sdl2:init '(:everything))
  ;; anti-aliasing
  (sdl2:gl-set-attribute :multisamplebuffers 1)
  (sdl2:gl-set-attribute :multisamplesamples 4)
  (sdl2:gl-set-attribute :accelerated-visual 1)
  (setf *window* (sdl2:create-window "Yrmadis" 0 0 *width* *height* '(:opengl)))
  (setf *gl* (sdl2:gl-create-context *window*))
  (sdl2:gl-make-current *window* *gl*)
  (gl:enable :depth-test)
  (gl:enable :multisample)
  (setf *camera* (camera:camera 0 0 3))
  (setf *sphere* (sphere:load-sphere (sphere:make-sphere 5)))
  (setf *shader* (make-instance 'shaders:opengl-shader
                                :vert "src/glsl/model-view-projection.vert"
                                :frag "src/glsl/cubemap.frag"))
  (setf *cubemap* (make-instance 'cubemap::cubemap
                                 :posx "./textures/earth/posx.png"
                                 :posy "./textures/earth/posy.png"
                                 :posz "./textures/earth/posz.png"
                                 :negx "./textures/earth/negx.png"
                                 :negy "./textures/earth/negy.png"
                                 :negz "./textures/earth/negz.png"))
  (values))

(defun cleanup ()
  (sdl2:gl-make-current *window* *gl*)
  (gl:delete-texture (slot-value *cubemap* 'cubemap::texture))
  (gl:delete-buffers (list *sphere*))
  (gl:delete-program (slot-value *shader* 'shaders::program))
  (sdl2:gl-delete-context *gl*)
  (sdl2:gl-reset-attributes)
  (sdl2:destroy-window *window*)
  (values))

(defparameter *walk-speed* 0.1)

(defun render ()
  (when (find :up *pressed-down*)
    (camera:walk *camera* :z (- *walk-speed*)))
  (when (find :down *pressed-down*)
    (camera:walk *camera* :z (+ *walk-speed*)))
  (when (find :left *pressed-down*)
    (camera:walk *camera* :x (+ *walk-speed*)))
  (when (find :right *pressed-down*)
    (camera:walk *camera* :x (- *walk-speed*)))
  (gl:viewport 0 0 *width* *height*)
  (gl:clear-color 0.1 0.1 0.1 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program (slot-value *shader* 'shaders::program))
  (gl:bind-texture :texture-cube-map (slot-value *cubemap* 'cubemap::texture))
  (gl:uniform-matrix-4fv (gl:get-uniform-location
                          (slot-value *shader* 'shaders::program)
                          "model")
                         (marr
                          (m*
                           (mtranslation (vec3 0.0 0 0))
                           (mscaling (vec3 0.8 0.8 0.8))
                           (mrotation +vx+ (camera::rad 30))
                           (mrotation +vy+ (let ((time (sdl2:get-ticks)))
                                             (* 0.03 (camera::rad time)))))))
  (let ((view (camera:view-matrix *camera*))
        (projection (mperspective (slot-value *camera* 'camera::zoom)
                                  (/ *width* *height*)
                                  0.1 100.0)))
    (gl:uniform-matrix-4fv (gl:get-uniform-location
                            (slot-value *shader* 'shaders::program)
                            "view")
                           (marr view))
    (gl:uniform-matrix-4fv (gl:get-uniform-location
                            (slot-value *shader* 'shaders::program)
                            "projection")
                           (marr projection)))
  (gl:bind-vertex-array *sphere*)
  (gl:polygon-mode :front-and-back :fill)
  (gl:draw-arrays :triangles 0 (* 3 (length (sphere:make-sphere 5))))
  (gl:bind-vertex-array 0)
  (gl:bind-texture :texture-cube-map 0)
  (gl:use-program 0)
  (sdl2:gl-swap-window *window*)
  (values))

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

(defgeneric handle-event (event-type event))

(defmethod handle-event ((event-type t) event)
  (format t "Ignoring event of type ~A~%" event-type)
  (values))

(defvar *pressed-down* nil
  "Buttons currently pressed down")

(defmethod handle-event ((event-type (eql :keyup)) event)
  (let* ((keysym (cffi:foreign-slot-pointer
                  event
                  '(:struct sdl2:keyboard-event)
                  'sdl2:keysym))
         (keycode (cffi:foreign-slot-value
                   keysym
                   '(:struct sdl2:keysym)
                   'sdl2/keyboard::keycode)))
    (alexandria:removef *pressed-down* keycode)
    (format t "~A~%" *pressed-down*)
    (values)))

(defmethod handle-event ((event-type (eql :keydown)) event)
  (let* ((keysym (cffi:foreign-slot-pointer
                  event
                  '(:struct sdl2:keyboard-event)
                  'sdl2:keysym))
         (keycode (cffi:foreign-slot-value
                   keysym
                   '(:struct sdl2:keysym)
                   'sdl2/keyboard::keycode)))
    (pushnew keycode *pressed-down*)
    (format t "~A~%" *pressed-down*)
    (values)))

(defmethod handle-event ((event-type (eql :quit)) event)
  (invoke-restart 'quit))

(defun handle-input ()
  (let ((event (sdl2:make-event)))
    (while (plusp (sdl2:poll-event event))
      (let ((event-type (cffi:mem-ref event 'sdl2:event-type)))
        (handle-event event-type event)))))
 

(defun main ()
  (init)
  (unwind-protect
       (loop
        (restart-case
            (progn
              (handle-input)
              (render)
              (sleep 1/60))
          (quit ()
            (return))))
    (cleanup)))
     
                  
