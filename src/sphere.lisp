(defpackage yrmadis2/sphere
  (:use :cl :3d-vectors)
  (:export
   #:make-sphere
   #:load-sphere))

(in-package yrmadis2/sphere)

(defun split-triangle (triangle)
  (destructuring-bind (a b c)
      triangle
    (let ((x (v+ a (v* (v- b a) 1/2)))
          (y (v+ b (v* (v- c b) 1/2)))
          (z (v+ c (v* (v- a c) 1/2))))
      (list
       (list a x z)
       (list x b y)
       (list x z y)
       (list c y z)))))

(defun split-triangles (triangles)
  (apply 'concatenate 'list (map 'list 'split-triangle triangles)))

(defparameter *center* (vec3 0 0 0))

(defun normalize-triangle (triangle)
  (destructuring-bind (a b c)
      triangle
    (let ((x (vunit (v- a *center*)))
          (y (vunit (v- b *center*)))
          (z (vunit (v- c *center*))))
      (list x y z))))

(defclass sphere ()
  (precision
   triangles
   vao)
  (:default-initargs
   :precision 3))

(defmethod initialize-instance :after ((sphere sphere) &key precision)
  (let* ((triangles (make-sphere precision))
         (vao (load-sphere triangles)))
    (setf (slot-value sphere 'vao) vao
          (slot-value sphere 'triangles) (* (length triangles) 3)
          (slot-value sphere 'precision) precision)))

(defun make-sphere (precision)
  (loop repeat precision
        for triangles = (list (list (vec3 0 1 0)
                                    (vec3 1 0 -1)
                                    (vec3 1 0 1))
                              (list (vec3 0 1 0)
                                    (vec3 -1 0 1)
                                    (vec3 1 0 1))
                              (list (vec3 0 1 0)
                                    (vec3 -1 0 -1)
                                    (vec3 -1 0 1))
                              (list (vec3 0 1 0)
                                    (vec3 1 0 -1)
                                    (vec3 -1 0 -1))
                              (list (vec3 0 -1 0)
                                    (vec3 1 0 -1)
                                    (vec3 1 0 1))
                              (list (vec3 0 -1 0)
                                    (vec3 -1 0 1)
                                    (vec3 1 0 1))
                              (list (vec3 0 -1 0)
                                    (vec3 -1 0 -1)
                                    (vec3 -1 0 1))
                              (list (vec3 0 -1 0)
                                    (vec3 1 0 -1)
                                    (vec3 -1 0 -1)))
          then (split-triangles triangles)
        finally (return (map 'list 'normalize-triangle triangles))))

(defun load-sphere (triangles)
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (vertices (gl:alloc-gl-array :float (* 9 (length triangles)))))
    (unwind-protect
         (progn
           (gl:bind-vertex-array vao)
           (gl:bind-buffer :array-buffer vbo)
           (let ((index -1))
             (dolist (triangle triangles)
               (setf (gl:glaref vertices (incf index)) (vx (first triangle))
                     (gl:glaref vertices (incf index)) (vy (first triangle))
                     (gl:glaref vertices (incf index)) (vz (first triangle))
                     (gl:glaref vertices (incf index)) (vx (second triangle))
                     (gl:glaref vertices (incf index)) (vy (second triangle))
                     (gl:glaref vertices (incf index)) (vz (second triangle))
                     (gl:glaref vertices (incf index)) (vx (third triangle))
                     (gl:glaref vertices (incf index)) (vy (third triangle))
                     (gl:glaref vertices (incf index)) (vz (third triangle)))))
           (gl:buffer-data :array-buffer :static-draw vertices)
           (gl:vertex-attrib-pointer 0 3 :float nil
                                     (* (sb-alien:alien-size float :bytes) 3)
                                     (* (sb-alien:alien-size float :bytes) 0))
           (gl:enable-vertex-attrib-array 0)
           vao)
      (gl:free-gl-array vertices)
      (gl:bind-vertex-array 0)
      (gl:bind-buffer :array-buffer 0))))
