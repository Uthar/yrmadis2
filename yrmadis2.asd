(defsystem yrmadis2
  :class :package-inferred-system
  :pathname "src"
  :depends-on (cl-opengl
               3d-quaternions
               sdl2/all
               yrmadis2/all))
