(defsystem yrmadis2
  :class :package-inferred-system
  :pathname "src"
  :depends-on (cl-opengl
               sdl2/all
               yrmadis2/all))
