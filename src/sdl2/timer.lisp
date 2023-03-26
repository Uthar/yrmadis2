(defpackage sdl2/timer
  (:use :cl)
  (:local-nicknames
   (:c :cffi))
  (:export
   #:get-ticks
   #:get-ticks-64))

(in-package sdl2/timer)

(c:defcfun ("SDL_GetTicks" get-ticks) :uint32)

(c:defcfun ("SDL_GetTicks64" get-ticks-64) :uint64)
