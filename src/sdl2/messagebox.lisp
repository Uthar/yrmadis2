(defpackage sdl2/messagebox
  (:use :cl)
  (:local-nicknames
   (:c :cffi))
  (:export
   :show-simple-message-box))

(in-package sdl2/messagebox)

(c:defcenum flags
  (:error #x10)
  (:warning #x20)
  (:information #x40)
  ;; (:buttons-left-to-right #x80)
  ;; (:buttons-right-to-left #x100)
  )

;; (c:defcenum button-flags
;;   (:button-returnkey-default #x1)
;;   (:button-escapekey-default #x2))

;; (c:defcstruct button-data
;;   (flags button-flags)
;;   (buttonid :int)
;;   (text :string))

;; (c:defcstruct color
;;   (r :uint8)
;;   (g :uint8)
;;   (b :uint8))

;; (c:defcenum color-type
;;   :background
;;   :text
;;   :button-border
;;   :button-background
;;   :button-selected
;;   :max)

;; (c:defcstruct color-scheme
;;   ;; (c:foreign-enum-value 'color-type :max)
;;   (colors (:struct color) :count 5))

;; (c:defcstruct data
;;   (flags :uint32)
;;   (window :pointer)
;;   (title :string)
;;   (message :string)
;;   (numbuttons :int)
;;   (buttons :pointer) ;; button-data
;;   (color-scheme :pointer) ;; color-scheme
;;   )

;; (c:defcfun ("SDL_ShowMessageBox" show-message-box) :int
;;   (messageboxdata :pointer) ;; data
;;   (buttonid :pointer) ;; int
;;   )

(c:defcfun ("SDL_ShowSimpleMessageBox" show-simple-message-box) :int
  (flags flags)
  (title :string)
  (message :string)
  (window :pointer))

;; (show-simple-message-box :information "test" "test" (c:null-pointer))
