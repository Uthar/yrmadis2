(defpackage sdl2/video
  (:use :cl)
  (:local-nicknames
   (:c :cffi))
  (:export
   #:get-num-video-displays
   #:get-display-name
   #:create-window
   #:destroy-window
   #:gl-create-context
   #:gl-delete-context
   #:gl-swap-window
   #:gl-make-current
   #:gl-reset-attributes
   #:gl-set-attribute))

(in-package sdl2/video)

(c:defbitfield window-flags
  (:fullscreen  #x00000001)
  (:opengl      #x00000002)
  (:shown       #x00000004)
  (:hidden      #x00000008)
  (:borderless  #x00000010)
  (:resizable   #x00000020)
  (:minimized   #x00000040)
  (:maximized   #x00000080)
  (:mouse-grabbed  #x00000100)
  (:input-focus    #x00000200)
  (:mouse-focus    #x00000400)
  (:fullscreen-desktop #x00001001)
  (:foreign        #x00000800)
  (:allow-highdpi  #x00002000)
  (:mouse-capture  #x00004000)
  (:always-on-top  #x00008000)
  (:skip-taskbar   #x00010000)
  (:utility        #x00020000)
  (:tooltip        #x00040000)
  (:popup-menu     #x00080000)
  (:keyboard-grabbed  #x00100000)
  (:vulkan  #x10000000)
  (:metal   #x20000000)
  )

(c:defcenum gl-attr
  :red-size
  :green-size
  :blue-size
  :alpha-size
  :buffer-size
  :doublebuffer
  :depth-size
  :stencil-size
  :accum-red-size
  :accum-green-size
  :accum-blue-size
  :accum-alpha-size
  :stereo
  :multisamplebuffers
  :multisamplesamples
  :accelerated-visual
  :retained-backing
  :context-major-version
  :context-minor-version
  :context-egl
  :context-flags
  :context-profile-mask
  :share-with-current-context
  :framebuffer-srgb-capable
  :context-release-behavior
  :context-reset-notification
  :context-no-error
  :floatbuffers)
    
(c:defcfun ("SDL_GetNumVideoDisplays" get-num-video-displays) :int)

(c:defcfun ("SDL_GetDisplayName" get-display-name) :string
  (display-index :int))

(c:defcfun ("SDL_CreateWindow" create-window) :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags window-flags))

(c:defcfun ("SDL_DestroyWindow" destroy-window) :void
  (window :pointer))

(c:defcfun ("SDL_GL_ResetAttributes" gl-reset-attributes) :void)

(c:defcfun ("SDL_GL_SetAttribute" gl-set-attribute) :int
  (attr gl-attr)
  (value :int))

(c:defcfun ("SDL_GL_CreateContext" gl-create-context) :pointer
  (window :pointer))

(c:defcfun ("SDL_GL_DeleteContext" gl-delete-context) :void
  (context :pointer))

(c:defcfun ("SDL_GL_SwapWindow" gl-swap-window) :void
  (window :pointer))

(c:defcfun ("SDL_GL_MakeCurrent" gl-make-current) :int
  (window :pointer)
  (context :pointer))
