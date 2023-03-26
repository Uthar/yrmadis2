;; To start rendering the solar system:
;;
;; First, run direnv allow on a GNU/Linux system with Nix installed
;;
;; Then, run:
(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-system 'yrmadis2)
(yrmadis2/main:main)
