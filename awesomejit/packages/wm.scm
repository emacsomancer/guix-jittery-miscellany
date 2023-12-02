(define-module (awesomejit packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages lua)
  ;; #:use-module (awesomejit packages lua)
  #:use-module (gnu packages wm))

(define-public awesome-next
  (let ((commit "375d9d723550023f75ff0066122aba99fdbb2a93")
        (revision "0"))
    (package (inherit awesome)
             (name "awesome-next")
             (version (git-version "4.3" revision commit))
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/awesomeWM/awesome")
                      (commit commit)))
                (sha256
                 (base32 "0ycis7519d7znyaa2x9dxgs0asfr4w7xsb34lcifygnlwnz11hpm"))))
             (arguments
              (substitute-keyword-arguments
               (package-arguments awesome)
               ((#:configure-flags flags)
                ;; lua-ldoc output is non-deterministic, so disable it.
                `(cons "-DGENERATE_DOC=off" ,flags))))
             (inputs (modify-inputs (package-inputs awesome)
                                    ;; Remove ldoc since we're not using it.
                                    (delete "lua-ldoc")
                                    (prepend librsvg))))))

(define-public awesome-next-luajit
  ;; Requires commit 353ccfb0dde0dc5c264595f9d17e47a23c82bcdf or later, because
  ;; we need the -DLUA_EXECUTABLE flag.
  (package
    (inherit awesome-next)
    (name "awesome-next-luajit")
    (inputs (modify-inputs (package-inputs awesome-next)
              (replace "lua" luajit)
              (replace "lua-lgi" lua5.1-lgi)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments awesome-next)
       ((#:configure-flags flags ''())
        `(cons* (string-append "-DLUA_LIBRARY="
                 (assoc-ref %build-inputs "lua") "/lib/libluajit-5.1.so")
          (string-append "-DLUA_INCLUDE_DIR="
           (assoc-ref %build-inputs "lua") "/include/luajit-2.1/")
          (string-append "-DLUA_EXECUTABLE="
           (assoc-ref %build-inputs "lua") "/bin/luajit")
          ,flags))
       ((#:modules modules)
        `((ice-9 string-fun) ,@modules))
       ((#:phases phases)
        ;; grandparent awesome package looks directly at 'lua' package instead of
        ;; its input, which causes it to set paths with 5.3 in them rather than
        ;; 5.1, so we patch those.
        `(modify-phases ,phases
          (add-before 'configure 'set-luajit-paths
           (lambda* (#:key inputs #:allow-other-keys)
            (setenv "LUA_PATH"
             (string-replace-substring (getenv "LUA_PATH") "5.3" "5.1"))
            (setenv "LUA_CPATH"
              (string-replace-substring (getenv "LUA_CPATH") "5.3" "5.1"))))
          (replace 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((awesome (assoc-ref outputs "out"))
                   (cairo (string-append (assoc-ref inputs "cairo") "/lib"))
                   (lua-version "5.1") ; luajit corresponds to lua5.1
                   (lua-lgi (assoc-ref inputs "lua-lgi")))
             (wrap-program (string-append awesome "/bin/awesome")
              `("LUA_PATH" ";" suffix
                (,(format #f "~a/share/lua/~a/?.lua" lua-lgi lua-version)))
              `("LUA_CPATH" ";" suffix
                (,(format #f "~a/lib/lua/~a/?.so" lua-lgi lua-version)))
              `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
              `("LD_LIBRARY_PATH" suffix (,cairo)))
             #t)))))))))

;; (define-public awesome-next-luajit-master
;;   ;; Requires commit 353ccfb0dde0dc5c264595f9d17e47a23c82bcdf or later, because
;;   ;; we need the -DLUA_EXECUTABLE flag.
;;   (package
;;     (inherit awesome-next)
;;     (name "awesome-next-luajit-master")
;;     (inputs (modify-inputs (package-inputs awesome-next)
;;               (replace "lua" luajit-tip)
;;               (replace "lua-lgi" lua5.1-lgi)))
;;     (arguments
;;      (substitute-keyword-arguments
;;          (package-arguments awesome-next)
;;        ((#:configure-flags flags ''())
;;         `(cons* (string-append "-DLUA_LIBRARY="
;;                  (assoc-ref %build-inputs "lua") "/lib/libluajit-5.1.so")
;;           (string-append "-DLUA_INCLUDE_DIR="
;;            (assoc-ref %build-inputs "lua") "/include/luajit-2.1/")
;;           (string-append "-DLUA_EXECUTABLE="
;;            (assoc-ref %build-inputs "lua") "/bin/luajit")
;;           ,flags))
;;        ((#:modules modules)
;;         `((ice-9 string-fun) ,@modules))
;;        ((#:phases phases)
;;         ;; grandparent awesome package looks directly at 'lua' package instead of
;;         ;; its input, which causes it to set paths with 5.3 in them rather than
;;         ;; 5.1, so we patch those.
;;         `(modify-phases ,phases
;;           (add-before 'configure 'set-luajit-paths
;;            (lambda* (#:key inputs #:allow-other-keys)
;;             (setenv "LUA_PATH"
;;              (string-replace-substring (getenv "LUA_PATH") "5.3" "5.1"))
;;             (setenv "LUA_CPATH"
;;               (string-replace-substring (getenv "LUA_CPATH") "5.3" "5.1"))))
;;           (replace 'wrap
;;            (lambda* (#:key inputs outputs #:allow-other-keys)
;;             (let* ((awesome (assoc-ref outputs "out"))
;;                    (cairo (string-append (assoc-ref inputs "cairo") "/lib"))
;;                    (lua-version "5.1") ; luajit corresponds to lua5.1
;;                    (lua-lgi (assoc-ref inputs "lua-lgi")))
;;              (wrap-program (string-append awesome "/bin/awesome")
;;               `("LUA_PATH" ";" suffix
;;                 (,(format #f "~a/share/lua/~a/?.lua" lua-lgi lua-version)))
;;               `("LUA_CPATH" ";" suffix
;;                 (,(format #f "~a/lib/lua/~a/?.so" lua-lgi lua-version)))
;;               `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
;;               `("LD_LIBRARY_PATH" suffix (,cairo)))
;;              #t)))))))))
