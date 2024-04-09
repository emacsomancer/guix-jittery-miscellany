(define-module (awesomejit packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages lua)
  #:use-module (awesomejit packages lua)
  #:use-module (gnu packages wm))

(define-public stumpwm
    (let ((commit "c802c7edd6323c9e768dd9383e3813a71aa8d59b")
        (revision "0"))
      (package
        ;; (inherit awesome)
             (name "stumpwm")
             (version (git-version "19.11.51.g2948b7c" revision commit))
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stumpwm/stumpwm")
                      (commit commit)))
                (sha256
                 (base32 "0ycis7519d7znyaa2x9dxgs0asfr4w7xsb34lcifygnlwnz11hpm"))))
  ;; (package
  ;;   (name "stumpwm")
  ;;   (version "22.11")
  ;;   (source
  ;;    (origin
  ;;      (method git-fetch)
  ;;      (uri (git-reference
  ;;            (url "https://github.com/stumpwm/stumpwm")
  ;;            (commit version)))
  ;;      (file-name (git-file-name "stumpwm" version))
  ;;      (sha256
  ;;       (base32 "1wxgddmkgmpml44a3m6bd8y529b13jz14apxxipmij10wzpgay6d"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiasco
           texinfo

           ;; To build the manual.
           autoconf
           automake))
    (inputs
     (list sbcl-alexandria
           sbcl-cl-ppcre
           sbcl-clx))
    (outputs '("out" "lib"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* "stumpwm-tests.asd"
                (("\"ALL-TESTS\"")
                 "\"RUN-PACKAGE-TESTS\" :package"))))
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append (assoc-ref outputs "out") "/bin/stumpwm")
               outputs
               #:entry-program '((stumpwm:stumpwm) 0))))
          (add-after 'build-program 'create-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/stumpwm.desktop")
                  (lambda (file)
                    (format file
                       "[Desktop Entry]~@
                        Name=stumpwm~@
                        Comment=The Stump Window Manager~@
                        Exec=~a/bin/stumpwm~@
                        TryExec=~@*~a/bin/stumpwm~@
                        Icon=~@
                        Type=Application~%"
                       out))))))
          (add-after 'install 'install-manual
            (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (info (string-append out "/share/info")))
                (invoke "./autogen.sh")
                (invoke "sh" "./configure" "SHELL=sh")
                (apply invoke "make" "stumpwm.info" make-flags)
                (install-file "stumpwm.info" info))))
          (add-after 'install-manual 'remove-temporary-cache
            (lambda* (#:key outputs #:allow-other-keys)
              (delete-file-recursively (string-append (assoc-ref outputs "lib")
                                                      "/.cache")))))))
    (synopsis "Window manager written in Common Lisp")
    (description
     "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
    (home-page "https://github.com/stumpwm/stumpwm")
    (license license:gpl2+)
    (properties `((cl-source-variant . ,(delay cl-stumpwm)))))))


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

(define-public awesome-next-luajit-tip
  ;; Requires commit 353ccfb0dde0dc5c264595f9d17e47a23c82bcdf or later, because
  ;; we need the -DLUA_EXECUTABLE flag.
  (package
    (inherit awesome-next)
    (name "awesome-next-luajit-tip")
    (inputs (modify-inputs (package-inputs awesome-next)
              (replace "lua" luajit-tip)
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
