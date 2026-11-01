;; SPDX-FileCopyrightText: 
;;;           © 2025 B Slade <slade@lambda-y.net>
;;

;;
;; SPDX-License-Identifier: GPL-3.0

(define-module (jitterymisc packages window-management)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (guix transformations)  
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages window-management)
  #:use-module ((gnu packages lisp) #:prefix gnu:)
  #:use-module (jitterymisc packages lisp)
  #:use-module (jitterymisc packages lisp-xyz)
  #:use-module (jitterymisc packages lua))

;; SBCL and Common Lisp package versions.

(define sbcl-latest "2.6.10")
(define sbcl-base
  (string-append "sbcl@" sbcl-latest))
(define sbcl-spec
  (string-append "sbcl=" sbcl-base))

(define clx-latest "0.7.8")
(define clx-base
  (string-append "sbcl-clx@" clx-latest))
(define clx-spec
  (string-append "sbcl-clx=" clx-base))

;; Package transformations.

;; (define with-sbcl
;;   (options->transformation
;;    `((with-input . ,sbcl-spec))))
(define with-sbcl
  (package-input-rewriting
   `((,gnu:sbcl . ,sbcl))))

(define with-clx
  (options->transformation
   `((with-input . ,clx-spec))))

;; Apply CLX first, then SBCL, recursively throughout the
;; package's dependency graph.

(define (with-lisp-stack package)
  (with-sbcl (with-clx package)))

;; Convenience function for independently installed Lisp packages.

(define (sbcl-package name)
  (with-lisp-stack
   (specification->package name)))

;; StumpWM upstream revision.

(define stumpwm-commit
  "902d5c214d1775a33e46db50f40dc5bc3cec22ff")

;; Untransformed StumpWM package definition.

(define stumpwm-git-base
  (package
    (inherit stumpwm)
    (version
     (git-version "24.11" "1" stumpwm-commit))
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/stumpwm/stumpwm")
         (commit stumpwm-commit)))
       (file-name
        (git-file-name "stumpwm" version))
       (sha256
        (base32
         "01pagpxqbwdxhvdzxvj3d027ajxk1438g4fkj5nb64mm7vs33fcp"))))
    (inputs
     (modify-inputs (package-inputs stumpwm)
       (prepend sbcl-dbus)))))

;; Public StumpWM package.
;;
;; Its dependency graph uses SBCL 2.6.10 and CLX 0.7.8.

(define-public stumpwm-git
  (with-lisp-stack stumpwm-git-base))



;; (define-public stumpwm-next
;;   (let ((commit "a774bfec147be098be1df98db19e16e79cd3fefd")
;;         (revision "0"))
;;     (package (inherit stumpwm)
;;              (name "stumpwm")             
;;              (version (git-version "24.11" revision commit))
;;              (source
;;               (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                        (url "https://github.com/stumpwm/stumpwm")
;;                        (commit commit)))
;;                 (file-name (git-file-name "stumpwm" "24.11"))

;;                 (sha256
;;                  (base32 "1l4rxcva947ijxsfnzyy35ql7a8pjsxaag51pq2bib3qfy7wg5ld")))))))

;; (define-public awesome-next
;;   (let ((commit "41473c05ed9e85de66ffb805d872f2737c0458b6")
;;         (revision "0"))
;;     (package (inherit awesome)
;;              (name "awesome-next")
;;              (version (git-version "4.3" revision commit))
;;              (source
;;               (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/awesomeWM/awesome")
;;                       (commit commit)))
;;                 (sha256
;;                  (base32 "0j28ghk9qyzc7lc066bv2zv84iycc03615ra7ja39i00jwkiwrvl"))))
;;              (arguments
;;               (substitute-keyword-arguments
;;                (package-arguments awesome)
;;                ((#:configure-flags flags)
;;                 ;; lua-ldoc output is non-deterministic, so disable it.
;;                 `(cons "-DGENERATE_DOC=off" ,flags))))
;;              (inputs (modify-inputs (package-inputs awesome)
;;                                     ;; Remove ldoc since we're not using it.
;;                                     (delete "lua-ldoc")
;;                                     (prepend librsvg))))))

;; (define-public awesome-next-luajit
;;   ;; Requires commit 353ccfb0dde0dc5c264595f9d17e47a23c82bcdf or later, because
;;   ;; we need the -DLUA_EXECUTABLE flag.
;;   (package
;;     (inherit awesome-next)
;;     (name "awesome-next-luajit")
;;     (inputs (modify-inputs (package-inputs awesome-next)
;;               (replace "lua" luajit)
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

;; (define-public awesome-next-luajit-tip
;;   ;; Requires commit 353ccfb0dde0dc5c264595f9d17e47a23c82bcdf or later, because
;;   ;; we need the -DLUA_EXECUTABLE flag.
;;   (package
;;     (inherit awesome-next)
;;     (name "awesome-next-luajit-tip")
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
