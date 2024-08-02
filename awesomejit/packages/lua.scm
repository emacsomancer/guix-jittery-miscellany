(define-module (awesomejit packages lua)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lua)
  #:use-module (srfi srfi-1))

(define-public luajit-tip
  (let ((branch "2.1")
        (commit "93e87998b24021b94de8d1c8db244444c46fb6e9"))
    (package
      (name "luajit-tip")
      (version (git-version branch "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://luajit.org/git/luajit.git") 
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wbn7jkn72gx2sn5cbwnyv5kanqm449laaqj5whb1axmsn1mjcx6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; luajit is distributed without tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           ;; (add-after 'install 'create-luajit-symlink
           ;;   (lambda* (#:key outputs #:allow-other-keys)
           ;;     (let* ((out (assoc-ref outputs "out"))
           ;;            (bin (string-append out "/bin")))
           ;;       (with-directory-excursion bin
           ;;         (symlink ,(string-append name "-" branch)
           ;;                  ,name)))))
           )
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (home-page "https://www.luajit.org/")
      (synopsis
       "Just in time compiler for Lua programming language version 5.1")
      ;; On powerpc64le-linux, the build fails with an error: "No support for
      ;; PowerPC 64 bit mode (yet)".  See: https://issues.guix.gnu.org/49220
      (supported-systems (fold delete %supported-systems
                               (list "powerpc64le-linux" "riscv64-linux")))
      (description
       "LuaJIT is a Just-In-Time Compiler (JIT) for the Lua
programming language.  Lua is a powerful, dynamic and light-weight programming
language.  It may be embedded or used as a general-purpose, stand-alone
language.")
      (license license:x11))))

(define-public fennel
  (package
    (name "fennel")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bakpakin/Fennel.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d25v7swq3msxsdzv91wwxy89y3qgw4bvzq1px89qsjzbbd7ccg2"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "PREFIX="
                                               (assoc-ref %outputs "out")))
           #:tests? #t ;even on cross-build
           #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-after 'build 'patch-fennel
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "fennel"
                              (("/usr/bin/env .*lua")
                               (search-input-file inputs "/bin/lua")))))
                        (delete 'check)
                        ;; (add-after 'install 'check
                        ;;   (assoc-ref %standard-phases
                        ;;              'check))
                        )))
    (inputs (list lua))
    (home-page "https://fennel-lang.org/")
    (synopsis "Lisp that compiles to Lua")
    (description
     "Fennel is a programming language that brings together the speed,
simplicity, and reach of Lua with the flexibility of a Lisp syntax and macro
system.")
    (license license:expat)))

;; (define-public fennel-git-tip
;;   (let ((branch "main")
;;         (commit "3927010da56fc6161598a0e2edd2bad1b1a1a1af"))
;;     (package
;;      (name "fennel-git-tip")
;;      (version (git-version branch "0" commit))
;;      (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://git.sr.ht/~technomancy/fennel") 
;;                     (file-name (git-file-name name version))
;;                     (sha256
;;                      (base32
;;                       "1v5vb8zgxgsis8gmnd91sxx3vbnpvlnbfj8719s6gw0f35114mzb"))))))
;;      (build-system gnu-build-system)
;;      (arguments
;;       (list #:make-flags #~(list (string-append "PREFIX="
;;                                                 (assoc-ref %outputs "out")))
;;             #:tests? #t ;even on cross-build
;;             #:test-target "test"
;;             #:phases #~(modify-phases %standard-phases
;;                                       (delete 'configure)
;;                                       (add-after 'build 'patch-fennel
;;                                                  (lambda* (#:key inputs #:allow-other-keys)
;;                                                    (substitute* "fennel"
;;                                                                 (("/usr/bin/env .*lua")
;;                                                                  (search-input-file inputs "/bin/lua")))))
;;                                       (delete 'check)
;;                                       (add-after 'install 'check
;;                                                  (assoc-ref %standard-phases
;;                                                             'check))
;;                                       )))
;;      (inputs (list lua))
;;      (home-page "https://fennel-lang.org/")
;;      (synopsis "Lisp that compiles to Lua")
;;      (description
;;       "Fennel is a programming language that brings together the speed,
;; simplicity, and reach of Lua with the flexibility of a Lisp syntax and macro
;; system.")
;;      (license license:expat))))
