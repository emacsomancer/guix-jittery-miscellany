(define-module (awesomejit packages lua)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  ;; #:use-module (gnu packages)
  ;; #:use-module (gnu packages package-management)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages lua))

(define-public luajit-master
  (let ((branch "master")
        (commit "644723649ea04cb23b72c814b88b72a29e4afed4"))
    (package
      (name "luajit-master")
      (version (git-version branch "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://luajit.org/git/luajit.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1a002yh8v1i1q9w09494q0b8vsbmw3amn9jgfs5qnz7ba54jij0q"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; luajit is distributed without tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (add-after 'install 'create-luajit-symlink
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (with-directory-excursion bin
                   (symlink ,(string-append name "-" branch)
                            ,name))))))
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (home-page "https://www.luajit.org/")
      (synopsis
       "Just in time compiler for Lua programming language version 5.1")
      ;; On powerpc64le-linux, the build fails with an error: "No support for
      ;; PowerPC 64 bit mode (yet)".  See: https://issues.guix.gnu.org/49220
      ;; [bms: I'm not sure what module "fold" is part of; commented out for now.]
      ;; (supported-systems (fold delete %supported-systems
      ;;                          (list "powerpc64le-linux" "riscv64-linux")))
      (description
       "LuaJIT is a Just-In-Time Compiler (JIT) for the Lua
programming language.  Lua is a powerful, dynamic and light-weight programming
language.  It may be embedded or used as a general-purpose, stand-alone
language.")
      (license license:x11))))

