
(define-module (jitterymisc packages privilege)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (gnu packages gcc))

(define-public emacs-oom-launcher
  (package
    (name "emacs-oom-launcher")
    (version "1.0")

    ;; (source
    ;;  (local-file "/home/bslade/src/dev/c/proj/emacs-oom-launcher"
    ;;              #:recursive? #t))
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://codeberg.org/emacsomancer/emacs-oom-launcher.git")
         (commit "2d140322ce3773e66d30e23cd87b3b68538b5ea9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hmkzrlknaqq6cxiaz58b44c8aw0j5i3nx71xwb3x2iw1qwnkn6j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "gcc"
                      "-O2"
                      "-Wall"
                      "-Wextra"
                      "-Werror"
                      "-o" "emacs-oom-launcher"
                      "emacs-oom-launcher.c")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append
                          (assoc-ref outputs "out")
                          "/bin")))
                (install-file "emacs-oom-launcher"
                              (begin
                                (mkdir-p bin)
                                bin))))))))
    (native-inputs
     (list gcc))
    (home-page "https://codeberg.org/emacsomancer/emacs-oom-launcher")
    (synopsis "OOM-protected Emacs launcher")
    (description
     "A small privileged launcher that adjusts its Linux kernel
OOM score before executing Emacs as the invoking user.")
    (license license:gpl3+)))



