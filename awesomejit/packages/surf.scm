;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Benjamin Slade <slade@lambda-y.net>

(define-module (awesomejit packages surf)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages gawk)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public aartaka-surf
  (let ((commit "11d9703d0437a5e67bf83b5291e69f2580e38ca9")
        (revision "0"))
    (package
     (inherit surf)
     (source (origin
               (method git-fetch)
               (git-reference
                (url "https://github.com/aartaka/surf.git")
                (commit commit)))))))
