;;; GNU Guix --- Functional package management for GNU
;;; clone of https://codeberg.org/divyaranjan/divya-lambda/src/branch/master/divya-lambda/packages/emacs-xyz.scm#
;;; Copyright © 2024 Divya R. Pattanaik <divya@subvertising.org>
;;;
;;; This file is not part of GNU Guix
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Emacs packages for Guix

(define-module (jitterymisc packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses)
                #:prefix license:)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pdf))

(define-public emacs-binder
  (package
    (name "emacs-binder")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/divyaranjan/binder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        "1fdgl31zc15cysv1yys95j0106i2fwfxb3qcwwh7f3mcz2snr2md")))
    (build-system emacs-build-system)
    (home-page "https://codeberg.org/divyaranjan/binder")
    (synopsis
     "Binder is a global minor mode to facilitate working on multiple files")
    (description
     "Primarily, Binder provides a global minor mode Binder Mode.
This allows working with files in the current @code{binder-project-directory}.")
    (license license:gpl3+)))

(define-public emacs-reader
  (package
    (name "emacs-reader")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/divyaranjan/emacs-reader")
             (commit "master")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012i36l5wgzw4mbxr2knb96f7x95qfg0pqa7dfam23fmsfw57fy0"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'expand-load-path 'build-module
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "make" "USE_PKGCONFIG=no}"))) ; We don't need pkg-config
          (add-after 'install 'install-module
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (target-dir (string-append out
                                  "/share/emacs/site-lisp/" #$name "-" #$version)))
                (install-file "render-core.so" target-dir)))))))

    (native-inputs (list mupdf gcc))
    (home-page "https://codeberg.org/divyaranjan/emacs-reader")
    (synopsis
     "An all-in-one document reader for all formats in Emacs, backed by MuPDF.")
    (description
     "An all-in-one document reader for GNU Emacs, supporting all major document formats.
This package intends to take from doc-view, nov.el, and pdf-tools and make them better.
And as such, it is effectively a drop-in replacement for them.")
    (license license:gpl3+)))
