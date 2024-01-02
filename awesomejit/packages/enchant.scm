;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2024 Benjamin Slade <slade@lambda-y.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (awesomejit packages enchant)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (srfi srfi-1))


(define-public enchant
  (package
    (name "enchant")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/AbiWord/enchant/releases"
                                  "/download/v" version "/enchant-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07byzy5nanmkbfgcyr3xpsfywiivnl7jz8h9xs6hyxkfwllag1k6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static"
                           ;; Tests require a relocatable build.
                           "--enable-relocatable")))
    (inputs
     (list aspell hunspell))
    (propagated-inputs
     ;; Required by enchant.pc.
     (list glib))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("unittest-cpp" ,unittest-cpp)))
    (synopsis "Multi-backend spell-checking library wrapper")
    (description
      "On the surface, Enchant appears to be a generic spell checking library.
Looking closer, you'll see the Enchant is more-or-less a fancy wrapper around
the dlopen() system call.

Enchant steps in to provide uniformity and conformity on top of these libraries,
and implement certain features that may be lacking in any individual provider
library.  Everything should \"just work\" for any and every definition of \"just
working\".")
    (home-page "https://abiword.github.io/enchant/")
    (license lgpl2.1+)))

