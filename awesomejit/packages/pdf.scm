;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2019,2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020-2023 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Benjamin Slade <slade@lambda-y.net>
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

(define-module (awesomejit packages pdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public pdfpc
  (package
    (name "pdfpc")
    (version "4.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pdfpc/pdfpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kj84sf5hgr2v2ra6dxmxqcr173h17cpnhg9lcq36shdbdnncwg4"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f          ; no test target
       #:phases
       (modify-phases %standard-phases
         ;; This is really a bug in Vala.
         ;; https://github.com/pdfpc/pdfpc/issues/594
         (add-after 'unpack 'fix-vala-API-conflict
           (lambda _
             (substitute* "src/classes/action/movie.vala"
               (("info.from_caps\\(caps\\)")
                "Gst.Video.info_from_caps(out info, caps)")))))))
    (inputs
     `(("cairo" ,cairo)
       ("discount" ,discount) ; libmarkdown
       ("gtk+" ,gtk+)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("poppler" ,poppler)
       ("pango" ,pango)
       ("vala" ,vala)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (native-inputs
     (list pkg-config))
    (home-page "https://pdfpc.github.io/")
    (synopsis "Presenter console with multi-monitor support for PDF files")
    (description
     "pdfpc is a presentation viewer application which uses multi-monitor
output to provide meta information to the speaker during the presentation.  It
is able to show a normal presentation window on one screen, while showing a
more sophisticated overview on the other one providing information like a
picture of the next slide, as well as the left over time till the end of the
presentation.  The input files processed by pdfpc are PDF documents.")
    (license license:gpl3+)))
