;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2020, 2021, 2022 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Jesse Gildersleve <jessejohngildersleve@protonmail.com>
;;; Copyright © 2019-2026 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020, 2024, 2025 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Dimakis Dimakakos <me@bendersteed.tech>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021, 2022 Adam Kandur <rndd@tuta.io>
;;; Copyright © 2020-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2022 Aurora <rind38@disroot.org>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021-2025 André A. Gomes <andremegafone@gmail.com>
;;; Copyright © 2021, 2022, 2023 Cage <cage-dev@twistfold.it>
;;; Copyright © 2021 Cameron Chaparro <cameron@cameronchaparro.com>
;;; Copyright © 2021, 2024, 2025 Charles Jackson <charles.b.jackson@protonmail.com>
;;; Copyright © 2021, 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2021-2026 jgart <jgart@dismail.de>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021 Jacob MacDonald <jaccarmac@gmail.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022, 2023, 2024 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2022, 2023 Artyom Bologov <mail@aartaka.me>
;;; Copyright © 2023, 2024 Roman Scherer <roman@burningswell.com>
;;; Copyright © 2023 ykonai <mail@ykonai.net>
;;; Copyright © 2023 Gabriel Hondet <gabriel.hondet@cominety.net>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 Michal Atlas <michal_atlas+git@posteo.net>
;;; Copyright © 2024 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2024 Nik Gaffney <nik@fo.am>
;;; Copyright © 2024 Grigory Shepelev <shegeley@gmail.com>
;;; Copyright © 2025 Junker <dk@junkeria.club>
;;; Copyright © 2025,2026 Simen Endsjø <contact@simendsjo.me>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2025 vga <v@vgz.li>
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

;;; This file only contains Common Lisp libraries.
;;; Common Lisp compilers and tooling go to lisp.scm.
;;; Common Lisp applications should go to the most appropriate file,
;;; e.g. StumpWM is in window-management.scm.
;;; Common Lisp libraries related to code testing facilities go
;;; to lisp-check.scm.

(define-module (jitterymisc packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (jitterymisc packages lisp)
  #:use-module ((gnu packages lisp) #:prefix gnu:)
  #:use-module ((gnu packages lisp-xyz) #:prefix gnu:)  
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19))

(define-public sbcl-clx
  (let ((version "0.7.8"))
    (package
      (name "sbcl-clx")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/sharplispers/clx")
           (commit version)))
         (sha256
          (base32 "1ihc2vfz8azhvv1ijzmbywyc75qazjc9b6s4ja5949z1b6gfn409"))
         (file-name (git-file-name "cl-clx" version))
         ;; (snippet
         ;;  #~(begin
         ;;      (use-modules (ice-9 textual-ports)
         ;;                   (srfi srfi-13))

         ;;      (let* ((file "translate.lisp")
         ;;             (old "(function (base-char card16 generalized-boolean card8) card8)")
         ;;             (new "(function (display card8 card16) card8)")
         ;;             (contents
         ;;              (call-with-input-file file get-string-all)))
         ;;        (unless (string-contains contents old)
         ;;          (error "CLX: keysym-index-function declaration not found"))
         ;;        (call-with-output-file file
         ;;          (lambda (port)
         ;;            (display (string-replace-substring contents old new)
         ;;                     port))))))
         ;; (snippet
         ;;  #~(begin
         ;;      (use-modules (guix build utils))
         ;;      (chmod "translate.lisp" #o644)
         ;;      (substitute*
         ;;          "translate.lisp"
         ;;        (("(function \\(base-char card16 generalized-boolean card8\\) card8)")
         ;;         "(function (display card8 card16) card8)"))))

         (snippet
          #~(begin
              (use-modules (guix build utils))
              (substitute*
                  "translate.lisp"
                (("base-char card16 generalized-boolean card8")
                 "display card8 card16"))))
         ))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl sbcl-fiasco xorg-server-for-tests))
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-before 'check 'prepare-test-environment
                   (lambda _
                     (system "Xvfb :1 &")
                     (setenv "DISPLAY" ":1"))))))
      (home-page "https://www.cliki.net/portable-clx")
      (synopsis "X11 client library for Common Lisp")
      (description "CLX is an X11 client library for Common Lisp.  The code was
originally taken from a CMUCL distribution, was modified somewhat in order to
make it compile and run under SBCL, then a selection of patches were added
from other CLXes around the net.")
      (license license:x11))))


(define-public sbcl-clx-xembed
  (let ((commit "a5c4b844d31ee68ffa58c933cc1cdddde6990743")
        (revision "1"))
    (package
      (name "sbcl-clx-xembed")
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/laynor/clx-xembed")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/laynor/clx-xembed")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1abx4v36ycmfjdwpjk4hh8058ya8whwia7ds9vd96q2qsrs57f12"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-systems '("xembed")))
      (native-inputs
       (list sbcl))
      (inputs
       (list sbcl-clx))
      (synopsis "CL(x) xembed protocol implementation")
      (description "CL(x) xembed protocol implementation")
      (license license:expat))))
