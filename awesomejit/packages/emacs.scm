;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse John Gildersleve <jessejohngildersleve@zohomail.eu>
;;; Copyright © 2019 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2019, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Declan Tsien <declantsien@riseup.net>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (awesomejit packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)  
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)     ; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lesstif)   ; motif
  #:use-module (gnu packages linux)     ; alsa-lib, gpm
  #:use-module (gnu packages mail)      ; for mailutils
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages web)       ; for jansson
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (emacs->emacs-more-next))

(define-public emacs-more-next-minimal
  (package
    (inherit emacs-minimal)
    (name "emacs-more-next-minimal")
    (version "30.0.93")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git")
                  (commit (string-append "emacs-" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32 "05a587a7bnbz8ms45h4hs1c33j8268aymf3y1bpxipl3abi43jlm"))
            (patches
             (search-patches "emacs-next-exec-path.patch"
                             "emacs-fix-scheme-indent-function.patch"
                             "emacs-next-native-comp-driver-options.patch"
                             "emacs-pgtk-super-key-fix.patch"))))))

(define-public emacs-head-minimal
  (let ((commit "8661f40ce4d6bce649cb2a564f7c4e766318476c")
        (revision "0"))
   (package
    (inherit emacs-minimal)
    (name "emacs-head-minimal")
    (version (git-version "31.0.50" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nj3a7wsl5piqf6a8wnmfyjbpxp2dwl0r48flv9q624jx4nxfr2p"))
       (patches
        (search-patches "emacs-next-exec-path.patch"
                        "emacs-fix-scheme-indent-function.patch"
                        "emacs-next-native-comp-driver-options.patch"
                        "emacs-pgtk-super-key-fix.patch")))))))


(define* (emacs->emacs-more-next emacs #:optional name
                            #:key (version (package-version emacs-more-next-minimal))
                            (source (package-source emacs-more-next-minimal)))
  (package
    (inherit emacs)
    (name (or name
              (and (string-prefix? "emacs" (package-name emacs))
                   (string-append "emacs-next"
                                  (string-drop (package-name emacs)
                                               (string-length "emacs"))))))
    (version version)
    (source source)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'validate-comp-integrity
              (lambda* (#:key outputs #:allow-other-keys)
                #$(cond
                   ((%current-target-system)
                    #~(display
                       "Cannot validate native compilation on cross builds.\n"))
                   ((member (%current-system) '("armhf-linux" "i686-linux"))
                    #~(display "Integrity test is broken on 32 bit systems.\n"))
                   (else
                    #~(invoke
                       (string-append (assoc-ref outputs "out") "/bin/emacs")
                       "--batch"
                       "--load"
                       #$(local-file
                          (search-auxiliary-file
                           "emacs/comp-integrity-next.el"))
                       "-f" "ert-run-tests-batch-and-exit")))))))))))

(define* (emacs->emacs-head emacs #:optional name
                            #:key (version (package-version emacs-head-minimal))
                            (source (package-source emacs-head-minimal)))
  (package
    (inherit emacs)
    (name (or name
              (and (string-prefix? "emacs" (package-name emacs))
                   (string-append "emacs-head"
                                  (string-drop (package-name emacs)
                                               (string-length "emacs"))))))
    (version version)
    (source source)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'validate-comp-integrity
              (lambda* (#:key outputs #:allow-other-keys)
                #$(cond
                   ((%current-target-system)
                    #~(display
                       "Cannot validate native compilation on cross builds.\n"))
                   ((member (%current-system) '("armhf-linux" "i686-linux"))
                    #~(display "Integrity test is broken on 32 bit systems.\n"))
                   (else
                    #~(invoke
                       (string-append (assoc-ref outputs "out") "/bin/emacs")
                       "--batch"
                       "--load"
                       #$(local-file
                          (search-auxiliary-file
                           "emacs/comp-integrity-next.el"))
                       "-f" "ert-run-tests-batch-and-exit")))))))))))


(define-public emacs-lucid
  (package/inherit emacs
    (name "emacs-lucid")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-x-toolkit=lucid"
                 "--without-toolkit-scroll-bars"
                 "--with-native-compilation=yes"
                 "--with-xft"
                 "--with-harfbuzz"
                 "--without-m17n-flt"
                 "--with-libotf"
                 "--without-gsettings"
                 "--without-gconf"
                 "--with-tree-sitter"
                 (delete "--with-native-compilation=aot" #$flags)))))
    (inputs
     (modify-inputs (package-inputs emacs)
       (prepend
        libxaw ;; for toolkit
        cairo dbus giflib harfbuzz libjpeg-turbo libotf
        libpng (librsvg-for-system) libtiff libx11 libxft
        libxpm pango poppler)))
    (synopsis "Emacs text editor with Lucid toolkit")
    (description "This Emacs build uses the Lucid toolkit.")))

(define-public emacs-xwidgets-tune-cflags
  (package
    (inherit emacs)
    (name "emacs-xwidgets-tune-cflags")
    (synopsis "Emacs text editor with xwidgets and CFLAGS tuning.")
    (inputs
     (modify-inputs (package-inputs emacs)
       (prepend
        gtk+ ;; for toolkit
        cairo dbus giflib harfbuzz libjpeg-turbo libotf
        libpng (librsvg-for-system) libtiff libx11 libxft
        libxpm pango poppler)))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-native-compilation=yes"
                 "--with-xft"
                 "--with-harfbuzz"
                 ;; "--without-m17n-flt"
                 "--with-libotf"
                 "--without-gsettings"
                 "--without-gconf"
                 "--with-xwidgets"
                 "--with-modules"
                 "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
                 #$flags))))))

(define-public emacs-lucid-tune-cflags
  (package
    (inherit emacs-lucid)
    (name "emacs-lucid-tune-cflags")
    (synopsis "Emacs text editor with Lucid toolkit and CFLAGS tuning.")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-lucid)
       ((#:configure-flags flags #~'())
        #~(cons "CFLAGS=-O2 -mtune=native -march=native" #$flags))))))
;; removed " -fomit-frame-pointer" ??

(define-public emacs-next-lucid (emacs->emacs-more-next emacs-lucid))
(define-public emacs-next-lucid-tune-cflags (emacs->emacs-more-next emacs-lucid-tune-cflags))
(define-public emacs-next-xwidgets-tune-cflags (emacs->emacs-more-next emacs-xwidgets-tune-cflags))

(define-public emacs-head-lucid (emacs->emacs-head emacs-lucid))
(define-public emacs-head-lucid-tune-cflags (emacs->emacs-head emacs-lucid-tune-cflags))
(define-public emacs-head-xwidgets-tune-cflags (emacs->emacs-head emacs-xwidgets-tune-cflags))
