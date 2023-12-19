;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2013, 2015, 2017-2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Alexander I.Grafov <grafov@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marek Benc <dusxmt@gmx.com>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019, 2020 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019, 2021 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Boris A. Dekshteyn <boris.dekshteyn@gmail.com>
;;; Copyright © 2020 Alex McGrath <amk@amk.ie>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021, 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Renzo Poddighe <renzo@poddighe.nl>
;;; Copyright © 2021 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Niklas Eklund <niklas.eklund@posteo.net>
;;; Copyright © 2021 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2021 ikasero <ahmed@ikasero.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Derek Chuank <derekchuank@outlook.com>
;;; Copyright © 2022, 2023 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Tobias Kortkamp <tobias.kortkamp@gmail.com>
;;; Copyright © 2022 Mehmet Tekman <mtekman89@gmail.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
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

(define-module (awesomejit packages xdisorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages)
  #:use-module (ice-9 match))


(define-public xsecurelock
  (package
    (name "xsecurelock")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/google/xsecurelock/releases"
                    "/download/v" version "/xsecurelock-" version ".tar.gz"))
              (sha256
               (base32 "09c0br8vwx9q728i4iv1pcp4s0sm0cd1c5ligag4k2730kcg93bf"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--with-pam-service-name=login"
         "--with-xkb"
         "--with-default-authproto-module=/run/setuid-programs/authproto_pam"
         "--with-xscreensaver=/run/current-system/profile/libexec/xscreensaver"
         ;; "--with-xscreensaver=/run/current-system/profile/bin/xscreensaver"
         )))
    (native-inputs
     (list pandoc pkg-config))
    (inputs
     (list fontconfig
           libx11
           libxcomposite
           libxext
           libxfixes
           libxft
           libxmu
           libxrandr
           libxscrnsaver
           linux-pam))
    (propagated-inputs
     (list xscreensaver xrdb))
    (home-page "https://github.com/google/xsecurelock")
    (synopsis "X11 screen lock utility with the primary goal of security")
    (description "@code{xsecurelock} is an X11 screen locker which uses
a modular design to avoid the usual pitfalls of screen locking utility design.

As a consequence of the modular design, the usual screen locker service
shouldn't be used with @code{xsecurelock}.  Instead, you need to add a helper
binary to setuid-binaries:
@example
(setuid-programs
 (cons*
  (setuid-program
   (program (file-append xsecurelock \"/libexec/xsecurelock/authproto_pam\")))
  %setuid-programs))
@end example")
    (license license:asl2.0)))

;; (define-public xsecurelock-xscreensaver
;;   (let ((branch "master")
;;         (commit "28d1c68708b1081ce0e315297e7423f9ea246cfa"))
;;     (package
;;       (name "xsecurelock-xscreensaver")
;;       (version (git-version branch "0" commit))
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/google/xsecurelock/")
;;                       (commit commit)
;;                       ;; "/download/v" version "/xsecurelock-" version ".tar.gz"
;;                       ))
;;                 (sha256
;;                  (base32 "1pvm6s6d0r6ajsl7gyfxf5z3l53bmyf1d07nff2alzx2zsj2jxrs"))))
;;       (build-system gnu-build-system)
;;       (arguments
;;        '(#:configure-flags
;;          '("--with-pam-service-name=login"
;;            "--with-xkb"
;;            "--with-default-authproto-module=/run/setuid-programs/authproto_pam"
;;            "--with-xscreensaver=/run/current-system/profile/libexec/xscreensaver"
;;            ;; "--with-xscreensaver=/run/current-system/profile/bin/xscreensaver"
;;            )))
;;       (native-inputs
;;        (list pandoc pkg-config autoconf automake))
;;       (inputs
;;        (list fontconfig
;;              libx11
;;              libxcomposite
;;              libxext
;;              libxfixes
;;              libxft
;;              libxmu
;;              libxrandr
;;              libxscrnsaver
;;              linux-pam))
;;       (propagated-inputs
;;        (list xscreensaver))
;;       (home-page "https://github.com/google/xsecurelock")
;;       (synopsis "X11 screen lock utility with the primary goal of security")
;;       (description "@code{xsecurelock} is an X11 screen locker which uses
;; a modular design to avoid the usual pitfalls of screen locking utility design.

;; As a consequence of the modular design, the usual screen locker service
;; shouldn't be used with @code{xsecurelock}.  Instead, you need to add a helper
;; binary to setuid-binaries:
;; @example
;; (setuid-programs
;;  (cons*
;;   (setuid-program
;;    (program (file-append xsecurelock \"/libexec/xsecurelock/authproto_pam\")))
;;   %setuid-programs))
;; @end example")
;;       (license license:asl2.0))))
