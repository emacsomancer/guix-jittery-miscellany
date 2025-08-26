;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2015, 2018, 2019, 2020, 2021, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018, 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2020, 2022 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Gábor Boskovit <boskovits@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2019 Arne Babenhauserheide <arne_bab@web.de>
;;; Copyright © 2019 Riku Viitanen <riku.viitanen@protonmail.com>
;;; Copyright © 2020, 2021, 2023, 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Alex McGrath <amk@amk.ie>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2021, 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 David Wilson <david@daviwil.com>
;;; Copyright © 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Robin Templeton <robin@terpri.org>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Bird <birdsite@airmail.cc>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Chadwain Holness <chadwainholness@gmail.com>
;;; Copyright © 2022 Andy Tai <atai@atai.org>
;;; Copyright © 2023 Ott Joon <oj@vern.cc>
;;; Copyright © 2023 Dominik Delgado Steuter <dds@disroot.org>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
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

(define-module (awesomejit packages video)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
    #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  ;; #:use-module (gnu packages crates-io)
  ;; #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages music)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages php)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages time)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video))


;; (define-public yt-dlp
;;   (package/inherit youtube-dl
;;     (name "yt-dlp")
;;     (version "2024.05.27")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/yt-dlp/yt-dlp/")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "13j6vg0kxfw3hppq7gzbz2d72g415071gh5arkwzj902rh0c7777"))))
;;     (arguments
;;      (substitute-keyword-arguments (package-arguments youtube-dl)
;;        ((#:tests? _) (not (%current-target-system)))
;;        ((#:phases phases)
;;         #~(modify-phases #$phases
;;             ;; See the comment for the corresponding phase in youtube-dl.
;;             (replace 'default-to-the-ffmpeg-input
;;               (lambda* (#:key inputs #:allow-other-keys)
;;                 (substitute* "yt_dlp/postprocessor/ffmpeg.py"
;;                   (("location = self.get_param(.*)$")
;;                    (string-append
;;                      "location = '"
;;                      (dirname (search-input-file inputs "bin/ffmpeg"))
;;                      "'\n")))))
;;             (replace 'build-generated-files
;;               (lambda* (#:key inputs #:allow-other-keys)
;;                 (if (assoc-ref inputs "pandoc")
;;                   (invoke "make"
;;                           "PYTHON=python"
;;                           "yt-dlp"
;;                           "yt-dlp.1"
;;                           "completions")
;;                   (invoke "make"
;;                           "PYTHON=python"
;;                           "yt-dlp"
;;                           "completions"))))
;;             (replace 'fix-the-data-directories
;;               (lambda* (#:key outputs #:allow-other-keys)
;;                 (let ((prefix (assoc-ref outputs "out")))
;;                   (substitute* "setup.py"
;;                     (("'etc/")
;;                      (string-append "'" prefix "/etc/"))
;;                     (("'share/")
;;                      (string-append "'" prefix "/share/"))))))
;;             (delete 'install-completion)
;;             (replace 'check
;;               (lambda* (#:key tests? #:allow-other-keys)
;;                 (when tests?
;;                   (invoke "pytest" "-k" "not download"))))))))
;;     (inputs (modify-inputs (package-inputs youtube-dl)
;;               (append python-brotli
;;                       python-certifi
;;                       python-mutagen
;;                       python-pycryptodomex
;;                       python-websockets)))
;;     (native-inputs
;;      (append
;;        ;; To generate the manpage.
;;        (if (supported-package? pandoc)
;;          (list pandoc)
;;          '())
;;        (list python-pytest zip)))
;;     (description
;;      "yt-dlp is a small command-line program to download videos from
;; YouTube.com and many more sites.  It is a fork of youtube-dl with a
;; focus on adding new features while keeping up-to-date with the
;; original project.")
;;     (properties '((release-monitoring-url . "https://pypi.org/project/yt-dlp/")))
;;     (home-page "https://github.com/yt-dlp/yt-dlp")))
