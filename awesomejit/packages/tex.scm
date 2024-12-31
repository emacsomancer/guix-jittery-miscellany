;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2022 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2022 Simon South <simon@simonsouth.net>
;;; Copyright © 2022 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2022 Fabio Natali <me@fabionatali.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2023 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Dominik Delgado Steuter <d@delgado.nrw>
;;; Copyright © 2023 Timothy Sample <samplet@ngyro.com>
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

(define-module (awesomejit packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))


(define-public texlive-newcomputermodern
  (package
    (name "texlive-newcomputermodern")
    (version "7.0.0")
    (source (origin
              (method svn-multi-fetch)
              (uri (svn-multi-reference
                    (url (texlive-packages-repository version))
                    (revision 70951)
                    (locations (list "doc/fonts/newcomputermodern/"
                                     "fonts/opentype/public/newcomputermodern/"
                                     "tex/latex/newcomputermodern/"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qqw6src414v9f418xc9pjwzij67rh874rwz8vnrz9fczvpbiq8f"))))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/newcomputermodern")
    (synopsis "Computer Modern fonts including matching non-Latin alphabets")
    (description
     "This is a new assembly of Computer Modern fonts including extensions in
many directions for both Latin based languages, non-Latin based languages and
Mathematics, all compatible in style to CM fonts.  In addition to the Regular
weight of Computer Modern, it provides a Book weight for heavier printing.")
    (license license:gfl1.0)))
