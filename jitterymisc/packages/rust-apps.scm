;;; rust-apps
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020-2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.ccom>
;;; Copyright © 2021, 2022, 2025 Zheng Junjie <z572@z572.online>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021, 2023-2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2023 Arnav Andrew Jose <arnav.jose@gmail.com>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2023, 2024 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Suhail Singh <suhail@bayesians.ca>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
;;; Copyright © 2024 muradm <mail@muradm.net>
;;; Copyright © 2024 normally_js <normally_js@posteo.net>
;;; Copyright © 2025 Divya Ranjan Pattanaik <divya@subvertising.org>
;;; Copyright © 2025 Andrew Wong <wongandj@icloud.com>
;;; Copyright © 2024 Danny Milosavljevic <dannym@friendly-machines.com>
;;; Copyright © 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Gabriel Santos <gabrielsantosdesouza@disroot.org>
;;; Copyright © 2025 Timo Wilken <guix@twilken.net>
;;; Copyright © 2025 Igorj Gorjaĉev <igor@goryachev.org>
;;; Copyright © 2025 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2025 Samuel Sehnert <mail@buffersquid.com>
;;; Copyright © 2025 Julian Flake <julian@flake.de>
;;;
;;;
;;; This is free software; you can redistribute it and/or modify it
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

(define-module (jitterymisc packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde-internet)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages rust-apps))

(define-public eza
  (package
    (name "eza")
    (version "0.23.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eza" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "139mbfmvfp8h74kpb2ynn1zr4z7pdyyg7m6iynzdhv1281a6cf0c"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-manual
            (lambda* (#:key inputs #:allow-other-keys)
              (when (assoc-ref inputs "pandoc")
                (map (lambda (page)
                       (with-output-to-file page
                         (lambda _
                           (invoke "pandoc" "--standalone"
                                   "-f" "markdown"
                                   "-t" "man"
                                   (string-append "man/" page ".md")))))
                     (list "eza.1"
                           "eza_colors.5"
                           "eza_colors-explanation.5")))))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                       (string-append share "/bash-completion/completions"))
                     (zsh-completions-dir
                       (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                       (string-append share "/fish/vendor_completions.d"))
                     (nu-completions-dir
                       (string-append share "/nushell/vendor/autoload"))
                     (man1 (string-append share "/man/man1"))
                     (man5 (string-append share "/man/man5")))
                (when (file-exists? "eza.1")
                  (install-file "eza.1" man1))
                (when (file-exists? "eza_colors.5")
                  (install-file "eza_colors.5" man5))
                (when (file-exists? "eza_colors-explanation.5")
                  (install-file "eza_colors-explanation.5" man5))
                (install-file "completions/bash/eza" bash-completions-dir)
                (install-file "completions/zsh/_eza" zsh-completions-dir)
                (install-file "completions/fish/eza.fish" fish-completions-dir)
                (install-file "completions/nush/eza.nu" nu-completions-dir)))))))
    (native-inputs
     (append (list pkg-config)
             (if (supported-package? pandoc)
                 (list pandoc)
                 '())))
    (inputs (cons* libgit2-1.9 zlib (cargo-inputs 'eza)))
    (home-page "https://eza.rocks/")
    (synopsis "Modern replacement for ls")
    (description
     "@code{eza} is a modern replacement for the command-line
program @code{ls}.  It uses colours to distinguish file types and
metadata.  It also knows about symlinks, extended attributes, and Git.
This package is the community maintained fork of @code{exa}.")
    (license license:eupl1.2)))

(define-deprecated-package exa
  eza)

