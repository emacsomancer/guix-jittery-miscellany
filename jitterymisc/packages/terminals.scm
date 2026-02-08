;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mckinley Olsen <mck.olsen@gmail.com>
;;; Copyright © 2016, 2017, 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 luhux <luhux@outlook.com>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021, 2022, 2024 Raphaël Mélotte <raphael.melotte@mind.be>
;;; Copyright © 2021 ikasero <ahmed@ikasero.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2022 Felipe Balbi <balbi@kernel.org>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022, 2023 jgart <jgart@dismail.de>
;;; Copyright © 2023 Aaron Covrig <aaron.covrig.us@ieee.org>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2024 Suhail <suhail@bayesians.ca>
;;; Copyright © 2024 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; (add-to-load-path ".")
;; (add-to-load-path "/home/slade/.config/guix/")

(define-module (jitterymisc packages terminals)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)  
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  ;; #:use-module (gnu packages crates-apple)
  ;; #:use-module (gnu packages crates-io)
  ;; #:use-module (gnu packages crates-graphics)
  ;; #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig)
  ;; #:use-module (jitterymisc packages fonts)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  ;; #:use-module (jitterymisc packages gtk)
  ;; #:use-module (nil packages rust-crates)
  #:export (zutty )
                                                       )

;; (include "rust-crates.scm")


;; WIP!
;; (define-public alacritty-graphics
;;   (package
;;     (name "alacritty")
;;     (version "0.16.1")
;;     (source
;;      (origin
;;        ;; XXX: The crate at "crates.io" contains only the alacritty subproject
;;        ;; of alacritty and thus has limited contents.  In particular,
;;        ;; it does not contain "extra" directory with completions, icon, etc.
;;        (method git-fetch)
;;        (uri (git-reference
;;               ;; https://github.com/ayosec/alacritty/releases/download/v0.16.1-graphics/alacritty-linux-x86_64.gz
;;               (url "https://github.com/ayosec/alacritty")
;;               (commit (string-append "v" version "-graphics"))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "04n1mk5mgm1k3y45b6sjl16a86xamv8fvyc4cg2abrmlphc39skv"))))
;;     (build-system cargo-build-system)
;;     (arguments
;;      `(#:install-source? #f
;;        #:cargo-test-flags
;;        '("--"
;;          ;; Changes in clap regularly break this test.
;;          "--skip=cli::tests::completions")
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-after 'unpack 'patch-xdg-open
;;            (lambda* (#:key inputs #:allow-other-keys)
;;              (substitute* "alacritty/src/config/ui_config.rs"
;;                (("xdg-open") (search-input-file inputs "/bin/xdg-open")))))
;;          (add-after 'configure 'add-absolute-library-references
;;            (lambda* (#:key inputs vendor-dir #:allow-other-keys)
;;              ;; Fix dlopen()ing some libraries on pure Wayland (no $DISPLAY):
;;              ;; Failed to initialize any backend! Wayland status: NoWaylandLib
;;              ;; XXX We patch transitive dependencies that aren't even direct
;;              ;; inputs to this package, because of the way Guix's Rust build
;;              ;; system currently works.  <http://issues.guix.gnu.org/46399>
;;              ;; might fix this and allow patching them directly.
;;              (substitute* (find-files vendor-dir "\\.rs$")
;;                (("libEGL\\.so")
;;                 (search-input-file inputs "lib/libEGL.so"))
;;                (("libGL\\.so")
;;                 (search-input-file inputs "lib/libGL.so"))
;;                ;; Lots of libraries from rust-x11-dl and others.
;;                (("libX[[:alpha:]]*\\.so" all)
;;                 (search-input-file inputs (string-append "lib/" all)))

;;                ;; There are several libwayland libraries.
;;                (("libwayland\\.so" all)
;;                 (search-input-file inputs (string-append "lib/" all)))
;;                (("libwayland-[[:alpha:]]*\\.so" all)
;;                 (search-input-file inputs (string-append "lib/" all)))
;;                (("libxkbcommon-x11\\.so")
;;                 (search-input-file inputs "lib/libxkbcommon-x11.so"))
;;                (("libxkbcommon\\.so")
;;                 (search-input-file inputs "lib/libxkbcommon.so")))))
;;          (replace 'install
;;            ;; Upstream install script only takes care of executable.
;;            (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
;;              (let* ((out   (assoc-ref outputs "out"))
;;                     (bin   (string-append out "/bin"))
;;                     (share (string-append out "/share"))
;;                     (icons (string-append share "/icons/hicolor/scalable/apps"))
;;                     (tic   (search-input-file (or native-inputs inputs) "/bin/tic"))
;;                     (man   (string-append share "/man"))
;;                     (alacritty-bin (car (find-files "target" "^alacritty$"))))
;;                ;; Install the executable.
;;                (install-file alacritty-bin bin)
;;                ;; Install man pages.
;;                (mkdir-p (string-append man "/man1"))
;;                (mkdir-p (string-append man "/man5"))
;;                (define (create-manpage manpage)
;;                  (let ((mandir (string-append
;;                                  "/man" (string-take-right manpage 1) "/")))
;;                    (with-input-from-file (string-append manpage ".scd")
;;                      (lambda _
;;                        (with-output-to-file (string-append man mandir manpage)
;;                          (lambda _ (invoke "scdoc")))))))
;;                (with-directory-excursion "extra/man"
;;                  (for-each create-manpage '("alacritty.1"
;;                                             "alacritty-msg.1"
;;                                             "alacritty.5"
;;                                             "alacritty-bindings.5")) )v 
;;                ;; Install desktop file.
;;                (install-file "extra/linux/Alacritty.desktop"
;;                              (string-append share "/applications"))
;;                (install-file "extra/linux/org.alacritty.Alacritty.appdata.xml"
;;                              (string-append share "/metainfo"))
;;                ;; Install icon.
;;                (mkdir-p icons)
;;                (copy-file "extra/logo/alacritty-term.svg"
;;                           (string-append icons "/Alacritty.svg"))
;;                ;; Install terminfo.
;;                (mkdir-p (string-append share "/terminfo"))
;;                ;; We don't compile alacritty-common entry because
;;                ;; it's being used only for inheritance.
;;                (invoke tic "-x" "-e" "alacritty,alacritty-direct"
;;                        "-o" (string-append share "/terminfo/")
;;                        "extra/alacritty.info")
;;                ;; Install completions.
;;                (mkdir-p (string-append
;;                          out "/share/bash-completion/completions"))
;;                (copy-file "extra/completions/alacritty.bash"
;;                           (string-append
;;                            out "/share/bash-completion/completions/alacritty"))
;;                (install-file "extra/completions/_alacritty"
;;                              (string-append share "/zsh/site-functions"))
;;                (install-file "extra/completions/alacritty.fish"
;;                              (string-append share "/fish/vendor_completions.d"))))))))
;;     (native-inputs
;;      (list ncurses
;;            pkg-config
;;            python
;;            scdoc))
;;     (inputs
;;      (cons* expat
;;             fontconfig
;;             freetype
;;             libx11
;;             libxcb
;;             libxcursor
;;             libxext
;;             libxft
;;             libxi
;;             libxinerama
;;             libxkbcommon
;;             libxmu
;;             libxpresent
;;             libxrandr
;;             libxscrnsaver
;;             libxt
;;             libxtst
;;             libxxf86vm
;;             mesa
;;             xdg-utils
;;             wayland
;;             (cargo-inputs 'alacritty-graphics)))
;;     (native-search-paths
;;      ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
;;      ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
;;      (list (search-path-specification
;;             (variable "TERMINFO_DIRS")
;;             (files '("share/terminfo")))))
;;     (home-page "https://alacritty.org/")
;;     (synopsis "GPU-accelerated terminal emulator")
;;     (description
;;      "Alacritty is a GPU-accelerated terminal emulator with a strong focus on
;; simplicity and performance.  With such a strong focus on performance, included
;; features are carefully considered and you can always expect Alacritty to be
;; blazingly fast.  By making sane choices for defaults, Alacritty requires no
;; additional setup.  However, it does allow configuration of many aspects of the
;; terminal.  Note that you need support for OpenGL 3.2 or higher. This
;; branch supports display of sixels.")
;;     (license license:asl2.0)))



(define-public zutty
  (package
    (name "zutty")
    (version "0.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.hq.sig7.se/zutty.git")
                    (commit version)))
             (sha256 (base32 "115skr3lcw0hdshgxl72qnh635ajy7kk07dnrh9fhbx1bhvqcm3k"))))
    (build-system waf-build-system)
    (inputs (list pkg-config freetype glew libxmu libxt font-misc-misc))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         ;; Install docs: README + upstream Org files
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((docdir (string-append (assoc-ref outputs "out")
                                          "/share/doc/zutty")))
               (mkdir-p docdir)
               (install-file "README.md" docdir)
               (for-each
                (lambda (f)
                  (install-file (string-append "doc/" f) docdir))
                '("USAGE.org" "KEYS.org" "HACKING.org" "VTTEST.org"))
               #t)))

         ;; Install desktop file
         (add-after 'install 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((applications-dir (string-append (assoc-ref outputs "out")
                                                    "/share/applications")))
               (mkdir-p applications-dir)
               (install-file "icons/zutty.desktop" applications-dir)
               #t)))

         ;; Install icons
         (add-after 'install 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((icons-dir (string-append (assoc-ref outputs "out")
                                             "/share/icons/hicolor")))

               ;; scalable SVG
               (mkdir-p (string-append icons-dir "/scalable/apps"))
               (install-file "icons/zutty.svg"
                             (string-append icons-dir "/scalable/apps"))

               ;; PNG sizes
               (for-each
                (lambda (size)
                  (let ((dir (string-append icons-dir "/" size "x" size "/apps")))
                    (mkdir-p dir)
                    (install-file
                     (string-append "icons/zutty_" size "x" size ".png")
                     dir)))
                '("16" "32" "48" "64" "96" "128"))
               #t))))))
    (synopsis "X terminal emulator rendering through OpenGL ES Compute Shaders")
    (description
     "Zutty [zuːc̟] is a GPU-accelerated terminal emulator for X
Window, written in C++ and only relying on OpenGL ES 3.1 for rendering.
It provides efficient rendering, VTxxx support, and a minimal, maintainable codebase.
The wrapper ensures Zutty finds its default font and falls back gracefully to X core fonts if needed.")
    (home-page "https://tomscii.sig7.se/zutty/")
    (license license:gpl3)))

;; (define-public zutty
;;   (package
;;     (name "zutty")
;;     (version "0.16")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://git.hq.sig7.se/zutty.git")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256 (base32 "115skr3lcw0hdshgxl72qnh635ajy7kk07dnrh9fhbx1bhvqcm3k"))))
;;     (build-system waf-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (delete 'check)
;;          (add-after 'install 'wrap-zutty
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (let* ((out (assoc-ref outputs "out"))
;;                     (font-dir (string-append out "/share/fonts")))
;;                (mkdir-p font-dir)
;;                ;; copy default 9x18 font
;;                (install-file (string-append (package-input-path font-misc-misc) "/9x18.pcf")
;;                              font-dir)
;;                ;; wrap Zutty binary
;;                (wrap-program (string-append out "/bin/zutty")
;;                  (list (cons "FONT_PATH" font-dir)
;;                        (cons "ZUTTY_FONT_FALLBACK" "9x18")))))))))
;;     (native-inputs (list pkg-config python))
;;     (inputs (list freetype glew libxmu libxt font-misc-misc))
;;     (synopsis "X terminal emulator rendering through OpenGL ES Compute Shaders")
;;     (description
;;      "Zutty [zuːc̟] is a GPU-accelerated terminal emulator for X
;; Window, written in C++ and only relying on OpenGL ES 3.1 for rendering.
;; It provides efficient rendering, VTxxx support, and a minimal, maintainable codebase.
;; The wrapper ensures Zutty finds its default font and falls back gracefully to X core fonts if needed.")
;;     (home-page "https://tomscii.sig7.se/zutty/")
;;     (license license:gpl3)))


;; (provide 'zutty)

;; (define-public zutty
;;   (package
;;     (name "zutty")
;;     (version "0.16")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://git.hq.sig7.se/zutty.git")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32
;;          "115skr3lcw0hdshgxl72qnh635ajy7kk07dnrh9fhbx1bhvqcm3k"))))
;;     (build-system waf-build-system)
;;     (arguments
;;      `(#:phases
;;        (modify-phases %standard-phases
;;          (delete 'check)            ; no check
;;        )))
;;     ;; (propagated-inputs
;;     ;;  (list libxmu))
;;     (native-inputs
;;      (list pkg-config python))
;;     (inputs
;;      (list freetype
;;            glew
;;            font-misc-misc ;; zutty has 9x18 font as default
;;            libxmu
;;            libxt ;; needed for xmu to be detected by `pkg-config'
;;            ))
;;     (home-page "https://tomscii.sig7.se/zutty/")
;;     (synopsis "X terminal emulator rendering through OpenGL ES Compute Shaders")
;;     (description "Zutty [zuːc̟] is a GPU-accelerated terminal emulator for the X
;; Window, written in C++ and only relying on OpenGL ES 3.1 for rendering.
;; What really sets Zutty apart is its radically simple, yet extremely efficient
;; rendering implementation, coupled with a sufficiently complete (VTxxx) feature
;; set to make it useful for a wide range of users. Zutty offers high throughput
;; with low latency, and strives to conform to relevant (published or de-facto)
;; standards. Zutty provides a clean implementation written from scratch, resulting
;; in a minimal, maintainable, modern codebase unencumbered by historical baggage.")
;;     (license license:gpl3)))


;; alacritty-graphics
;; zutty
