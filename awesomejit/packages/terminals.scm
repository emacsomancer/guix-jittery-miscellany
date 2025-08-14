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

(define-module (awesomejit packages terminals)
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
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages docbook)
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
  #:use-module (awesomejit packages fonts)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; #:use-module (awesomejit packages gtk)
  ;; #:export (ghostty)
  )

(define-public zutty
  (package
    (name "zutty")
    (version "0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.hq.sig7.se/zutty.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "115skr3lcw0hdshgxl72qnh635ajy7kk07dnrh9fhbx1bhvqcm3k"))))
    (build-system waf-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)            ; no check
       )))
    ;; (propagated-inputs
    ;;  (list libxmu))
    (native-inputs
     (list pkg-config python))
    (inputs
     (list freetype
           glew
           font-misc-misc ;; zutty has 9x18 font as default
           libxt ;; needed for xmu to be detected by `pkg-config'
           libxmu))
    (home-page "https://tomscii.sig7.se/zutty/")
    (synopsis "X terminal emulator rendering through OpenGL ES Compute Shaders")
    (description "Zutty [zuːc̟] is a GPU-accelerated terminal emulator for the X
Window, written in C++ and only relying on OpenGL ES 3.1 for rendering.
What really sets Zutty apart is its radically simple, yet extremely efficient
rendering implementation, coupled with a sufficiently complete (VTxxx) feature
set to make it useful for a wide range of users. Zutty offers high throughput
with low latency, and strives to conform to relevant (published or de-facto)
standards. Zutty provides a clean implementation written from scratch, resulting
in a minimal, maintainable, modern codebase unencumbered by historical baggage.")
    (license license:gpl3)))


;;; based on: https://codeberg.org/look/saayix/src/branch/main/modules/saayix/packages/terminals.scm
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;;           © 2025 B Slade <slade@lambda-y.net>
;;;
;;; This file is NOT part of GNU Guix.

;; (define ghostty
;;   (package
;;     (name "ghostty")
;;     (version "1.1.3")
;;     (source
;;       (origin
;;         (method git-fetch)
;;         (uri (git-reference
;;                (url "https://github.com/ghostty-org/ghostty")
;;                (commit "9d9d781a0b7142ddc176167ef5e889618d295ef5")))
;;         (file-name (git-file-name name version))
;;         (sha256
;;           (base32 "1pl05qdwcrqf1fil0siqr4p2nvx14p0qld691n55qhii9blbi4c8"))))
;;     (build-system zig-build-system)
;;     (arguments
;;       (list #:tests? #f
;;             #:zig zig-0.14
;;             #:install-source? #f
;;             #:zig-release-type "fast"
;;             #:zig-build-flags
;;             #~(list "-Dcpu=baseline"
;;                     "-Drenderer=opengl"
;;                     "--prefix"
;;                     "."
;;                     "--system"
;;                     (string-append (getenv "TMPDIR") "/source/zig-cache")
;;                     "--search-prefix"
;;                     #$(this-package-input "libadwaita")
;;                     "--search-prefix"
;;                     #$(this-package-input "gtk4-layer-shell")
;;                     "--search-prefix"
;;                     (string-append (getenv "TMPDIR") "/source/bzip2")
;;                     "-fno-sys=oniguruma")
;;             #:modules
;;            '((guix build zig-build-system)
;;              (guix build utils)
;;              (ice-9 match))
;;             #:phases
;;             #~(modify-phases %standard-phases
;;                 (replace 'unpack-dependencies
;;                   (lambda _
;;                     (mkdir-p "bzip2/lib")
;;                     (symlink
;;                       (string-append #$(this-package-input "bzip2")
;;                                      "/lib/libbz2.so")
;;                       "bzip2/lib/libbzip2.so")))
;;                 (add-after 'unpack 'unpack-zig
;;                   (lambda _
;;                     (for-each
;;                       (match-lambda
;;                         ((dst src)
;;                          (let* ((dest (string-append "zig-cache/" dst)))
;;                            (mkdir-p dest)
;;                            (cond
;;                              ((string-contains src ".tar.gz")
;;                               (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
;;                              ((string-contains src ".tar.xz")
;;                               (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
;;                              ((string-contains src ".tar.zst")
;;                               (invoke "tar" "-xf" src "-C" dest "--strip-components=1"))
;;                              (else (copy-recursively src dest))))))
;;                       `(("N-V-__8AAB0eQwD-0MdOEBmz7intriBReIsIDNlukNVoNu6o"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/zlib-1220fed0c74e1019b3ee29edae2051788b080cd96e90d56836eea857b0b966742efb.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0p6h2i9ajdp46lckdpibfqy4vz5nh5r22bqq96mp41k0ydiqis0p")))))
;;                         ("ziglyph-0.11.2-AAAAAHPtHwB4Mbzn1KvOV7Wpjo82NYEc_v0WC8oCLrkf"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/ziglyph-b89d43d1e3fb01b6074bc1f7fc980324b04d26a5.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1ngkyc81gqqfkgccxx4hj4w4kb3xk0ng7z73bwihbwbdw7rvvivj")))))
;;                         ("zigimg-0.1.0-lly-O6N2EABOxke8dqyzCwhtUCAafqP35zC7wsZ4Ddxj"
;;                          (ungexp
;;                            (origin
;;                              (method git-fetch)
;;                              (uri (git-reference
;;                                     (url "https://github.com/TUSF/zigimg")
;;                                     (commit
;;                                       "31268548fe3276c0e95f318a6c0d2ab10565b58d")))
;;                              (file-name "zigimg")
;;                              (sha256
;;                                (base32
;;                                  "1pf0rbyrrxq02dvkxfa9sba1f18bycdgvs2js2mfmkj8c6pmzfd1")))))
;;                         ("wayland-0.4.0-dev-lQa1kjfIAQCmhhQu3xF0KH-94-TzeMXOqfnP0-Dg6Wyy"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://codeberg.org/ifreund/zig-wayland/archive/f3c5d503e540ada8cbcb056420de240af0c094f7.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "06ajicqccha3ha1csv854kzbvqgicbi9mcsm7gdqcga0brkwdghk")))))
;;                         ("zig_objc-0.0.0-Ir_Sp3TyAADEVRTxXlScq3t_uKAM91MYNerZkHfbD0yt"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/mitchellh/zig-objc/archive/3ab0d37c7d6b933d6ded1b3a35b6b60f05590a98.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0z1mncqyq7nivs5f8xbwfj5hsblyhvgxw2c5dgjn0jk1mi3nszff")))))
;;                         ("N-V-__8AAB9YCQBaZtQjJZVndk-g_GDIK-NTZcIa63bFp9yZ"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/zig_js-12205a66d423259567764fa0fc60c82be35365c21aeb76c5a7dc99698401f4f6fefc.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0gaqqfb125pj62c05z2cpzih0gcm3482cfln50d41xf2aq4mw8vz")))))
;;                         ("zg-0.13.4-AAAAAGiZ7QLz4pvECFa_wG4O4TP4FLABHHbemH2KakWM"
;;                          (ungexp
;;                            (origin
;;                              (method git-fetch)
;;                              (uri (git-reference
;;                                     (url "https://codeberg.org/atman/zg")
;;                                     (commit
;;                                       "4a002763419a34d61dcbb1f415821b83b9bf8ddc")))
;;                              (file-name "zg")
;;                              (sha256
;;                                (base32
;;                                  "006nxw79n49j43vrxv0dmfql3afds1llq9fil7hbzbp4r3lyb3by")))))
;;                         ("zf-0.10.3-OIRy8aiIAACLrBllz0zjxaH0aOe5oNm3KtEMyCntST-9"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/natecraddock/zf/archive/7aacbe6d155d64d15937ca95ca6c014905eb531f.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1chbgj8d9wxdzsbq7c2d4hvqsn02y22jb5x7lmwbdqkz0wssayyy")))))
;;                         ("z2d-0.6.0-j5P_HvLdCABu-dXpCeRM7Uk4m16vULg1980lMNCQj4_C"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/vancluever/z2d/archive/1e89605a624940c310c7a1d81b46a7c5c05919e3.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0x7hlz067j1b8qalkbv09birw55ql9j3si981xdy9dbs8r4rahiw")))))
;;                         ("N-V-__8AAAzZywE3s51XfsLbP9eyEw57ae9swYB9aGB6fCMs"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/wuffs-122037b39d577ec2db3fd7b2130e7b69ef6cc1807d68607a7c232c958315d381b5cd.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "04qwpr8c4xjla4skwb1fpvkjc0c611qhbhz9xp3c9rlnpq5d4k4y")))))
;;                         ("N-V-__8AAKw-DAAaV8bOAAGqA0-oD7o-HNIlPFYKRXSPT03S"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/wayland-protocols-258d8f88f2c8c25a830c6316f87d23ce1a0f12d9.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1y1h0pmql53x6ixbsycgkzxlxsxqs9fkps754c7ycx8vx3fwmvaw")))))
;;                         ("N-V-__8AAKrHGAAs2shYq8UkE6bGcR1QJtLTyOE_lcosMn6t"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/wayland-9cb3d7aa9dc995ffafdbdef7ab86a949d0fb0e7d.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "03f574n5w0y6glr7lf8xjd71844qh8kxxb1s3zjpfxj3ivb92hga")))))
;;                         ("vaxis-0.1.0-BWNV_FUICQAFZnTCL11TUvnUr1Y0_ZdqtXHhd51d76Rn"
;;                          (ungexp
;;                            (origin
;;                              (method git-fetch)
;;                              (uri (git-reference
;;                                     (url "https://github.com/rockorager/libvaxis")
;;                                     (commit
;;                                       "1f41c121e8fc153d9ce8c6eb64b2bbab68ad7d23")))
;;                              (file-name "vaxis")
;;                              (sha256
;;                                (base32
;;                                  "0xihpzj37mhpzxjrfy20z651vxqrqsgkr9iqhv1g7slkyyi7gmkc")))))
;;                         ("N-V-__8AAHffAgDU0YQmynL8K35WzkcnMUmBVQHQ0jlcKpjH"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/utfcpp-1220d4d18426ca72fc2b7e56ce47273149815501d0d2395c2a98c726b31ba931e641.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1ksrdf7dy4csazhddi64xahks8jzf4r8phgkjg9hfxp722iniipz")))))
;;                         ("N-V-__8AANb6pwD7O1WG6L5nvD_rNMvnSc9Cpg1ijSlTYywv"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/spirv_cross-1220fb3b5586e8be67bc3feb34cbe749cf42a60d628d2953632c2f8141302748c8da.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1qspcsx56v0mddarb6f05i748wsl2ln3d8863ydsczsyqk7nyaxm")))))
;;                         ("N-V-__8AAPlZGwBEa-gxrcypGBZ2R8Bse4JYSfo_ul8i2jlG"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/sentry-1220446be831adcca918167647c06c7b825849fa3fba5f22da394667974537a9c77e.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1pqqqcin8nw398rvn187dfqlab4vikdssiry14qqs6nnr1y4kiia")))))
;;                         ("N-V-__8AAKYZBAB-CFHBKs3u4JkeiT4BMvyHu3Y5aaWF3Bbs"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/plasma_wayland_protocols-12207e0851c12acdeee0991e893e0132fc87bb763969a585dc16ecca33e88334c566.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0hgl1p173pxs50z1p6mjjzcqssn44aq0ip166k56p3nd98hvln2w")))))
;;                         ("N-V-__8AADYiAAB_80AWnH1AxXC0tql9thT-R-DYO1gBqTLc"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/pixels-12207ff340169c7d40c570b4b6a97db614fe47e0d83b5801a932dcd44917424c8806.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "06pi3f3lhyxfzczhwrc2b4n0jhhzydbz96qlpw12a24is0b3ps2m")))))
;;                         ("N-V-__8AAHjwMQDBXnLq3Q2QhaivE0kE2aD138vtX2Bq1g7c"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/oniguruma-1220c15e72eadd0d9085a8af134904d9a0f5dfcbed5f606ad60edc60ebeccd9706bb.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "187jk4fxdkzc0wrcx4kdy4v6p1snwmv8r97i1d68yi3q5qha26h0")))))
;;                         ("N-V-__8AAG3RoQEyRC2Vw7Qoro5SYBf62IHn3HjqtNVY6aWK"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/libxml2-2.11.5.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "05b2kbccbkb5pkizwx2s170lcqvaj7iqjr5injsl5sry5sg0aa3c")))))
;;                         ("libxev-0.0.0-86vtc-ziEgDbLP0vihUn1MhsxNKY4GJEga6BEr7oyHpz"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/mitchellh/libxev/archive/3df9337a9e84450a58a2c4af434ec1a036f7b494.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "12k35sa7ll3rasw92ys4grzcaz8kj2asiv7ilzkk3xkvsw1nm9m0")))))
;;                         ("N-V-__8AAJrvXQCqAT8Mg9o_tk6m0yf5Fz-gCNEOKLyTSerD"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/libpng-1220aa013f0c83da3fb64ea6d327f9173fa008d10e28bc9349eac3463457723b1c66.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0fm0y7543w2gx5sz3zg9i46x1am51c77a554r0zqwpphdjs9bk7y")))))
;;                         ("N-V-__8AAEH8MwQaEsARbyV42-bSZGcu1am8xtg2h67wTFC3"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/mbadolato/iTerm2-Color-Schemes/archive/4c57d8c11d352a4aeda6928b65d78794c28883a5.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0ddj389h2s4w0khf91rnskbsq61gjxqrdn22d51kv2qg86z71svk")))))
;;                         ("N-V-__8AAH0GaQC8a52s6vfIxg88OZgFgEW6DFxfSK4lX_l3"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/imgui-1220bc6b9daceaf7c8c60f3c3998058045ba0c5c5f48ae255ff97776d9cd8bfc6402.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0q3qxycyl0z64mxf5j24c0g0yhif3mi7qf183rwan4fg0hgd0px0")))))
;;                         ("N-V-__8AAGmZhABbsPJLfbqrh6JTHsXhY6qCaLAQyx25e0XE"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/highway-66486a10623fa0d72fe91260f96c892e41aceb06.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "04m21b46h6c4x099r9qb720ql9llpzz8yq3k94i8zq7l7s4zim47")))))
;;                         ("N-V-__8AAG02ugUcWec-Ndp-i7JTsJ0dgF8nnJRUInkGLG7G"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/harfbuzz-11.0.0.tar.xz")
;;                              (sha256
;;                                (base32
;;                                  "16rb7aazy36pj3xrjy149dd90j9yv7q5jnqx5kz2air1zsx52qzi")))))
;;                         ("N-V-__8AALiNBAA-_0gprYr92CjrMj1I5bqNu0TSJOnjFNSr"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/gtk4-layer-shell-1.1.0.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "12396gx723ybgq1xp9i02257hsmzqhb5z9b39xdyypha4s0l4a4q")))))
;;                         ("gobject-0.2.0-Skun7IWDlQAOKu4BV7LapIxL9Imbq1JRmzvcIkazvAxR"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/jcollie/ghostty-gobject/releases/download/0.14.0-2025-03-18-21-1/ghostty-gobject-0.14.0-2025-03-18-21-1.tar.zst")
;;                              (sha256
;;                                (base32
;;                                  "1clqi8glm65ylx4fv5fy5j2cy5d393jyyn749yfprpcx8nbjjrw5")))))
;;                         ("N-V-__8AABzkUgISeKGgXAzgtutgJsZc0-kkeqBBscJgMkvy"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/glslang-12201278a1a05c0ce0b6eb6026c65cd3e9247aa041b1c260324bf29cee559dd23ba1.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1dcpm70fhxk07vk37f5l0hb9gxfv6pjgbqskk8dfbcwwa2xyv8hl")))))
;;                         ("N-V-__8AAMrJSwAUGb9-vTzkNR-5LXS81MR__ZRVfF3tWgG6"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://github.com/glfw/glfw/archive/e7ea71be039836da3a98cea55ae5569cb5eb885c.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "02i91dn556qhb4mafybs0rgcl633plpkzxcmlg0ycc9581fpawrk")))))
;;                         ("N-V-__8AADcZkgn4cMhTUpIz6mShCKyqqB-NBtf_S2bHaTC-"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/gettext-0.24.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1dqq2ln01mfwr4gblvy0cyvarbqnv09ml5sdhksdlw1xb4ym0669")))))
;;                         ("N-V-__8AAKLKpwC4H27Ps_0iL3bPkQb-z6ZVSrB-x_3EEkub"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/freetype-1220b81f6ecfb3fd222f76cf9106fecfa6554ab07ec7fdc4124b9bb063ae2adf969d.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "035r5bypzapa1x7za7lpvpkz58fxynz4anqzbk8705hmspsh2wj2")))))
;;                         ("N-V-__8AAIrfdwARSa-zMmxWwFuwpXf1T3asIN7s5jqi9c1v"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/fontconfig-2.14.2.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "0mcarq6v9k7k9a8is23vq9as0niv0hbagwdabknaq6472n9dv8iv")))))
;;                         ("N-V-__8AALw2uwF_03u4JRkZwRLc3Y9hakkYV7NKRR9-RIZJ"
;;                          (ungexp
;;                            (origin
;;                              (method url-fetch)
;;                              (uri "https://deps.files.ghostty.org/breakpad-b99f444ba5f6b98cac261cbb391d8766b34a5918.tar.gz")
;;                              (sha256
;;                                (base32
;;                                  "1nbadlml3r982bz1wyp17w33hngzkb07f47nrrk0g68s7na9ijkc"))))))))))))
;;     (native-inputs
;;       (list `(,glib "bin")
;;             blueprint-compiler
;;             gnu-gettext
;;             gobject-introspection
;;             ncurses
;;             pandoc
;;             pkg-config
;;             tar))
;;     (inputs
;;       (list bzip2
;;             expat
;;             fontconfig
;;             freetype
;;             glslang
;;             gtk4-layer-shell
;;             harfbuzz
;;             libadwaita
;;             libglvnd
;;             libpng
;;             libx11
;;             libxcursor
;;             libxi
;;             libxrandr
;;             zlib))
;;     (native-search-paths
;;       ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
;;       ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
;;       (list (search-path-specification
;;               (variable "TERMINFO_DIRS")
;;               (files '("share/terminfo")))))
;;     (synopsis "Fast, native, feature-rich terminal emulator pushing modern features")
;;     (description "Ghostty is a terminal emulator that differentiates itself by
;; being fast, feature-rich, and native. While there are many excellent terminal
;; emulators available, they all force you to choose between speed, features, or
;; native UIs. Ghostty provides all three.")
;;     (home-page "https://ghostty.org")
;;     (license license:expat)))
