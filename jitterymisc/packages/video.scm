;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2025 John Kehayias <john@guixotic.coop>
;;; Copyright © 2025 Robin Templeton <robin@guixotic.coop>
;;; Copyright © 2025 Benjamin Slade <slade@lambda-y.net>
(define-module (jitterymisc packages video)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nongnu packages video)
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages nvidia)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix nonguix-license:))

(define-public grayjay
  (package
    (name "grayjay")
    (version "12.1")
    (source
     (origin
       (method url-fetch)
       ;; https://updater.grayjay.app/Apps/Grayjay.Desktop/Grayjay.Desktop-linux-x64.zip
       (uri (string-append "https://updater.grayjay.app/Apps/Grayjay.Desktop/"
                           ;; version
                           "/Grayjay.Desktop-linux-x64"
                           ;; "-v"
                           ;; version
                           ".zip"))
       (file-name (string-append name "-" version "-x86_64.zip"))
       (sha256
        (base32 "0m0sq3qwg21wgyplsgq421wryl22gg9c64jnpanzgzsm68aql05s"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f         ; prevent corruption of .NET programs
           #:patchelf-plan
           #~(let ((libs '("alsa-lib"
                           "at-spi2-core"
                           "cairo"
                           "cups"
                           "dbus"
                           "eudev"
                           "expat"
                           "fontconfig-minimal"
                           "gcc"
                           "glib"
                           "glibc"
                           "gtk+"
                           "icu4c"
                           "libdrm"
                           "libnotify"
                           "librsvg"
                           "libsecret"
                           "libx11"
                           "libxcb"
                           "libxcomposite"

                           "libxcursor"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxi"
                           "libxkbcommon"
                           "libxkbfile"
                           "libxrandr"
                           "libxrender"
                           "libxshmfence"
                           "libxtst"
                           "mesa"
                           "mit-krb5"
                           "nspr"
                           ("nss" "/lib/nss")
                           ("out" "/lib/grayjay/cef")
                           "openssl"
                           "pango"
                           "pulseaudio"
                           "sqlcipher"
                           "xcb-util"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "xcb-util-renderutil"
                           "xcb-util-wm"
                           "xdg-utils"
                           "zlib")))
               `(("ClearScriptV8.linux-x64.so" ,libs)
                 ("Grayjay" ,libs)
                 ("cef/chrome-sandbox" ,libs)
                 ("cef/dotcefnative" ,libs)
                 ;; Some of these likely are not directly used after
                 ;; patchelf-ing the main binaries, other than libcef.so.
                 ;; This allows validate-runpath to pass though.
                 ("cef/libEGL.so" ,libs)
                 ("cef/libGLESv2.so" ,libs)
                 ;; XXX: Can replace with chromium-embedded-framework?
                 ("cef/libcef.so" ,libs)
                 ("cef/libvk_swiftshader.so" ,libs)
                 ("cef/libvulkan.so.1" ,libs)
                 ("libe_sqlite3.so" ,libs)
                 ("libsodium.so" ,libs)))
           #:install-plan ''(("." "lib/grayjay"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'remove-files
                 (lambda _
                   ;; Disable automatic updates, unbundle ffmpeg, and remove
                   ;; "Portable" which makes Grayjay try to (unsuccessfully)
                   ;; run from its installed path.  (Grayjay doesn't find the
                   ;; updater or ffmpeg when run outside of lib/grayjay.)
                   (delete-file "FUTO.Updater.Client")
                   (delete-file "ffmpeg")
                   (delete-file "Portable")))
               (add-before 'install 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/lib/grayjay/Grayjay")
                              (string-append bin "/Grayjay")))))
               (add-before 'install 'install-icon
                 (lambda _
                   (let ((dir (string-append
                               #$output
                               "/share/icons/hicolor/scalable/apps")))
                     (mkdir-p dir)
                     (copy-file "grayjay.png"
                                (string-append dir
                                               "/app.grayjay.Grayjay.png")))))
               (add-after 'install 'wrap-program

                 (lambda* (#:key inputs #:allow-other-keys)
                   (wrap-program (string-append #$output "/lib/grayjay/Grayjay")
                     `("PATH" prefix
                       (,(string-append #$(this-package-input "ffmpeg")
                                        "/bin"))))))
               (add-after 'install 'create-desktop-file
                 (lambda _
                   (make-desktop-entry-file
                    (string-append #$output "/share/applications/Grayjay.desktop")
                    #:name "Grayjay"
                    #:type "Application"
                    #:exec (string-append #$output "/bin/Grayjay")
                    #:icon "app.grayjay.Grayjay"
                    #:categories '("AudioVideo" "Player")
                    #:startup-w-m-class "Grayjay"
                    #:comment "Universal media aggregator"))))))
    (native-inputs (list unzip))
    (inputs (list alsa-lib
                  at-spi2-core
                  bash-minimal
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  ffmpeg
                  fontconfig
                  freetype
                  `(,gcc "lib")
                  glib
                  glibc
                  gtk+
                  icu4c-76
                  libdrm
                  libnotify
                  librsvg
                  libsecret
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxrender
                  libxshmfence
                  libxtst
                  mesa
                  mit-krb5
                  nspr
                  nss
                  openssl
                  pango
                  pulseaudio
                  sqlcipher
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  xdg-utils
                  zlib))
    (home-page "https://grayjay.app/")
    (synopsis "Universal media aggregator")
    (description "Grayjay is a media aggregator application that enables users
to stream and download multimedia content from various online sources, most

prominently YouTube.  It also offers an extensible plugin API to create and
import new integrations.")
    (license
     ;; "Source First License 1.1" which allows distribution, modification,
     ;; etc. but with a non-commercial prohibition.
     (nonguix-license:nonfree
      "https://gitlab.futo.org/videostreaming/grayjay/-/blob/master/LICENSE.md"))))

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
