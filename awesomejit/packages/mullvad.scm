;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2025 Benjamin Slade <slade@lambda-y.net>
;;; based on https://codeberg.org/fishinthecalculator/small-guix/src/branch/master/small-guix/packages/mullvad.scm

(define-module (awesomejit packages mullvad)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages networking)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  ;; #:use-module (guix packages libffi)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match))

(define %mullvad-vpn-desktop-version
  ;; "2023.4"
  "2025.2"
  )

(define (mullvad-vpn-desktop-origin-url system)
  (string-append "https://github.com/mullvad/mullvadvpn-app/releases/"
                 "download/" %mullvad-vpn-desktop-version "/MullvadVPN-"
                 %mullvad-vpn-desktop-version "_" system "64.deb"))

(define* (mullvad-vpn-desktop-origin-values #:key amd64-hash aarch64-hash)
  (match (%current-system)
    ("x86_64-linux"
     (values (mullvad-vpn-desktop-origin-url "amd") amd64-hash))
    ("aarch64-linux"
     (values (mullvad-vpn-desktop-origin-url "arm") aarch64-hash))))

(define-public mullvad-vpn-desktop
  (define-values (url hash)
    (mullvad-vpn-desktop-origin-values
     #:amd64-hash "04qjmjsd78i1hhr8r28abfh3z2snkpnjpn3l4ia7mzzmnmz25npc"
     #:aarch64-hash "0r27z9ak21bjfx19syb7bf8q5dq0gjlvwky20mc6nlj15bwnjjbv"))
  (package
   (name "mullvad-vpn-desktop")
   (version %mullvad-vpn-desktop-version)
   (source
    (origin
     (method url-fetch)
     (uri url)
     (file-name (string-append name "-" version "-" (%current-system) ".deb"))
     (sha256 (base32 hash))))
   (build-system chromium-binary-build-system)
   (arguments
    (list
      ;; There's no point in substitutes.
      #:substitutable? #f
      #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
      #:wrapper-plan
       #~(append
          (list "usr/bin/mullvad"
                "usr/bin/mullvad-daemon"
                "usr/bin/mullvad-exclude")
          (map (lambda (file)
                (string-append "opt/Mullvad VPN/" file))
              '("chrome-sandbox"
                "chrome_crashpad_handler"
                "libEGL.so"
                "libffmpeg.so"
                "libGLESv2.so"
                "libvk_swiftshader.so"
                "libvulkan.so.1"
                "mullvad-gui"
                "resources/libtalpid_openvpn_plugin.so"
                "resources/mullvad-problem-report"
                "resources/mullvad-setup"
                "resources/openvpn")))
      #:install-plan
       #~'(("opt/" "/share")
           ("usr/bin/" "/bin")
           ("usr/lib/" "/lib")
           ("usr/local/share/" "/share")
           ("usr/share/" "/share"))
      #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'unpack-deb
             (lambda* (#:key inputs #:allow-other-keys)
                (invoke "ar" "x" #$source)
                (invoke "rm" "-v" "control.tar.gz"
                                  "debian-binary"
                                  (string-append #$name "-" #$version "-" #$(%current-system) ".deb"))
                (invoke "tar" "xvf" "data.tar.xz")
                (invoke "rm" "-vrf" "data.tar.xz" "./usr/bin/mullvad-problem-report")))
           (add-before 'install 'patch-assets
             (lambda _
                (let* ((bin (string-append #$output "/bin"))
                       (icon (string-append #$output "/share/icons/hicolor/1024x1024/apps/mullvad-vpn.png"))
                       (usr/share "./usr/share")
                       (old-exe "/opt/Mullvad VPN/mullvad-vpn")
                       (exe (string-append bin "/mullvad-vpn")))
                  (patch-shebang (string-append (getcwd) old-exe))
                  (substitute* (string-append usr/share "/applications/mullvad-vpn.desktop")
                   (("^Icon=mullvad-vpn") (string-append "Icon=" icon))
                   (((string-append "^Exec=" old-exe)) (string-append "Exec=" exe))))))
           (add-before 'install-wrapper 'symlink-entrypoint
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (exe (string-append bin "/mullvad-vpn"))
                     (daemon-exe (string-append bin "/mullvad-daemon"))
                     (share (string-append #$output "/share/Mullvad VPN"))
                     (share/resources (string-append share "/resources"))
                     (target (string-append share "/mullvad-vpn")))
                (symlink (string-append share "/resources/mullvad-problem-report")
                         (string-append bin "/mullvad-problem-report"))
                (symlink target exe)
                (wrap-program exe
                  `("MULLVAD_DISABLE_UPDATE_NOTIFICATION" = ("1"))
                  `("LD_LIBRARY_PATH" = (,share)))
                (wrap-program daemon-exe
                  `("MULLVAD_RESOURCE_DIR" = (,share/resources)))))))))
   (native-inputs (list tar))
   (inputs
     (list iputils
           libnotify))
   (synopsis "The Mullvad VPN client app for desktop")
   (supported-systems '("x86_64-linux" "aarch64-linux"))
   (description "This is the VPN client software for the Mullvad VPN service.
For more information about the service, please visit Mullvad's website,
mullvad.net (Also accessible via Tor on this onion service).")
   (home-page "https://mullvad.net")
   (license license:gpl3)))
