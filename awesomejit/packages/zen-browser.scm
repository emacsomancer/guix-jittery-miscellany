(define-module (awesomejit packages zen-browser)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix build utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (zen-browser
            zen-browser-twilight))

(define zen-browser
  (package
    (name "zen-browser")
    (version "1.7b")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://github.com/zen-browser/desktop/releases/download/"
                version
                "/zen.linux-x86_64.tar.bz2"))
        (sha256
          (base32 "1vvzf7vy0927zm9hds7720drrwi4qfny99h65vn8x39m8zx6w9xp"))))
    (build-system copy-build-system)
    (arguments
      (list #:install-plan
            #~'(("." "lib/zen"))
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'patch-elf
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((ld.so (string-append #$(this-package-input "glibc")
                                                #$(glibc-dynamic-linker)))
                          (rpath (string-join
                                   (cons*
                                     (string-append #$output "/lib/zen")
                                     (string-append #$(this-package-input "gtk+") "/share")
                                     (map
                                       (lambda (input)
                                         (string-append (cdr input) "/lib"))
                                       inputs))
                                   ":")))
                      ;; Got this proc from hako's Rosenthal, thanks
                      (define (patch-elf file)
                        (format #t "Patching ~a ..." file)
                        (unless (string-contains file ".so")
                          (invoke "patchelf" "--set-interpreter" ld.so file))
                        (invoke "patchelf" "--set-rpath" rpath file)
                        (display " done\n"))
                      (for-each
                        (lambda (binary)
                          (patch-elf binary))
                        (append
                          (map
                            (lambda (binary)
                              (string-append #$output "/lib/zen/" binary))
                            '("glxtest" "updater" "vaapitest" "zen" "zen-bin" "pingsender"))
                          (find-files (string-append #$output "/lib/zen") ".*\\.so.*"))))))
                (add-after 'patch-elf 'install-bin
                  (lambda _
                    (let* ((zen (string-append #$output "/lib/zen/zen"))
                           (bin-zen (string-append #$output "/bin/zen")))
                      (mkdir (string-append #$output "/bin"))
                      (symlink zen bin-zen))))
                (add-after 'install-bin 'install-desktop
                  (lambda _
                    (let* ((share-applications (string-append #$output "/share/applications"))
                           (desktop (string-append share-applications "/zen.desktop")))
                      (mkdir-p share-applications)
                      (make-desktop-entry-file desktop
                        #:name "Zen Browser"
                        #:icon "zen"
                        #:type "Application"
                        #:comment #$(package-synopsis this-package)
                        #:exec (string-append #$output "/bin/zen %u")
                        #:keywords '("Internet" "WWW" "Browser" "Web" "Explorer")
                        #:categories '("Network" "Browser")
                        ; #:actions '("new-window" "new-private-window" "profilemanager")
                        #:mime-type '("text/html"
                                      "text/xml"
                                      "application/xhtml+xml"
                                      "x-scheme-handler/http"
                                      "x-scheme-handler/https"
                                      "application/x-xpinstall"
                                      "application/pdf"
                                      "application/json")
                        #:startup-w-m-class "zen")))))))
    (native-inputs (list patchelf))
    (inputs (list alsa-lib
                  at-spi2-core
                  cairo
                  dbus
                  dbus
                  eudev
                  fontconfig
                  freetype
                  gcc-toolchain
                  gdk-pixbuf
                  glib
                  glib
                  glibc
                  gtk+
                  libnotify
                  libva
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxrandr
                  libxrender
                  nspr
                  nss
                  pango
                  pulseaudio))
    (home-page "https://zen-browser.app/")
    (synopsis "Experience tranquillity while browsing the web without people
tracking you!")
    (description "Beautifully designed, privacy-focused, and packed with features.
We care about your experience, not your data.")
    (license (list license:mpl2.0))))


(define zen-browser-twilight
  (package
    (name "zen-browser-twilight")
    (version "1.7t")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
                "https://github.com/zen-browser/desktop/releases/download/"
                "twilight"
                "/zen.linux-x86_64.tar.bz2"))
        (sha256
          (base32 "0s9s075hjk501c5n3xdzdm03ai93xbc488xw9f4sm0c303mx4pmy"))))
    (build-system copy-build-system)
    (arguments
      (list #:install-plan
            #~'(("." "lib/zen"))
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'install 'patch-elf
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((ld.so (string-append #$(this-package-input "glibc")
                                                #$(glibc-dynamic-linker)))
                          (rpath (string-join
                                   (cons*
                                     (string-append #$output "/lib/zen")
                                     (string-append #$(this-package-input "gtk+") "/share")
                                     (map
                                       (lambda (input)
                                         (string-append (cdr input) "/lib"))
                                       inputs))
                                   ":")))
                      ;; Got this proc from hako's Rosenthal, thanks
                      (define (patch-elf file)
                        (format #t "Patching ~a ..." file)
                        (unless (string-contains file ".so")
                          (invoke "patchelf" "--set-interpreter" ld.so file))
                        (invoke "patchelf" "--set-rpath" rpath file)
                        (display " done\n"))
                      (for-each
                        (lambda (binary)
                          (patch-elf binary))
                        (append
                          (map
                            (lambda (binary)
                              (string-append #$output "/lib/zen/" binary))
                            '("glxtest" "updater" "vaapitest" "zen" "zen-bin" "pingsender"))
                          (find-files (string-append #$output "/lib/zen") ".*\\.so.*"))))))
                (add-after 'patch-elf 'install-bin
                  (lambda _
                    (let* ((zen (string-append #$output "/lib/zen/zen"))
                           (bin-zen (string-append #$output "/bin/zen-twilight")))
                      (mkdir (string-append #$output "/bin"))
                      (symlink zen bin-zen))))
                (add-after 'install-bin 'install-desktop
                  (lambda _
                    (let* ((share-applications (string-append #$output "/share/applications"))
                           (desktop (string-append share-applications "/zen-twilight.desktop")))
                      (mkdir-p share-applications)
                      (make-desktop-entry-file desktop
                        #:name "Zen Browser (Twilight)"
                        #:icon "zen"
                        #:type "Application"
                        #:comment #$(package-synopsis this-package)
                        #:exec (string-append #$output "/bin/zen-twilight %u")
                        #:keywords '("Internet" "WWW" "Browser" "Web" "Explorer")
                        #:categories '("Network" "Browser")
                        ; #:actions '("new-window" "new-private-window" "profilemanager")
                        #:mime-type '("text/html"
                                      "text/xml"
                                      "application/xhtml+xml"
                                      "x-scheme-handler/http"
                                      "x-scheme-handler/https"
                                      "application/x-xpinstall"
                                      "application/pdf"
                                      "application/json")
                        #:startup-w-m-class "zen-twilight")))))))
    (native-inputs (list patchelf))
    (inputs (list alsa-lib
                  at-spi2-core
                  cairo
                  dbus
                  dbus
                  eudev
                  fontconfig
                  freetype
                  gcc-toolchain
                  gdk-pixbuf
                  glib
                  glib
                  glibc
                  gtk+
                  libnotify
                  libva
                  libx11
                  libxcb
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxi
                  libxrandr
                  libxrender
                  nspr
                  nss
                  pango
                  pulseaudio))
    (home-page "https://zen-browser.app/")
    (synopsis "Experience tranquillity while browsing the web without people
tracking you!")
    (description "Beautifully designed, privacy-focused, and packed with features.
We care about your experience, not your data.")
    (license (list license:mpl2.0))))
