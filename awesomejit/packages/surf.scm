;;; GNU Guix --- Functional package management for GNU

;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2015 Dmitry Bogatov <KAction@gnu.org>
;;; Copyright © 2015, 2023 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021 Nikolay Korotkiy <sikmir@disroot.org>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2024 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 cage <cage-dev@twistfold.it>
;;; Copyright © 2025 Benjamin Slade <slade@lambda-y.net>

(define-module (awesomejit packages surf)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public surf
  (let ((commit "11d9703d0437a5e67bf83b5291e69f2580e38ca9")
        (revision "0"))
  (package
    (name "surf")
    (version (git-version "0" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/aartaka/surf")
           (commit commit)))
       (sha256
        (base32 "063a4fnvsjbc61alnbfdpxy0nwhh9ql9j6s9hkdv12713kv932ds"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Use the right file name for dmenu and xprop.
         (add-before 'build 'set-dmenu-and-xprop-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "config.def.h"
               (("dmenu") (search-input-file inputs "/bin/dmenu"))
               (("xprop") (search-input-file inputs "/bin/xprop")))
             #t)))))
    (inputs
     `(("dmenu" ,dmenu)
       ("gcr" ,gcr-3)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk-with-libsoup2)
       ("xprop" ,xprop)))
    (native-inputs
     (list pkg-config))
    (home-page "https://surf.suckless.org/")
    (synopsis "Simple web browser")
    (description
     "Surf is a simple web browser based on WebKit/GTK+.  It is able to
display websites and follow links.  It supports the XEmbed protocol which
makes it possible to embed it in another application.  Furthermore, one can
point surf to another URI by setting its XProperties.")
    (license (list license:expat
                   license:x11)))))
