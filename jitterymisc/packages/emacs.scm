;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse John Gildersleve <jessejohngildersleve@zohomail.eu>
;;; Copyright © 2019 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2019, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Declan Tsien <declantsien@riseup.net>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024, 2025 Benjamin Slade <slade@lambda-y.net>
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

(define-module (jitterymisc packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)  
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)     ; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lesstif)   ; motif
  #:use-module (gnu packages linux)     ; alsa-lib, gpm
  #:use-module (gnu packages mail)      ; for mailutils
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages web)       ; for jansson
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (emacs->emacs-more-next))

(define-public emacs-more-next-minimal
  (package
    (inherit emacs-minimal)
    (name "emacs-more-next-minimal")
    (version "31.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/emacs/emacs-"
                                  version ".tar.xz"))
       (sha256
        (base32 "11j59ybvzbkxfsm9zmhj6ixxls2424rhcw5znlr1kj40jl6pk98x"))
        ;; (base32 "1nggbgnns7lvxn68gzlcsgwh3bigvrbn45kh6dqia9yxlqc6zwxk"))
       (patches
        (search-patches "emacs-exec-path.patch"
                        "emacs-fix-scheme-indent-function.patch"
                        "emacs-native-comp-driver-options.patch"
                        "emacs-pgtk-super-key-fix.patch"
                        ;; XXX This commit should already be on 31.0 but
                        ;; without this emacs-next will fail a test.
                        "emacs-zoom-image-test-fix.patch"
                        ))))))

(define-public emacs-head-minimal
  (let ((commit "8661f40ce4d6bce649cb2a564f7c4e766318476c")
        (revision "0"))
   (package
    (inherit emacs-minimal)
    (name "emacs-head-minimal")
    (version (git-version "31.0.50" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nj3a7wsl5piqf6a8wnmfyjbpxp2dwl0r48flv9q624jx4nxfr2p"))
       (patches
        (search-patches "emacs-exec-path.patch"
                        "emacs-fix-scheme-indent-function.patch"
                        "emacs-native-comp-driver-options.patch"
                        "emacs-pgtk-super-key-fix.patch"
                        ;; XXX This commit should already be on 31.0 but
                        ;; without this emacs-next will fail a test.
                        "emacs-zoom-image-test-fix.patch")))))))


(define* (emacs->emacs-more-next emacs #:optional name
                            #:key (version (package-version emacs-more-next-minimal))
                            (source (package-source emacs-more-next-minimal)))
  (package
    (inherit emacs)
    (name (or name
              (and (string-prefix? "emacs" (package-name emacs))
                   (string-append "emacs-next"
                                  (string-drop (package-name emacs)
                                               (string-length "emacs"))))))
    (version version)
    (source source)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'autogen
              (lambda _
                (invoke "sh" "autogen.sh")))
              (delete 'validate-comp-integrity)))))))

(define* (emacs->emacs-head emacs #:optional name
                            #:key (version (package-version emacs-head-minimal))
                            (source (package-source emacs-head-minimal)))
  (package
    (inherit emacs)
    (name (or name
              (and (string-prefix? "emacs" (package-name emacs))
                   (string-append "emacs-head"
                                  (string-drop (package-name emacs)
                                               (string-length "emacs"))))))
    (version version)
    (source source)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'autogen
              (lambda _
                (invoke "sh" "autogen.sh")))
            (delete 'validate-comp-integrity)))))))


;; (define-public emacs-lucid
;;   (package/inherit emacs
;;     (name "emacs-lucid")
;;     (arguments
;;      (substitute-keyword-arguments (package-arguments emacs)
;;        ((#:configure-flags flags #~'())
;;         #~(cons* "--with-x-toolkit=lucid"
;;                  "--without-toolkit-scroll-bars"
;;                  "--with-native-compilation=yes"
;;                  "--with-xft"
;;                  "--with-harfbuzz"
;;                  ;; "--without-m17n-flt"
;;                  "--with-libotf"
;;                  "--without-gsettings"
;;                  "--without-gconf"
;;                  "--with-tree-sitter"
;;                  "--with-modules"
;;                  (delete "--with-native-compilation=aot" #$flags)))))
;;     (inputs
;;      (modify-inputs (package-inputs emacs)
;;        (prepend
;;         libxaw3d ;; for toolkit
;;         cairo dbus giflib harfbuzz libjpeg-turbo libotf
;;         libpng (librsvg-for-system) libtiff libx11 libxft
;;         libxpm pango poppler)))
;;     (native-inputs (list autoconf libfaketime pkg-config texinfo))
;;     (synopsis "Emacs text editor with Lucid toolkit")
;;     (description "This Emacs build uses the Lucid toolkit.")))

(define-public emacs-tune-cflags
  (package
    (inherit emacs)
    (name "emacs-tune-cflags")
    (synopsis "Emacs text editor with CFLAGS tuning.")
    (inputs
     (modify-inputs (package-inputs emacs)
       (prepend
        gtk+ ;; for toolkit
        cairo dbus giflib harfbuzz libjpeg-turbo libotf 
        libpng (librsvg-for-system) libtiff libx11 libxft 
        libxpm pango poppler)))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-native-compilation=yes"
                 "--with-xft"
                 "--with-harfbuzz"
                 ;; "--without-m17n-flt"
                 "--with-libotf"
                 "--without-gsettings"
                 "--without-gconf"
                 "--with-modules"
                 "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
                 "LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto"
                 ;; LDFLAGS? -O2??
                 (delete "--with-native-compilation=aot" #$flags))))
     ;; (list #:phases
     ;;       #~(modify-phases %standard-phases
     ;;           (add-before 'configure 'override-LDFLAGS
     ;;             (lambda _
     ;;               (setenv "LDFLAGS"
     ;;                       "-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto")))
     ;;           (add-before 'configure 'override-CFLAGS
     ;;             (lambda _
     ;;               (setenv "CFLAGS"
     ;;                       "-O2 -mtune=native -march=native -fomit-frame-pointer")))))
     )))

(define-public emacs-lucid-tune-cflags
  (package
    (inherit emacs-lucid)
    (name "emacs-lucid-tune-cflags")
    (synopsis "Emacs text editor with Lucid/Athena toolkit and CFLAGS tuning.")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-lucid)
       ((#:configure-flags flags #~'())
        #~(cons*
           "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
           "LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto"
           #$flags))))))

(define-public emacs-minimal-31
  (package
    (name "emacs-minimal-31")
    (version "31.1")
    ;; Note: When using (replacement …), ensure that comp-native-version-dir
    ;; stays the same across grafts.
    ;; Run `make check-system TESTS=emacs-native-comp' to ensure that grafts
    ;; can meaningfully be applied.
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/emacs/emacs-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "11j59ybvzbkxfsm9zmhj6ixxls2424rhcw5znlr1kj40jl6pk98x"))
              (patches (search-patches "emacs-disable-jit-compilation.patch"
                                       "emacs-exec-path.patch"
                                       "emacs-fix-scheme-indent-function.patch"
                                       "emacs-native-comp-driver-options.patch"
                                       "emacs-native-comp-fix-filenames.patch"
                                       "emacs-native-comp-pin-packages.patch"
                                       "emacs-zoom-image-test-fix.patch"))
              (modules '((guix build utils)))
              (snippet
               '(with-directory-excursion "lisp"
                  ;; Delete the bundled byte-compiled elisp files and generated
                  ;; autoloads.
                  (for-each delete-file
                            (append (find-files "." "\\.elc$")
                                    (find-files "." "loaddefs\\.el$")
                                    (find-files "eshell" "^esh-groups\\.el$")))))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules (%emacs-modules build-system)
      #:configure-flags #~(list "--with-gnutls=no" "--disable-build-details")
      #:make-flags
      #~(list (string-append "SELECTOR=" #$%emacs-selector)
              (let ((release-date "2025-08-14 05:04:03"))
                (string-append "RUN_TEMACS= "
                               #$(this-package-native-input "libfaketime")
                               "/bin/faketime -m -f '" release-date "'"
                               " ./temacs")))
      #:parallel-build? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enable-elogind
            (lambda _
              (substitute* "configure.ac"
                (("libsystemd") "libelogind"))
              (when (file-exists? "configure")
                (delete-file "configure"))))
          (add-after 'unpack 'avoid-sysinfo-call-at-build-time
            (lambda _
              ;; This is a useful trick for reproducibility: when we configured
              ;; with --disable-build-details, (system-name) is nil at build
              ;; time on the lisp side.
              ;; Find those places with strace -k -e sysinfo.
              (substitute* "lisp/jit-lock.el"
                (("\\(condition-case nil \\(load-average\\) \\(error\\)\\)"
                  all)
                 (format #f "(and (system-name) ~a)" all)))))
          (add-after 'unpack 'patch-program-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Substitute "sh" command.
              (substitute* '("src/callproc.c"
                             "lisp/term.el"
                             "lisp/htmlfontify.el"
                             "lisp/mail/feedmail.el"
                             "lisp/obsolete/pgg-pgp.el"
                             "lisp/obsolete/pgg-pgp5.el"
                             "lisp/org/ob-eval.el"
                             "lisp/textmodes/artist.el"
                             "lisp/progmodes/sh-script.el"
                             "lisp/textmodes/artist.el"
                             "lisp/htmlfontify.el"
                             "lisp/term.el")
                (("\"/bin/sh\"")
                 (format #f "~s" (search-input-file inputs "bin/sh"))))
              (substitute* '("lisp/gnus/mm-uu.el"
                             "lisp/gnus/nnrss.el"
                             "lisp/mail/blessmail.el")
                (("\"#!/bin/sh\\\n\"")
                 (format #f "\"#!~a~%\"" (search-input-file inputs "bin/sh"))))
              (substitute* '("lisp/jka-compr.el"
                             "lisp/man.el")
                (("\"sh\"")
                 (format #f "~s" (search-input-file inputs "bin/sh"))))

              ;; Substitute "awk" command.
              (substitute* '("lisp/gnus/nnspool.el"
                             "lisp/org/ob-awk.el"
                             "lisp/man.el")
                (("\"awk\"")
                 (format #f "~s" (search-input-file inputs "bin/awk"))))

              ;; Substitute "find" command.
              (substitute* '("lisp/gnus/gnus-search.el"
                             "lisp/obsolete/nnir.el"
                             "lisp/progmodes/executable.el"
                             "lisp/progmodes/grep.el"
                             "lisp/filecache.el"
                             "lisp/ldefs-boot.el"
                             "lisp/mpc.el")
                (("\"find\"")
                 (format #f "~s" (search-input-file inputs "bin/find"))))

              ;; Substitute "sed" command.
              (substitute* "lisp/org/ob-sed.el"
                (("org-babel-sed-command \"sed\"")
                 (format #f "org-babel-sed-command ~s"
                         (search-input-file inputs "bin/sed"))))
              (substitute* "lisp/man.el"
                (("Man-sed-command \"sed\"")
                 (format #f "Man-sed-command ~s"
                         (search-input-file inputs "bin/sed"))))

              (substitute* "lisp/doc-view.el"
                (("\"(gs|dvipdf|ps2pdf|pdftotext)\"" all what)
                 (let ((replacement (false-if-exception
                                     (search-input-file
                                      inputs
                                      (string-append "/bin/" what)))))
                   (if replacement
                       (string-append "\"" replacement "\"")
                       all))))
              ;; Make sure Tramp looks for binaries in the right places on
              ;; remote Guix System machines, where 'getconf PATH' returns
              ;; something bogus.
              (substitute* "lisp/net/tramp.el"
                ;; Patch the line after "(defcustom tramp-remote-path".
                (("\\(tramp-default-remote-path")
                 (format
                  #f "(tramp-default-remote-path ~s ~s ~s ~s ~s ~s ~s "
                  "/run/privileged/bin"
                  "~/.guix-profile/bin" "~/.guix-profile/sbin"
                  "~/.guix-home/bin" "~/.guix-home/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin")))

              ;; Make sure Man and ffap looks for C header files in the right
              ;; places.
              (substitute* '("lisp/man.el" "lisp/ffap.el")
                (("\"/usr/include\" \"/usr/local/include\"" line)
                 (string-join
                  (list line
                        "\"~/.guix-profile/include\""
                        "\"~/.guix-home/include\""
                        "\"/run/current-system/profile/include\"")
                  " ")))

              ;; match ".gvfs-fuse-daemon-real" and ".gvfsd-fuse-real"
              ;; respectively when looking for GVFS processes.
              (substitute* "lisp/net/tramp-gvfs.el"
                (("\\(tramp-process-running-p \"(.*)\"\\)" all process)
                 (format #f "(or ~a (tramp-process-running-p ~s))"
                         all (string-append "." process "-real"))))))
          (add-before 'configure 'fix-/bin/pwd
            (lambda _
              ;; Use `pwd', not `/bin/pwd'.
              (substitute* (find-files "." "^Makefile\\.in$")
                (("/bin/pwd")
                 "pwd"))))
          (add-after 'unpack 'fix-tests
            (lambda* (#:key tests? inputs #:allow-other-keys)
              (when tests?
                (substitute* "test/src/process-tests.el"
                  (("/bin//sh") (search-input-file inputs "bin/sh")))
                (substitute* "test/lisp/eshell/em-script-tests.el"
                  (("/usr/bin/env") (search-input-file inputs "bin/env"))))))
          (add-before 'configure 'install-c-source
            (lambda _
              (let ((dest (string-append #$output:doc "/share/emacs/c-source"))
                    (lisp-dir (string-append #$output:doc
                                             "/share/emacs/site-lisp")))
                (mkdir-p dest)
                (copy-recursively "src" dest)
                (mkdir-p lisp-dir)
                (with-output-to-file (string-append lisp-dir
                                                    "/guix-emacs-c-source.el")
                  (lambda ()
                    (display
                     (string-append
                      "(setq find-function-C-source-directory \"" dest "\")\n\n"
                      "(provide 'guix-emacs-c-source)")))))))
          (add-after 'install 'install-site-start
            ;; Use 'guix-emacs' in "site-start.el", which is used autoload the
            ;; Elisp packages found in EMACSLOADPATH.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (lisp-dir (string-append out "/share/emacs/site-lisp"))
                     (emacs    (string-append out "/bin/emacs")))

                ;; This is duplicated from emacs-utils to prevent coupling.
                (define* (emacs-byte-compile-directory dir)
                  (let ((expr `(progn
                                (setq byte-compile-debug t)
                                (byte-recompile-directory
                                 (file-name-as-directory ,dir) 0 1))))
                    (invoke emacs "--quick" "--batch"
                            (format #f "--eval=~s" expr))))

                (copy-file #$(local-file
                              (search-auxiliary-file "emacs/guix-emacs.el"))
                           (string-append lisp-dir "/guix-emacs.el"))
                (with-output-to-file (string-append lisp-dir "/site-start.el")
                  (lambda ()
                    (display
                     (string-append
                      "(when (require 'guix-emacs nil t)\n"
                      "  (guix-emacs-autoload-packages 'no-reload)\n"
                      "  (advice-add 'package-load-all-descriptors"
                      " :after #'guix-emacs-load-package-descriptors))\n\n"
                      ";; The file guix-emacs-c-source.el is available from the"
                      " 'doc' output.\n"
                      "(require 'guix-emacs-c-source nil t)"))))
                ;; Remove the extraneous subdirs.el file, as it causes Emacs to
                ;; add recursively all the the sub-directories of a profile's
                ;; share/emacs/site-lisp union when added to EMACSLOADPATH,
                ;; which leads to conflicts.
                (delete-file (string-append lisp-dir "/subdirs.el"))
                ;; Byte compile the site-start files.
                (emacs-byte-compile-directory lisp-dir))))
          (add-after 'install 'wrap-emacs-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lisp-dirs (find-files (string-append out "/share/emacs")
                                            "^lisp$"
                                            #:directories? #t)))
                (for-each
                 (lambda (prog)
                   (wrap-program prog
                     ;; Some variants rely on uname being in PATH for Tramp.
                     ;; Tramp paths can't be hardcoded, because they need to
                     ;; be portable.
                     `("PATH" suffix
                       ,(map dirname
                             (list (search-input-file inputs "/bin/gzip")
                                   ;; for coreutils
                                   (search-input-file inputs "/bin/yes"))))
                     `("EMACSLOADPATH" suffix ,lisp-dirs)))
                 (find-files (string-append out "/bin")
                             ;; Matches versioned and unversioned emacs binaries.
                             ;; We don't patch emacsclient, because it takes its
                             ;; environment variables from emacs.
                             ;; Likewise, we don't need to patch helper binaries
                             ;; like etags, ctags or ebrowse.
                             "^emacs(-[0-9]+(\\.[0-9]+)*)?$")))))
          (add-after 'wrap-emacs-paths 'undo-double-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Directly copy emacs-X.Y to emacs, so that it is not wrapped
              ;; twice.  This also fixes a minor issue, where WMs would not be
              ;; able to track emacs back to emacs.desktop.
              (with-directory-excursion (assoc-ref outputs "out")
                (copy-file
                 (car (find-files "bin" "^emacs-([0-9]+\\.)+[0-9]+$"))
                 "bin/emacs")))))))
    (inputs (list bash-minimal coreutils findutils gawk gzip ncurses sed))
    (native-inputs (list autoconf libfaketime pkg-config texinfo))
    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible text editor (minimal build for byte-compilation)")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)
    (native-search-paths
     (list (search-path-specification
            (variable "EMACSLOADPATH")
            (files '("share/emacs/site-lisp")))
           (search-path-specification
            (variable "EMACSNATIVELOADPATH")
            (files '("lib/emacs/native-site-lisp")))
           (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))
           ;; Most variants support tree-sitter, so let's include it here.
           (search-path-specification
            (variable "TREE_SITTER_GRAMMAR_PATH")
            (files '("lib/tree-sitter")))))
    (properties `((upstream-name . "emacs")))))

(define-public emacs-no-x-31
  (package/inherit emacs-minimal-31
    (name "emacs-no-x-31")
    (synopsis "The extensible, customizable, self-documenting text
editor (console only)")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags flags #~'())
        #~(cons* "--with-modules" "--with-native-compilation=aot"
                 (delete "--with-gnutls=no" #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'set-paths 'set-libgccjit-path
              (lambda* (#:key inputs #:allow-other-keys)
                (define (first-subdirectory/absolute directory)
                  (let ((files (scandir
                                directory
                                (lambda (file)
                                  (and (not (member file '("." "..")))
                                       (file-is-directory? (string-append
                                                            directory "/"
                                                            file)))))))
                    (and (not (null? files))
                         (string-append directory "/" (car files)))))
                (let* ((libgccjit-libdir
                        (first-subdirectory/absolute ;; version
                         (first-subdirectory/absolute ;; host type
                          (search-input-directory inputs "lib/gcc")))))
                  (setenv "LIBRARY_PATH"
                          (string-append (getenv "LIBRARY_PATH")
                                         ":" libgccjit-libdir)))))
            (add-after 'unpack 'patch-compilation-driver
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "lisp/emacs-lisp/comp.el"
                  (("\\(defcustom native-comp-driver-options nil")
                   (format
                    #f "(defcustom native-comp-driver-options '(~@{~s~^ ~})"
                    (string-append
                     "-B" (dirname (search-input-file inputs "/bin/nm")))
                    (string-append
                     "-B" (dirname (search-input-file inputs "/lib/libc.so")))
                    (string-append
                     "-B" (dirname (search-input-file inputs "/lib/libgccjit.so")))
                    (string-append
                     "-B" (string-append
                           (dirname
                            (search-input-file inputs "/lib/libgccjit.so"))
                           "/gcc")))))
                (substitute* "lisp/emacs-lisp/disass.el"
                  (("\"objdump\"")
                   (string-append "\"" (search-input-file inputs "/bin/objdump") "\"")))))
            (add-after 'build 'build-trampolines
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "trampolines" make-flags)))
            (add-after 'validate-runpath 'validate-comp-integrity
              (lambda* (#:key outputs #:allow-other-keys)
                #$(cond
                   ((%current-target-system)
                    #~(display "Cannot validate native-comp on cross builds.\n"))
                   ((member (%current-system) '("armhf-linux" "i686-linux"))
                    #~(display "Integrity test is broken on armhf.\n"))
                   (else
                    #~(invoke
                       (string-append (assoc-ref outputs "out") "/bin/emacs")
                       "--batch"
                       "--load"
                       #$(local-file
                          (search-auxiliary-file "emacs/comp-integrity.el"))
                       "-f" "ert-run-tests-batch-and-exit")))))))))
    (inputs
     (modify-inputs inputs
       (prepend gnutls
                ;; For native compilation
                libgccjit

                ;; Avoid Emacs's limited movemail substitute that retrieves POP3
                ;; email only via insecure channels.
                ;; This is not needed for (modern) IMAP.
                mailutils

                acl
                alsa-lib
                elogind
                ghostscript
                gpm
                jansson
                lcms
                libice
                libselinux
                libsm
                libxml2
                m17n-lib
                sqlite
                tree-sitter
                zlib)))))

(define-public emacs-31release-lucid-tune-cflags
  (package/inherit emacs-no-x-31
    (name "emacs-31release-lucid-tune-cflags")
    (synopsis
     "The extensible, customizable, self-documenting text editor (with Lucid/Athena toolkit) [and CFLAG tuning]")
    (inputs (modify-inputs (package-inputs emacs)
              (delete "gtk+")
              (prepend libxaw)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments emacs-no-x)
       ((#:configure-flags flags #~'())
        #~(cons "--with-x-toolkit=lucid"
                "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
                "LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto"
                #$flags))))))



;; (define-public emacs-31release-lucid-tune-cflags
;;   (package
;;     (inherit emacs-lucid)
;;     (name "emacs-31release-lucid-tune-cflags")
;;     (version "31.1")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "mirror://gnu/emacs/emacs-"
;;                                   version ".tar.xz"))
;;               (sha256
;;                (base32
;;                 "11j59ybvzbkxfsm9zmhj6ixxls2424rhcw5znlr1kj40jl6pk98x"))))
;;     (synopsis "Emacs text editor with Lucid/Athena toolkit with CFLAGS tuning.")
;;     (arguments
;;      (substitute-keyword-arguments (package-arguments emacs-lucid)
;;        ((#:configure-flags flags #~'())
;;         #~(cons*
;;            "--with-x-toolkit=lucid"
;;            "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
;;            "LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto"
;;            #$flags))))))

(define-public emacs-pgtk-tune-cflags
  (package
    (inherit emacs-pgtk)
    (name "emacs-pgtk-tune-cflags")
    (synopsis "Emacs text editor built with CFLAGS tuning and graphical UI purely in terms
of GTK (for use under Wayland).")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:configure-flags flags #~'())
        #~(cons* "--with-native-compilation=yes"
                 ;; "--with-xft"
                 ;; "--with-harfbuzz"
                 ;; "--without-m17n-flt"
                 ;; "--with-libotf"
                 ;; "--without-gsettings"
                 ;; "--without-gconf"
                 ;; "--with-modules"
                 "CFLAGS=-O2 -mtune=native -march=native -fomit-frame-pointer"
                 "LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto"
                 ;; LDFLAGS? -O2??
                 (delete "--with-native-compilation=aot" #$flags))))
     ;; (list #:phases
     ;;       #~(modify-phases %standard-phases
     ;;           (add-before 'configure 'override-LDFLAGS
     ;;             (lambda _
     ;;               (setenv "LDFLAGS"
     ;;                       "-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,-z,relro -Wl,-z,now          -Wl,-z,pack-relative-relocs -flto=auto")))
     ;;           (add-before 'configure 'override-CFLAGS
     ;;             (lambda _
     ;;               (setenv "CFLAGS"
     ;;                       "-O2 -mtune=native -march=native -fomit-frame-pointer")))))
     )))

ö


(define-public emacs-next-lucid (emacs->emacs-more-next emacs-lucid))
(define-public emacs-next-lucid-tune-cflags (emacs->emacs-more-next emacs-lucid-tune-cflags))
(define-public emacs-next-tune-cflags (emacs->emacs-more-next emacs-tune-cflags))

;; (define-public emacs-lucid-athena-31 emacs-31release-lucid-tune-cflags)

;; (define-public emacs-next-xwidgets-tune-cflags (emacs->emacs-more-next emacs-xwidgets-tune-cflags))

;; (define-public emacs-head-lucid (emacs->emacs-head emacs-lucid))
;; (define-public emacs-head-lucid-tune-cflags (emacs->emacs-head emacs-lucid-tune-cflags))

;; (define-public emacs-head-xwidgets-tune-cflags (emacs->emacs-head emacs-xwidgets-tune-cflags))

