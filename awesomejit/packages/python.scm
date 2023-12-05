;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2020, 2021, 2022 Inria
;;;             2023 Benjamin Slade

(define-module (awesomejit packages python)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public python-ical2org
  (package
   (name "python-ical2org")
   (version "0.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "ical2org" version))
     (sha256
      (base32
       "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy"))))
   (build-system python-build-system)
   (propagated-inputs
    (list python-urllib3))
   (home-page
    "https://github.com/ical2org-py/ical2org.py")
   (synopsis "Convert ical .ics file to org-mode")
   (description "This script converts an ical calendar (for instance, as exported from google calendar) into an org-mode document. It is conceived as a replacement of the awk script located here [http://orgmode.org/worg/org-tutorials/org-google-sync.html]. The main difference is that ical2orgpy correctly manages recurring events of 'yearly', 'daily' and 'weekly' types. ical2orgpy duplicates all recurring events falling into a specified time-frame into the exported org-document.")
   (license license:gpl3+)))
