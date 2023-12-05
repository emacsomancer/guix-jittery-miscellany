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
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public python-x-wr-timezone
  (package
   (name "python-x-wr-timezone")
   (version "2.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "x-wr-timezone" version))
     (sha256
      (base32
       "00k4l800j2d3zfqhxjlzjk0b6493wqhmg4r58xsp12sx2ia2sxla"))))
   (build-system python-build-system)
   (arguments
    ;; Broken tests or cyclic dependecies with other packages.
    '(#:phases
      (modify-phases %standard-phases
                     (delete 'sanity-check))
      #:tests? #f))
   (propagated-inputs
    (list python-x-wr-timezone))
   (home-page
    "https://github.com/niccokunzmann/x-wr-timezone")
   (synopsis "Handling of non-standard X-WR-TIMEZONE icalendar property in Python and Command Line")
   (description "Some calendar providers introduce the non-standard X-WR-TIMEZONE parameter to ICS calendar files. Strict interpretations according to RFC 5545 ignore the X-WR-TIMEZONE parameter. This causes the times of the events to differ from those which make use of X-WR-TIMEZONE. This module aims to bridge the gap by converting calendars using X-WR-TIMEZONE to a strict RFC 5545 calendars.")
   (license license:lgpl3+)))


(define-public python-recurring-ical-events
  (package
   (name "python-recurring-ical-events")
   (version "2.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "recurring_ical_events" version))
     (sha256
      (base32
       "00k4l800j2d3zfqhxjlzjk0b6493wqhmg4r58xsp12sx2ia2sxla"))))
   (build-system python-build-system)
   (arguments
    ;; Broken tests or cyclic dependecies with other packages.
    '(#:phases
      (modify-phases %standard-phases
                     (delete 'sanity-check))
      #:tests? #f))
   (home-page
    "https://github.com/niccokunzmann/python-recurring-ical-events")
   (synopsis "Python library for recurrence of ical events based on icalendar")
   (description "ICal has some complexity to it: Events, TODOs and Journal entries can be repeated, removed from the feed and edited later on. This tool takes care of these circumstances.")
   (license license:lgpl3+)))

(define-public python-ical2orgpy
  (package
   (name "python-ical2orgpy")
   (version "0.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "ical2orgpy" version))
     (sha256
      (base32
       "0a2ai78jzn250s1dlsimsfp6035421nrabj08xdlgsh7mb82gkap"))))
   (build-system python-build-system)
   (arguments
    ;; Broken tests or cyclic dependecies with other packages.
    '(#:phases
      (modify-phases %standard-phases
                     (delete 'sanity-check))
      #:tests? #f))
   (propagated-inputs
    (list python-click
          python-icalendar
          python-pytz
          python-tzlocal
          python-recurring-ical-events))
   (home-page
    "https://github.com/ical2org-py/ical2org.py")
   (synopsis "Convert ical .ics file to org-mode")
   (description "This python script converts an ical calendar (for instance, as exported from google calendar) into an org-mode document. It is conceived as a replacement of the awk script located at https://orgmode.org/worg/org-tutorials/org-google-sync.html. The main difference is that ical2orgpy correctly manages recurring events of 'yearly', 'daily' and 'weekly' types. ical2orgpy duplicates all recurring events falling into a specified time-frame into the exported org-document.")
   (license license:gpl3+)))
