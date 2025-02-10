;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (awesomejit services mullvad)
  #:use-module (awesomejit packages mullvad)
  #:use-module (gnu)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (mullvad-daemon-service-type
            mullvad-daemon-shepherd-services))

(define %mullvad-daemon-accounts
  (list (user-group (name "mullvad") (system? #t))
        (user-account
          (name "mullvad")
          (group "mullvad")
          (system? #t)
          (comment "Mullvad's daemon user account")
          (home-directory "/var/empty")
          (shell "/run/current-system/profile/sbin/nologin")
          (supplementary-groups '("netdev")))))

(define (%mullvad-daemon-activation config)
  (let ((cachedir "/var/cache/mullvad-vpn")
        (datadir "/var/lib/mullvad-vpn")
        (logdir "/var/log/mullvad-vpn"))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam "root"))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (dirs (list #$cachedir #$datadir #$logdir)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    dirs)))))

(define (mullvad-daemon-shepherd-services config)
  (let* ((command (file-append mullvad-vpn-desktop "/bin/mullvad-daemon")))
    (list
      (shepherd-service (provision `(mullvad-early-boot-blocking))
                        (respawn? #f)
                        (one-shot? #t)
                        (documentation "Mullvad early boot network blocker")
                        (start
                         #~(make-forkexec-constructor
                            (list #$command "--initialize-early-boot-firewall")
                            #:user "mullvad"
                            #:group "mullvad"))
                        (stop
                         #~(make-kill-destructor)))
      (shepherd-service (provision `(mullvad-daemon))
                        (requirement '(mullvad-early-boot-blocking
                                       networking))
                        (respawn? #t)
                        (documentation "Mullvad VPN daemon")
                        (start
                         #~(make-forkexec-constructor
                            (list #$command "-v" "--disable-stdout-timestamps")
                            #:user "mullvad"
                            #:group "mullvad"))
                        (stop
                         #~(make-kill-destructor))))))

(define mullvad-daemon-service-type
  (service-type (name 'mullvad-daemon)
                (extensions (list (service-extension profile-service-type
                                                     (lambda _ (list mullvad-vpn-desktop)))
                                  (service-extension account-service-type
                                                     (const %mullvad-daemon-accounts))
                                  (service-extension activation-service-type
                                                     %mullvad-daemon-activation)
                                  (service-extension shepherd-root-service-type
                                                     mullvad-daemon-shepherd-services)))
                (default-value '())
                (description
                 "This service provides a way to run Mullvad's daemon as Shepherd services.")))
