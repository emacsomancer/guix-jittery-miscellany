;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (awesomejit services mullvad)
  #:use-module (awesomejit packages mullvad)
  #:use-module (gnu)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (mullvad-daemon-service-type
            mullvad-daemon-shepherd-services

            mullvad-daemon-user
            mullvad-daemon-group

            mullvad-daemon-configuration
            mullvad-daemon-configuration?
            mullvad-daemon-configuration-fields
            mullvad-daemon-configuration-mullvad-vpn-desktop
            mullvad-daemon-configuration-user
            mullvad-daemon-configuration-group
            mullvad-daemon-configuration-supplementary-groups
            mullvad-daemon-configuration-cachedir
            mullvad-daemon-configuration-configdir
            mullvad-daemon-configuration-datadir
            mullvad-daemon-configuration-logdir))

(define %default-mullvad-daemon-supplementary-groups
  '("netdev" "users"))

(define list-of-strings?
  (list-of string?))

(define-maybe/no-serialization string)

(define-configuration/no-serialization mullvad-daemon-configuration
  (mullvad-vpn-desktop
   (package mullvad-vpn-desktop)
   "The mullvad-vpn-desktop package that will be installed in the system profile.")
  (user
   (maybe-string)
   "The user that will be used to run @command{mullvad-daemon}.  When unset the
root user will be used.")
  (group
   (maybe-string)
   "The group that will be used to run @command{mullvad-daemon}.  When unset the
root group will be used.")
  (supplementary-groups
   (list-of-strings '())
   "A list of supplementary groups that will be created and to which the
configured @code{user} will be added.  The @code{netdev} and @code{users}
are always appended to this fields value. This field is ignored when the
user is root.")
  (configdir
   (string "/etc/mullvad-vpn")
   "The directory where mullvad-daemon will write its configs.")
  (cachedir
   (string "/var/cache/mullvad-vpn")
   "The directory where mullvad-daemon will write cache data.")
  (datadir
   (string "/var/lib/mullvad-vpn")
   "The directory where mullvad-daemon will write state.")
  (logdir
   (string "/var/log/mullvad-vpn")
   "The directory where mullvad-daemon will write logs."))

(define (mullvad-daemon-user config)
 (define user (mullvad-daemon-configuration-user config))
 (if (maybe-value-set? user)
     user
     "root"))

(define (mullvad-daemon-group config)
  (define user (mullvad-daemon-configuration-user config))
  (define group (mullvad-daemon-configuration-group config))
  (if (and (maybe-value-set? user)
           (maybe-value-set? group))
      group
      "root"))

(define (mullvad-daemon-accounts config)
  (let ((user (mullvad-daemon-user config))
        (group (mullvad-daemon-group config))
        (supplementary-groups
         (mullvad-daemon-configuration-supplementary-groups config)))

    (if (string=? user "root")
        '()
        (append
         (map (lambda (name)
                (user-group (name name) (system? #t)))
              supplementary-groups)
         (list (user-group (name group) (system? #t))
               (user-account
                (name user)
                (group group)
                (system? #t)
                (comment "Mullvad's daemon user account")
                (home-directory "/var/empty")
                (shell "/run/current-system/profile/sbin/nologin")
                (supplementary-groups
                 (append supplementary-groups
                         %default-mullvad-daemon-supplementary-groups))))))))

(define (mullvad-daemon-activation config)
  (let ((user (mullvad-daemon-user config))
        (configdir (mullvad-daemon-configuration-configdir config))
        (cachedir (mullvad-daemon-configuration-cachedir config))
        (datadir (mullvad-daemon-configuration-datadir config))
        (logdir (mullvad-daemon-configuration-logdir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam #$user))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (dirs (list #$configdir #$cachedir #$datadir #$logdir)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    dirs)))))

(define (mullvad-daemon-shepherd-services config)
  (let* ((user (mullvad-daemon-user config))
         (group (mullvad-daemon-group config))
         (package
          (mullvad-daemon-configuration-mullvad-vpn-desktop config))
         (command (file-append package "/bin/mullvad-daemon")))
    (list
      (shepherd-service (provision `(mullvad-early-boot-blocking))
                        (respawn? #f)
                        (one-shot? #t)
                        (documentation "Mullvad early boot network blocker")
                        (start
                         #~(make-forkexec-constructor
                            (list #$command "--initialize-early-boot-firewall")
                            #:user #$user
                            #:group #$group))
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
                            #:user #$user
                            #:group #$group))
                        (stop
                         #~(make-kill-destructor))))))

(define mullvad-daemon-service-type
  (service-type (name 'mullvad-daemon)
                (extensions (list (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list
                                                        (mullvad-daemon-configuration-mullvad-vpn-desktop config))))
                                  (service-extension account-service-type
                                                     mullvad-daemon-accounts)
                                  (service-extension activation-service-type
                                                     mullvad-daemon-activation)
                                  (service-extension shepherd-root-service-type
                                                     mullvad-daemon-shepherd-services)))
                (default-value (mullvad-daemon-configuration))
                (description
                 "This service provides a way to run Mullvad's daemon as Shepherd services.")))
