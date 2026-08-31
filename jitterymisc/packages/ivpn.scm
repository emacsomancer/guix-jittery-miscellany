(define-module (ivpn)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages vpn)) ; For wireguard-tools / openvpn if needed

(define-public ivpn-cli
  (package
    (name "ivpn-cli")
    (version "3.15.13") 
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ivpn/desktop-app")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qlcmnayaxbyv5jpxqcyca8y52j4avlg7jvvn2qkg574sbn6yhgl")))) 
    (build-system go-build-system)
    (arguments
     '(#:import-path "://github.com"
       #:unpack-path "github.com/ivpn/desktop-app"
       #:phases
       (modify-phases %standard-phases
         ;; Subvert hardcoded FHS paths if necessary
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Example: If daemon or CLI looks for /opt/ivpn/etc, 
               ;; you can substitute it here using substitute*
               #t))))))
    (inputs
     `(("wireguard-tools" ,wireguard-tools)
       ("openvpn" ,openvpn)))
    (home-page "https://github.com/ivpn/desktop-app")
    (synopsis "Official Command Line Interface and Daemon for IVPN")
    (description
     "This package builds the Go-based background daemon (ivpn-service) 
and the command-line control interface (ivpn) for the IVPN service.")
    (license gpl3)))
