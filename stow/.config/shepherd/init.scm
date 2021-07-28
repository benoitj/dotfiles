;;(define gpg-agent
;;  (make <service>
;;    #:provides '(gpg-agent)
;;    #:respawn? #t
;;    #:start (make-system-constructor "gpg-connect-agent /bye")
;;    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))
;;
;;(define mcron
;;  (make <service>
;;    #:provides '(mcron)
;;    #:respawn? #t
;;    #:start (make-forkexec-constructor '("mcron"))
;;    #:stop  (make-kill-destructor)))
;;
;;(define syncthing
;;  (make <service>
;;    #:provides '(syncthing)
;;    #:respawn? #t
;;    #:start (make-forkexec-constructor '("syncthing" "-no-browser"))
;;    #:stop  (make-kill-destructor)))

(define goimapnotify
  (make <service>
    #:provides '(goimapnotify)
    #:respawn? #t
    #:start (make-forkexec-constructor '("goimapnotify" "-conf" "/home/benoit/.config/goimapnotify/config.json"))
    #:stop  (make-kill-destructor)))

(define emacs
  (make <service>
    #:provides '(emacs)
    #:respawn? #t
    #:start (make-forkexec-constructor '("emacs" "--fg-daemon"))
    #:stop  (make-kill-destructor)))

(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop  (make-kill-destructor)))

;(register-services gpg-agent mcron syncthing pulseaudio)
(register-services pulseaudio goimapnotify emacs)
(action 'shepherd 'daemonize)

;; Start user services
(for-each start '(pulseaudio goimapnotify emacs))

