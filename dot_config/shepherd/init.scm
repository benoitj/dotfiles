;;(define gpg-agent
;;  (make <service>
;;    #:provides '(gpg-agent)
;;    #:respawn? #t
;;    #:start (make-system-constructor "gpg-connect-agent /bye")
;;    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))

;;/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

(register-services
  (make <service>
    #:provides '(compton)
    #:respawn? #t
    #:start (make-forkexec-constructor '("compton"))
    #:stop  (make-kill-destructor))


  (make <service>
    #:provides '(dunst)
    #:respawn? #t
    #:start (make-forkexec-constructor '("dunst"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(dwmstatus)
    #:respawn? #t
    #:start (make-forkexec-constructor '("dwmstatus"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(emacs)
    #:respawn? #t
    #:start (make-forkexec-constructor '("emacs" "--fg-daemon"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(goimapnotify)
    #:respawn? #t
    #:start (make-forkexec-constructor '("goimapnotify" "-debug" "-conf" "/home/benoit/.config/goimapnotify/config.json"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(mcron)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(syncthing)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing" "--no-browser"))
    #:stop  (make-kill-destructor))

  (make <service>
    #:provides '(unclutter)
    #:respawn? #t
    #:start (make-forkexec-constructor '("unclutter"))
    #:stop  (make-kill-destructor)))

;(register-services gpg-agent mcron syncthing pulseaudio)
;(register-services
; compton
; dunst
; dwmstatus
; emacs
; goimapnotify
; polkit
; pulseaudio
; syncthing
; unclutter
; )
(action 'shepherd 'daemonize)

;; Start user services
(for-each start '(
                  compton
                  dunst
                  dwmstatus
                  ;emacs
                  goimapnotify
                  mcron
                  ;pulseaudio
                  syncthing
                  unclutter
                  ))
