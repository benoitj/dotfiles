;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(define audio-manifest
  (specifications->manifest
   '("abcde"
     ;;"beets"
     ;; TODO borgmatic 1.5.15-1
     "cdparanoia"
     "chromaprint"
     "mpd"
     "python-pyacoustid"
     "python-requests"
     ;; TODO mpeg4 metadata setter "atomicparsley"
     )))

(define tools-manifest
  (specifications->manifest
   '(
     "borg"
     ;; TODO missing "borgmatic"
     "browserpass-native" ;; requires to register in firefox using: make -C $(guix build browserpass-native)/lib/browserpass hosts-firefox-user
     ;; TODO "cups"
     ;; TODO cnijfilter2-mg7500 5.00-1
     ;; TODO"czkawka"
     "isync"
;;     "git:send-email"
     "msmtp"
     "notmuch"
     "transmission" ;; out and gui
     "youtube-dl"
     )))

(define web-manifest
  (specifications->manifest
   '(
     ;; TODO "amfora"
     ;; chromium 91.0.4472.164-1
     )))

(concatenate-manifests
 (list
  audio-manifest
  tools-manifest
  web-manifest))
