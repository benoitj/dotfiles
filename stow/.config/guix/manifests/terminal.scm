;; -*- mode: guix-scheme-*-

(specifications->manifest
 '(
   ;; sound tools
   "alsa-plugins:out" ;; required by qutebrowser to play audio
   "alsa-plugins:pulseaudio" ;; required by qutebrowser to play audio. missing LD_LIBRARY_PATH
   "alsa-utils"
   "pulsemixer"
   "pamixer"
   "gst-plugins-base"
   "gst-plugins-good"
   "gst-plugins-bad"
   "gst-plugins-ugly"
   "gstreamer"

   ;; network tools
   "irssi"
   "isync"
   "profanity"
   "tig"

   ;; devices
   "dosfstools"
   "cdrdao"
   "cdparanoia"
   "cd-discid"
   ;; ?_cnijfilter2-mg7500 5.00-1

   ;; audio / video / media
   "ffmpeg"
   "flac"
   "gawk"
   "chromaprint"
   "fluidsynth"
   "imagemagick"
   "opus-tools"
   "sox"
   "twolame"
   "rubberband"
   "vorbis-tools"
   ;;"wavegain"
   ;;"dr14_tmeter"

   ;; text manipulation / processing
   "highlight"
   "pandoc"
   "dos2unix"
   "cowsay"
   "figlet"
   "recutils" ;; piping tools like recsel
   "bsd-games"

   ;; file processing
   "diffutils"
   "gzip"
   "findutils"
   "fzy"
   "jq"
   "tree"
   "mediainfo"
   "pbzip2" ;; multi core replacement for bzip2/bunzip2
   "pv"     ;; pipe progress monitor
   "ranger"
   "ripgrep"
   "gnupg"
   "ncurses"
   "pinentry"
   "pinentry-tty"
   "python"
   "python-yubikey-manager"
   "yubikey-personalization"
   "sshfs"
   "openssh"

   ;; console tools
   "bc" ;; calculator

   ;; spell checker
   "aspell"
   "aspell-dict-en"
   "aspell-dict-fr"


   ;; system monitor
   ;; rkhunter
   


   ))

;; not found:
;; - expressvpn: see aur
;; - iptables: system configuration
;; - dateutils: https://github.com/hroptatyr/dateutils/issues
;; "starship" ;; prompt
