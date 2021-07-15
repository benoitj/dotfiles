;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(define audio-manifest
  (specifications->manifest
   '("alsa-plugins:out" ;; required by qutebrowser to play audio
     "alsa-plugins:pulseaudio" ;; required by qutebrowser to play audio. missing LD_LIBRARY_PATH
     "alsa-utils"
     "pulsemixer"
     "pamixer"
     "gst-plugins-base"
     "gst-plugins-good"
     "gst-plugins-bad"
     "gst-plugins-ugly"
     "gstreamer"
     )))

(define x-wm-manifest
  (specifications->manifest
   '("arandr"
     "xrandr"
     "autorandr"
     "compton"
     "dex"
     "dragon-drop"
     "dunst"
     "fontconfig"
     "font-adobe-source-code-pro"
     "font-dejavu"
     "font-fira-code"
     "font-google-noto"
     "font-abattis-cantarell"
     "font-hack"
     "font-openmoji"
     "libnotify"
     "mate-polkit"
     "rofi"
     "setxkbmap"
     "unclutter"
     "wmctrl"
     "xclip"
     "xdg-utils"
     "xdg-user-dirs"
     "xdotool"
     "xrdb"
     "xsel")))

(define tools-manifest
  (specifications->manifest
   '(
     "clojure"
     "cmake"
     "fd"
     "feh"
     "gcc-toolchain"
     "ghc"
     "git"
     "go"
     "go-gitlab.com-shackra-goimapnotify"
     ;; gopls
     "graphviz"
     "guile"
     "flameshot"
     "gimp"
     "keepassxc"
     "leiningen"
     "libmediainfo"
     "libreoffice"
     "libvterm"
     "lynx"
     "maim"
     "make"
     "maven"
     "mcron"
     "mpv"
     "mupdf"
     "nss-certs"
     "openjdk@11"
     "openjdk@11:jdk"
     "plantuml"
     "poppler"
     "python-tldextract"
     "python"
     "ruby"
     "rust:out"
     "rust:cargo"
     "tesseract-ocr"
     "tidy"
     "sqlite"
     "strace"
     "surfraw"
     "sxiv"
     "syncthing"
     "vim"
     "w3m"
     "wget"
     "wordnet" ;; used by doom "lookup" module
     "zathura"
     "zathura-pdf-mupdf"
     "zathura-djvu")))

(define tools-extra-manifest
  (specifications->manifest
   '(
     "password-store"
     "browserpass-native" ;; requires to register in firefox using: make -C $(guix build browserpass-native)/lib/browserpass hosts-firefox-user
     "isync"
     "git:send-email"
     "msmtp"
     "notmuch"
     "transmission" ;; out and gui
     "youtube-dl"
     )))

(concatenate-manifests
 (list
  ;;  (packages->manifest
  ;;   `((,my-dmenu "out")
  ;;     (,my-st "out")
  ;;     (,my-dwm "out")
  ;;     (,my-dwmstatus "out")))
  ;;  ;;     (,my-qutebrowser "out")))
  audio-manifest
  x-wm-manifest
  tools-manifest
  tools-extra-manifest
  (specifications->manifest
   '(

     ;;
     ;;    "flatpak"
     ;;     ;; need a replacement. brings the whole gnome desktop: "cheese"
     ;;
     ;;     ;;   "clojure-lsp-bin"
     ;;     ;;   "docker"
     ;;
     ;;     ;; why these tools
     ;;     ;;"perl-authen-sasl"
     ;;     ;;"perl-net-smtp-ssl"
     ;;     ;;"perl-mime-tools"
     ;;
     ;;     ;; browsers
     ;;     "qutebrowser"
     ))))

;; others
;; 
;;     "firefox";; only on nonguix and means compilation required
;;   "xorg-server"
;;   "xf86-video-intel"
;;   "xorg-xinit"
;;   "xorg-xrandr"
;;   "xorg-xsetroot"
;;   "acpi"
;;   "acpid"
;;   "xf86-input-libinput"
;;   "xorg-xinput"


;; not found:
;; fira-code-nerd <- has bunch of glyph needed for ligatures
;; qt5-webengine-widevine ;; qute support for drm
;; qutebrowser 2 <- patch on it's way
;;"pdfjs" ;; used by qutebrowser to display pdf inline
;;   "hugo"
;;   "gopls" ;; go language server
;; "libxft-bgra" ;; fixes for color emoji in st, dwm, dmenu (crashes)
;; dbus-broker dbus for dwm
;;  "kdiff3"
;;  printer / scanner
