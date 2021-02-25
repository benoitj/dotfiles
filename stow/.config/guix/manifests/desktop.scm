;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(concatenate-manifests
 (list
  (packages->manifest
   `((,my-dmenu "out")
     (,my-st "out")
     (,my-dwm "out")
     (,my-dwmstatus "out")))
  (specifications->manifest
   '(
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
     
     ;; wm / xtools
     "arandr"
     "autorandr"
     "compton"
     "dunst"
     "fontconfig"
     "font-adobe-source-code-pro"
     "font-dejavu"
     "font-fira-code"
     "font-google-noto"
     "font-hack"
     "libnotify"
     "sxhkd"
     "unclutter"
     "wmctrl"
     "xclip"
     "xdotool"
     "xsel"
     "flatpak"
          
     "scrot"
     ;; "slip" dmenu / scrot
     "mate-polkit"
     "mpv"
     "libmediainfo"
     "feh"
     ;; "goimapnotify"
     ;; gopls
     "flameshot"
     "mupdf"
     "poppler"
     ;;   "libreoffice"
     "zathura"
     "zathura-pdf-mupdf"
     "zathura-djvu"
     "sxiv"
     "gimp"
     ;; need a replacement. brings the whole gnome desktop: "cheese"
     
     ;; editor
     "emacs"
     
     ;; devtools
     ;;   "clojure"
     ;;   "clojure-lsp-bin"
     "cmake"
     "git"
     "go"
     "go-github.com-howeyc-gopass"
     "guile"
     ;;   "leiningen"
     "maven"
     "openjdk@11."
     "ruby"
     "tidy"
     ;; TODO enable docker
     ;;   "docker"
     
     ;; why these tools
     ;;"perl-authen-sasl"
     ;;"perl-net-smtp-ssl"
     ;;"perl-mime-tools"
     
     ;; browsers
     "qutebrowser"
     "w3m"
     "lynx"
     "surfraw"
     ;;   "firefox"
     
     ;; network tools
     "transmission" ;; out and gui
     "youtube-dl"
     ))))

;; others
;; 
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
;; qt5-webengine-widevine ;; qute support for drm
;;"pdfjs" ;; used by qutebrowser to display pdf inline
;;   "hugo"
;;   "gopls" ;; go language server
;; "wmname" ;; dont think I use this now
;; "dragon-drag-and-drop" ;; drag/drop from terminal
;; "slip" ;; screenshot with dmenu
;; "libxft-bgra" ;; fixes for color emoji in st, dwm, dmenu (crashes)
;; dbus-broker dbus for dwm
;; mydwm
;; mydmenu
;; myst
;; mydwmstatus
;;  "kdiff3"
;;  printer / scanner
