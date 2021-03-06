;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(concatenate-manifests
 (list
  (packages->manifest
   `((,my-dmenu "out")
     (,my-st "out")
     (,my-dwm "out")
     (,my-dwmstatus "out")
     (,my-qutebrowser "out")))
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
     "xrandr"
     "autorandr"
     "compton"
     "dunst"
     "fontconfig"
     "font-adobe-source-code-pro"
     "font-dejavu"
     "font-fira-code"
     "font-google-noto"
     "font-abattis-cantarell"
     "font-hack"
     "libnotify"
     "sxhkd"
     "unclutter"
     "wmctrl"
     "xclip"
     "xdotool"
     "xsel"
     "flatpak"
     "rofi"
     "xrdb"
     "setxkbmap"
     "xdg-utils"
     "xdg-user-dirs"
          
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
     "syncthing"
     "keepassxc"

     ;; need a replacement. brings the whole gnome desktop: "cheese"
     
     ;; editor
     "emacs"
     "emacs-modus-themes"
     "emacs-general"
     "emacs-evil"
     "emacs-evil-collection"
     "emacs-which-key"
     "emacs-use-package"
     "emacs-no-littering"
     "emacs-magit"
     "emacs-all-the-icons"
     "emacs-all-the-icons-dired"
     "emacs-doom-modeline"
     "emacs-ivy"
     "emacs-ivy-rich"
     ;; emacs-ivy-yasnipet, emacs-ivy-xref, emacs-ivy-posframe
     "emacs-counsel"
     ;; emacs-counsel-projectile, emacs-counsel-notmuch, emacs-counsel-jq, emacs-counsel-dash
     "emacs-swiper"
     "emacs-helpful"
     "emacs-hydra"
     
     ;; devtools
     ;;   "clojure"
     ;;   "clojure-lsp-bin"
     "cmake"
     "make"
     "strace"
     "git"
     "stow"
     "vim"
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
     ;; "qutebrowser" ;; replaced by my-qutebrowser
     "wget"
     "w3m"
     "lynx"
     "surfraw"
     ;;"firefox"
     
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
;; fira-code-nerd <- has bunch of glyph needed for ligatures
;; starship <- currently installed in .local/bin
;; qt5-webengine-widevine ;; qute support for drm
;; qutebrowser 2 <- patch on it's way
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
