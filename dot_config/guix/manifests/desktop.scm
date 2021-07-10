;; -*- mode: guix-scheme -*-

(use-modules
 (bj desktop))

(concatenate-manifests
 (list
;;  (packages->manifest
;;   `((,my-dmenu "out")
;;     (,my-st "out")
;;     (,my-dwm "out")
;;     (,my-dwmstatus "out")))
;;  ;;     (,my-qutebrowser "out")))
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

;;     ;; wm / xtools
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
     "font-openmoji"
     "libnotify"
     "rofi"
     "setxkbmap"
     "unclutter"
     "wmctrl"
     "xclip"
     "xdg-utils"
     "xdg-user-dirs"
     "xdotool"
     "xrdb"
     "xsel"
;;
;;    "flatpak"
;;     "scrot"
;;     ;; "slip" dmenu / scrot
;;     "mate-polkit"
;;     "mpv"
;;     "libmediainfo"
;;     "feh"
;;     ;; "goimapnotify"
;;     ;; gopls
;;     "flameshot"
;;     "mupdf"
;;     "poppler"
;;     "tesseract-ocr"
;;     ;;   "libreoffice"
;;     "zathura"
;;     "zathura-pdf-mupdf"
;;     "zathura-djvu"
;;     "sxiv"
;;     "gimp"
;;     "syncthing"
;;     "keepassxc"
;;     "notmuch"
;;     "isync"
;;     "msmtp"
;;
;;     ;; need a replacement. brings the whole gnome desktop: "cheese"
;;
;;     ;; editor
;;     "emacs"
;;     ;;     "emacs-bug-hunter"
;;     ;;     "emacs-modus-themes"
;;     ;;     "emacs-general"
;;     ;;     "emacs-evil"
;;     ;;     "emacs-evil-collection"
;;     ;;     "emacs-which-key"
;;     ;;     "emacs-use-package"
;;     ;;     "emacs-no-littering"
;;     ;;     "emacs-magit"
;;     ;;     "emacs-all-the-icons"
;;     ;;     "emacs-all-the-icons-dired"
;;     ;;     "emacs-doom-modeline"
;;     ;;     "emacs-ivy"
;;     ;;     "emacs-ivy-rich"
;;     ;; emacs-ivy-yasnipet, emacs-ivy-xref, emacs-ivy-posframe
;;     ;;     "emacs-counsel"
;;     ;;     "emacs-counsel-projectile"
;;     ;; emacs-counsel-projectile, emacs-counsel-notmuch, emacs-counsel-jq, emacs-counsel-dash
;;     ;;     "emacs-swiper"
;;     ;;     "emacs-helpful"
;;     ;;     "emacs-hydra"
;;     ;;     "emacs-rainbow-delimiters"
;;     ;;     "emacs-ace-window"
;;     ;;     "emacs-evil-visualstar"
;;     ;;     "emacs-deadgrep"
;;     ;;     "emacs-neotree"
;;     ;;     "emacs-smartparens"
;;     ;;     "emacs-editorconfig"
;;     ;;     "emacs-company"
;;     ;;     ;; "emacs-company-flx" ;; TODO missing in guix
;;     ;;     "emacs-company-quickhelp"
;;     ;;     "emacs-company-box"
;;     ;;     "emacs-pos-tip"
;;     ;;     "emacs-projectile"
;;     ;;     "emacs-vterm"
;;     ;;     "emacs-plantuml-mode"
;;     ;;     "emacs-adoc-mode"
;;     ;;     "emacs-dockerfile-mode"
;;     ;;     "emacs-haskell-mode"
;;     ;;     "emacs-cider"
;;     ;;     "emacs-org"
;;     ;;     "emacs-org-superstar"
;;     ;;     "emacs-org-roam"
;;     ;;     "emacs-elfeed"
;;     "sqlite" ;; needed by roam
;;     ;;     "emacs-org-rich-yank"
;;     ;;     "emacs-org-reveal"
;;     ;;     "emacs-org-re-reveal"
;;     ;;     "emacs-org-present"
;;     ;;     "emacs-org-generate"
;;     ;;     "emacs-org-download"
;;     ;;     "emacs-org-contrib"
;;     ;;     "emacs-org-pomodoro"
;;     ;; "emacs-org-beautify-theme" ;; TODO replace with custom faces
;;     ;;     "emacs-org-appear"
;;     "emacs-guix"
;;     "wordnet" ;; used by doom "lookup" module
;;
;;     "graphviz"
;;     "fd"
;;     ;;"plantuml"
;;     ;; devtools
;;     "openjdk@11"
;;     "openjdk@11:jdk"
;;     ;;   "clojure"
;;     ;;   "clojure-lsp-bin"
;;     "gcc-toolchain"
;;     "libvterm"
;;     "ghc"
;;     "cmake"
;;     "make"
;;     "strace"
;;     "git"
;;     "git:send-email"
;;     "stow"
;;     "vim"
     "password-store"
;;     "browserpass-native" ;; requires to register in firefox using: make -C $(guix build browserpass-native)/lib/browserpass hosts-firefox-user
     "go-github.com-howeyc-gopass"
;;     "python-tldextract"
;;     "python"
;;     "go"
;;     "guile"
;;     "rust:out"
;;     "rust:cargo"
;;     ;;   "leiningen"
;;     ;;"maven"
;;     ;;"openjdk@11."
;;     ;;"ruby"
;;     "tidy"
;;     ;; TODO enable docker
;;     ;;   "docker"
;;
;;     ;; why these tools
;;     ;;"perl-authen-sasl"
;;     ;;"perl-net-smtp-ssl"
;;     ;;"perl-mime-tools"
;;
;;     ;; browsers
;;     "qutebrowser"
;;     "wget"
;;     "w3m"
;;     "lynx"
;;     "surfraw"
;;     ;;"firefox"
;;
;;     ;; network tools
;;     "transmission" ;; out and gui
;;     "youtube-dl"
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
