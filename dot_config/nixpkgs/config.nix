{ pkgs }:
let
  inherit (pkgs) lib buildEnv;

in
{
  allowUnfree = true; # allow non-free, non-open source packages to install

  # if you are using Chromium, this is likely something you want to enable an option for
  chromium = {
    enableWideVine = true; # enable the DRM to allow things like Netflix to work
  };

  packageOverrides = pkgs: {
    coreApps = lib.lowPrio (buildEnv {
      name = "coreApps";
      paths = with pkgs; [
        aspell
        aspellDicts.en
        aspellDicts.fr
        atool
        autoconf
        automake
        bash-completion
        bc
        dnsutils
        # binutils # collision with gcc
        bluez
        # TODO replacement that contains bluetooth-player, advtest, etc.."bluez-tools"
        bsdgames
        cacert
        cargo
        # clang # collision with binutils
        clojure

        cmake
        cowsay
        dos2unix
        fd
        feh
        gcc
        ghc
        gnumake
        go
        goimapnotify
        graphviz
        guile
        gimp
        html-tidy
        keepassxc
        leiningen
        libreoffice
        lynx
        maim
        maven
        # mcron
        mediainfo
        mpv
        mupdf
        openjdk11
        pass
        plantuml
        poppler
        python39Packages.tldextract
        python39
        ruby
        rustc
        tesseract
        sqlite
        strace
        surfraw
        sxiv
        syncthing
        vim
        w3m
        wget
        wordnet
        zathura
      ];
    });

    xCoreApps = lib.lowPrio (buildEnv {
      name = "xCoreApps";
      paths = with pkgs; [
        #   "xorg-server"
        #   "xf86-video-intel"
        #   "xorg-xinit"
        #   "xorg-xrandr"
        #   "xorg-xsetroot"
        #   "acpi"
        #   "acpid"
        #   "xf86-input-libinput"
        #   "xorg-xinput"

      ];
    });

    xApps = lib.lowPrio (buildEnv {
      name = "xApps";
      paths = with pkgs; [
        arandr
        xorg.xrandr
        autorandr
        cantarell-fonts
        compton
        dejavu_fonts
        dex
        dragon-drop
        dunst
        firefox
        fontconfig
        libnotify
        mate.mate-polkit
        nerdfonts
        #qutebrowser
        rofi
        unclutter
        #vivaldi-widevine
        wmctrl
        xclip
        xdg_utils
        xdg-user-dirs
        xdotool
        xorg.setxkbmap
        xorg.xrdb
        xsel

        noto-fonts-emoji
        source-code-pro
      ];
    });

    extraApps = lib.lowPrio (buildEnv {
      name = "extraApps";
      paths = with pkgs; [
        ncspot
        mpd
        ncmpcpp
        calibre
        mesa_glu
        mesa
        patchelf
        qt5
      ];
    });
  };
}
