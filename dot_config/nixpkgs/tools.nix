{ config, pkgs, ... }:
with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    tldextract
  ]; 
  python-with-my-packages = python3.withPackages my-python-packages;
in
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ]; 

  home.packages = with pkgs; [
    pass
    ranger
    tig
    vim
    wget

    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    aspellDicts.fr
    atool
    autoconf
    automake
    bash-completion
    bc
    bind
    binutils
    bluez
    bluez-tools
    bsdgames
    cargo
    chezmoi
    #clang
    clojure
    cmake
    cowsay
#     ;; TODO docker 1:20.10.7-1
#     ;; TODO docker-compose 1.29.2-1
    curl
    dos2unix
    fd
    feh
    gcc
    ghc
    fzf
    gitFull
    git-annex
    git-doc
    git-hub
    git-stree
    go
    goimapnotify
    gopls
    gnumake
    gnupg
    graphviz
    gimp
    keepassxc
    leiningen
    libmediainfo
    libreoffice
    lynx
    maim
    gnumake
    maven
    mpv
    mupdf
    nix-bash-completions
    python-with-my-packages
    recutils
    scrot
  ];

  programs.emacs = {
    enable = true;
    package = emacsGcc;
    extraPackages = (epkgs: [
      epkgs.vterm
      epkgs.org-roam
    ] );
  };
}
