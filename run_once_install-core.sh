#!/bin/bash

PACMAN="sudo pacman -S --noconfirm --needed"
YAY="yay -S --needed"



test -d ~/src || mkdir -p ~/src

test -d ~/src/dmenu || git clone https://github.com/benoitj/dmenu ~/src/dmenu
which dmenu || (cd ~/src/dmenu; make; sudo make install)
test -d ~/src/st || git clone https://github.com/benoitj/st ~/src/st
which st || (cd ~/src/st; make; sudo make install)
test -d ~/src/dmenu || git clone https://github.com/benoitj/dwm ~/src/dwm
which dwm || (cd ~/src/dwm; make; sudo make install)

$PACMAN dunst sxhkd
$PACMAN vim
$PACMAN pulsemixer pamixer mpc
$PACMAN surfraw
$PACMAN scrot
$PACMAN emacs
$PACMAN xorg-xrandr arandr

$YAY --noconfirm starship
test -d ~/.sdkman || curl -s "https://get.sdkman.io" | bash
