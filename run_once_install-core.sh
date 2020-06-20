#!/bin/bash

PACMAN="sudo pacman -S --noconfirm --needed"
YAY="yay -S --needed"



test -d ~/src || mkdir -p ~/src

test -d ~/src/dmenu || git clone https://github.com/benoitj/dmenu ~/src/dmenu
which dmenu || (cd ~/src/dmenu; make; sudo make install)
test -d ~/src/st || git clone https://github.com/benoitj/st ~/src/st
which st || (cd ~/src/st; make; sudo make install)
test -d ~/src/dwm || git clone https://github.com/benoitj/dwm ~/src/dwm
which dwm || (cd ~/src/dwm; make; sudo make install)
test -d ~/src/dwmstatus || git clone https://github.com/benoitj/dwmstatus ~/src/dwmstatus
which dwmstatus || (cd ~/src/dwmstatus; make; sudo make install)

$PACMAN dunst sxhkd compton mate-polkit
$PACMAN vim
$PACMAN pulsemixer pamixer mpc
$PACMAN surfraw
$PACMAN scrot
$PACMAN emacs
$PACMAN xorg-xrandr arandr

$YAY --noconfirm starship
test -d ~/.sdkman || curl -s "https://get.sdkman.io" | bash
