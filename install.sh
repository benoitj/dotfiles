#!/bin/sh


command -v stow &> /dev/null || sudo pacman -S --noconfirm stow


#stow -v1 --no-folding -t ~ stow
stow -v1 -t ~ stow

