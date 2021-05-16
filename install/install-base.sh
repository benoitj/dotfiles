#!/usr/bin/env sh

install_locate() {
	sudo pacman -S --needed --noconfirm mlocate

	sudo updatedb
}

sudo pacman -S --needed --noconfirm man texinfo mlocate base-devel pulseaudio pulseaudio-alsa


type locate &> /dev/null || install_locate 
