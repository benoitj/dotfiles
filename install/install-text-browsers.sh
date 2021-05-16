#!/usr/bin/env sh

for pkg in w3m surfraw lynx
do
	type $pkg &> /dev/null || sudo pacman -S --needed --noconfirm $pkg
done

