#!/bin/bash

export LANG=en_US.UTF-8

PROFILE_D=~/.config/bash/bash_profile.d 
test -d $PROFILE_D || return 
test -f $PROFILE_D/*.sh ||  return
test -f ~/.config/bash/env && source ~/.config/bash/env

for profile_file in $PROFILE_D/*.sh
do
	source $profile_file
done

# these are fixes for discord. somehow it's missing libdrm, opt/Discord and something in mesa
# make sure you run nix-env -i mesa libdrm

export EDITOR="vim"
export BROWSER="qutebrowser"

# spaceship prompt configuration
export SPACESHIP_DIR_TRUNC_REPO=false
export SPACESHIP_GIT_BRANCH_COLOR=blue
export SPACESHIP_DIR_COLOR=green

[[ -d ~/.local/bin ]] && PATH="$HOME/.local/bin:$PATH"
[[ -d ~/.local/scripts/cli ]] && PATH="$HOME/.local/scripts/cli:$PATH"
[[ -d ~/.local/scripts/rofi ]] && PATH="$HOME/.local/scripts/rofi:$PATH"

export PATH
export TERMINAL=st
export _JAVA_AWT_WM_NONREPARENTING=1
