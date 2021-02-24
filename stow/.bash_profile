#!/bin/bash

PROFILE_D=~/.config/bash/bash_profile.d 
test -d $PROFILE_D || return 
test -f $PROFILE_D/*.sh ||  return
test -f ~/.config/bash/env && source ~/.config/bash/env

for profile_file in $PROFILE_D/*.sh
do
	source $profile_file
done


unset GUIX_PROFILE

GUIX_EXTRA_PROFILES="$HOME/.guix-extra-profiles"
export GUIX_EXTRA_PROFILES
if test -d $GUIX_EXTRA_PROFILES
then
	for i in $GUIX_EXTRA_PROFILES/*
	do
		profile=$i/$(basename "$i")
	    	if [ -f "$profile"/etc/profile ]
		then
			GUIX_PROFILE="$profile"
			. "$GUIX_PROFILE"/etc/profile
		fi
		unset profile
	done
fi
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
export GUIX_PROFILE="/home/benoit/.guix-profile"
source "$GUIX_PROFILE/etc/profile"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:.guix-extra-profiles/desktop/desktop/lib/alsa-lib/"

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
### ssh key agent
SSH_ENV="$HOME/.ssh/env"

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  source "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add -t 216000;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
  source "${SSH_ENV}" > /dev/null
  #ps ${SSH_AGENT_PID} doesn't work under cywgin
  #ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
  #  start_agent;
  #}
else
  start_agent;
fi

export TERMINAL=st

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

[[ "$(tty)" = "/dev/tty1" ]] && exec startx
